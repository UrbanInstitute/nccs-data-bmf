#!/usr/bin/env bash
# ============================================================================
# run_all_legacy.sh
#
# Run the legacy BMF harmonization pipeline over every vintage present in
# s3://nccsdata/legacy/bmf/. Each vintage runs in a fresh Rscript
# subprocess so memory and file connections are released between runs.
#
# Defaults to JOBS=1 (serial) for safety on small-RAM hosts. On a beefy
# instance like c5.18xlarge, set JOBS=8 (or higher) to run multiple
# vintages concurrently — each subprocess peaks at ~6-8 GB RAM, so size
# JOBS to keep total RAM under ~70 % of the host.
#
# Usage:
#   bash scripts/run_all_legacy.sh                  # serial, oldest first
#   bash scripts/run_all_legacy.sh --newest-first   # serial, newest first
#   JOBS=8 bash scripts/run_all_legacy.sh           # 8 concurrent vintages
#   SKIP_EXISTING=1 JOBS=8 bash scripts/run_all_legacy.sh
#       # skip vintages whose processed CSV is already in S3
#   SKIP_VINTAGES="2017-09,2017-12,2018-12" bash scripts/run_all_legacy.sh
#       # skip specific vintages by YYYY-MM (comma-separated)
#
# Known bad vintages (skip by default — set SKIP_VINTAGES="" to override):
#   2017-09, 2017-12, 2018-12 — these three NCCS-published files use
#   sequence-ID values in the EIN column (e.g. "000000001") instead of
#   real 9-digit IRS EINs, and have a non-standard TAXPER encoding.
#   They are structurally incompatible with the harmonization pipeline
#   and should not be processed until the upstream NCCS files are fixed
#   or a separate pipeline is built for that schema variant.
#
# Recommended JOBS settings:
#   16 GB RAM laptop      -> JOBS=1   (the default; do not parallelize)
#   m6i.2xlarge (32 GB)   -> JOBS=3
#   m6i.4xlarge (64 GB)   -> JOBS=6
#   c5.18xlarge (144 GB)  -> JOBS=8 to JOBS=12
#
# Logs:   logs/legacy/bmf_legacy_<YYYY>_<MM>.log  (one per vintage)
# Status: logs/legacy/run_summary.tsv             (vintage, status, seconds)
# ============================================================================
set -u -o pipefail

cd "$(dirname "$0")/.."

JOBS="${JOBS:-1}"
# Known-bad vintages with sequence-ID EINs and non-standard TAXPER. Override
# with SKIP_VINTAGES="" if upstream NCCS files have been fixed.
SKIP_VINTAGES="${SKIP_VINTAGES-2017-09,2017-12,2018-12}"
ORDER="oldest-first"
if [[ "${1:-}" == "--newest-first" ]]; then ORDER="newest-first"; fi

# SKIP_EXISTING requires the standalone aws CLI for s3api head-object.
# Without it the check would silently rc=127 ("command not found") and
# every vintage would re-run unnecessarily.
if [[ "${SKIP_EXISTING:-0}" == "1" ]] && ! command -v aws >/dev/null 2>&1; then
  echo "ERROR: SKIP_EXISTING=1 requires the AWS CLI but 'aws' is not on PATH." >&2
  echo "       Install with: bash scripts/setup_ec2.sh   (idempotent)"  >&2
  echo "       Or skip the existence check by unsetting SKIP_EXISTING." >&2
  exit 2
fi

mkdir -p logs/legacy
SUMMARY="logs/legacy/run_summary.tsv"
[[ -f "$SUMMARY" ]] || printf "vintage\tstatus\tseconds\tstarted_at\n" > "$SUMMARY"

echo "Listing legacy BMF vintages in S3..."
mapfile -t VINTAGES < <(Rscript --vanilla -e '
  suppressMessages(source("R/config.R"))
  ym <- list_available_legacy_bmf_files()
  cat(ym, sep = "\n")
')

if [[ ${#VINTAGES[@]} -eq 0 ]]; then
  echo "No legacy vintages found in S3. Aborting." >&2
  exit 1
fi

# list_available_legacy_bmf_files() returns descending. Reverse if oldest-first.
if [[ "$ORDER" == "oldest-first" ]]; then
  mapfile -t VINTAGES < <(printf '%s\n' "${VINTAGES[@]}" | tac)
fi

# Apply SKIP_VINTAGES filter (comma-separated YYYY-MM list).
if [[ -n "$SKIP_VINTAGES" ]]; then
  IFS=',' read -ra SKIP_LIST <<< "$SKIP_VINTAGES"
  declare -A SKIP_SET=()
  for s in "${SKIP_LIST[@]}"; do
    s_trim="${s// /}"
    [[ -n "$s_trim" ]] && SKIP_SET["$s_trim"]=1
  done
  KEPT=()
  SKIPPED_COUNT=0
  for ym in "${VINTAGES[@]}"; do
    if [[ -n "${SKIP_SET[$ym]:-}" ]]; then
      SKIPPED_COUNT=$((SKIPPED_COUNT + 1))
    else
      KEPT+=("$ym")
    fi
  done
  VINTAGES=("${KEPT[@]}")
  echo "SKIP_VINTAGES filter applied: skipped $SKIPPED_COUNT vintage(s) [$SKIP_VINTAGES]"
fi

echo "Found ${#VINTAGES[@]} vintages. Order: $ORDER. JOBS=$JOBS."
echo

# ----------------------------------------------------------------------------
# Per-vintage worker. Used by both the serial loop and the parallel xargs
# path. Captures result row in run_summary.tsv on its own.
# ----------------------------------------------------------------------------
process_vintage() {
  local ym="$1"
  local year="${ym%-*}"
  local month="${ym#*-}"
  local tag="${year}_${month}"
  local log="logs/legacy/bmf_legacy_${tag}.log"
  local started
  started=$(date -Iseconds)
  local t0
  t0=$(date +%s)

  if [[ "${SKIP_EXISTING:-0}" == "1" ]]; then
    # Use s3api head-object (returns rc=0 only on exact match) rather
    # than `aws s3 ls`, which in CLI v2 returns rc=0 even when the
    # specific key does not exist (it does a prefix listing internally).
    if aws s3api head-object \
         --bucket nccsdata \
         --key "processed/bmf-legacy/${tag}/bmf_legacy_${tag}_processed.csv" \
         >/dev/null 2>&1; then
      printf "[%s] SKIP %s (already in S3)\n" "$started" "$ym"
      printf "%s\tskipped\t0\t%s\n" "$ym" "$started" >> "$SUMMARY"
      return 0
    fi
  fi

  printf "==== [%s] Legacy %s ====\n" "$started" "$ym"

  Rscript --vanilla -e "
    LEGACY_BMF_YEAR  <- ${year}
    LEGACY_BMF_MONTH <- ${month}
    source('R/run_legacy_pipeline.R')
  " > "$log" 2>&1
  local rc=$?

  local elapsed=$(( $(date +%s) - t0 ))
  if [[ $rc -eq 0 ]]; then
    printf "     -> ok %s (%ds), log: %s\n" "$ym" "$elapsed" "$log"
    printf "%s\tok\t%d\t%s\n" "$ym" "$elapsed" "$started" >> "$SUMMARY"
  else
    printf "     -> FAILED %s rc=%d (%ds), log: %s\n" "$ym" "$rc" "$elapsed" "$log" >&2
    printf "%s\tfailed_rc%d\t%d\t%s\n" "$ym" "$rc" "$elapsed" "$started" >> "$SUMMARY"
  fi
  return $rc
}

export -f process_vintage
export SUMMARY

# ----------------------------------------------------------------------------
# Dispatch: serial loop if JOBS=1, otherwise parallel via xargs -P.
# xargs preserves exit non-zero if any subprocess fails.
# ----------------------------------------------------------------------------
if [[ "$JOBS" -le 1 ]]; then
  for ym in "${VINTAGES[@]}"; do
    process_vintage "$ym" || true
  done
else
  printf '%s\n' "${VINTAGES[@]}" \
    | xargs -n 1 -P "$JOBS" -I {} bash -c 'process_vintage "$@"' _ {}
fi

echo
echo "Done. Summary: $SUMMARY"
column -t -s $'\t' "$SUMMARY" | tail -n +1
