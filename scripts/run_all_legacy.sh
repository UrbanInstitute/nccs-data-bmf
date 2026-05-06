#!/usr/bin/env bash
# ============================================================================
# run_all_legacy.sh
#
# Run the legacy BMF harmonization pipeline serially over every vintage
# present in s3://nccsdata/legacy/bmf/. Each vintage runs in a fresh Rscript
# subprocess so memory and file connections are released between runs.
#
# Usage:
#   bash scripts/run_all_legacy.sh                # all vintages, oldest first
#   bash scripts/run_all_legacy.sh --newest-first # newest vintages first
#   SKIP_EXISTING=1 bash scripts/run_all_legacy.sh
#       # skip a vintage if its processed CSV already exists in S3
#
# Logs:   logs/legacy/bmf_legacy_<YYYY>_<MM>.log  (one per vintage)
# Status: logs/legacy/run_summary.tsv             (vintage, status, seconds)
# ============================================================================
set -u -o pipefail

cd "$(dirname "$0")/.."

ORDER="oldest-first"
if [[ "${1:-}" == "--newest-first" ]]; then ORDER="newest-first"; fi

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

echo "Found ${#VINTAGES[@]} vintages. Order: $ORDER"
echo

for ym in "${VINTAGES[@]}"; do
  year="${ym%-*}"
  month="${ym#*-}"
  tag="${year}_${month}"
  log="logs/legacy/bmf_legacy_${tag}.log"
  started=$(date -Iseconds)
  t0=$(date +%s)

  if [[ "${SKIP_EXISTING:-0}" == "1" ]]; then
    if aws s3 ls "s3://nccsdata/processed/bmf-legacy/${tag}/bmf_legacy_${tag}_processed.csv" \
         >/dev/null 2>&1; then
      printf "[%s] SKIP %s (already in S3)\n" "$started" "$ym"
      printf "%s\tskipped\t0\t%s\n" "$ym" "$started" >> "$SUMMARY"
      continue
    fi
  fi

  printf "==== [%s] Legacy %s ====\n" "$started" "$ym"

  Rscript --vanilla -e "
    LEGACY_BMF_YEAR  <- ${year}
    LEGACY_BMF_MONTH <- ${month}
    source('R/run_legacy_pipeline.R')
  " > "$log" 2>&1
  rc=$?

  elapsed=$(( $(date +%s) - t0 ))
  if [[ $rc -eq 0 ]]; then
    status="ok"
    printf "     -> ok (%ds), log: %s\n" "$elapsed" "$log"
  else
    status="failed_rc${rc}"
    printf "     -> FAILED rc=%d (%ds), log: %s\n" "$rc" "$elapsed" "$log" >&2
  fi
  printf "%s\t%s\t%d\t%s\n" "$ym" "$status" "$elapsed" "$started" >> "$SUMMARY"
done

echo
echo "Done. Summary: $SUMMARY"
column -t -s $'\t' "$SUMMARY" | tail -n +1
