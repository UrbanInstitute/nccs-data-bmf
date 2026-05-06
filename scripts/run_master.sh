#!/usr/bin/env bash
# ============================================================================
# run_master.sh
#
# Build the Master BMF: one row per EIN drawn from the most-recent vintage
# across both the current monthly BMF pipeline and the legacy
# 501CX-NONPROFIT-PX pipeline.
#
# Designed for an EC2 instance with the existing setup_ec2.sh bootstrap
# applied. Runs in a fresh Rscript subprocess so memory and connections
# are fully released on exit.
#
# Usage:
#   bash scripts/run_master.sh
#
# Logs: logs/master/run.log
# ============================================================================
set -u -o pipefail

cd "$(dirname "$0")/.."

mkdir -p logs/master
LOG="logs/master/run.log"

echo "==== $(date -Iseconds) Master BMF build ===="
Rscript --vanilla -e "source('R/run_master_pipeline.R')" 2>&1 | tee "$LOG"
rc=${PIPESTATUS[0]}

if [[ $rc -eq 0 ]]; then
  echo "Master BMF build OK. Log: $LOG"
else
  echo "Master BMF build FAILED rc=$rc. Log: $LOG" >&2
  exit "$rc"
fi
