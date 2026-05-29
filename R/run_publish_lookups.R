# ============================================================================
# run_publish_lookups.R
#
# Standalone runner for the BMF lookup publication step. Use this when you
# want to refresh s3://nccsdata/lookups/bmf/ without re-running the full
# master pipeline (e.g. after an edit to data/lookup/bmf_code_lookup.xlsx).
#
# Usage (in R):
#   source("R/run_publish_lookups.R")
#
# Optional flags before sourcing:
#   PUBLISH_DRY_RUN  <- TRUE   # write CSVs locally, print plan, don't upload
#   PUBLISH_VINTAGE  <- "2026_05"  # override the YYYY_MM stamp
# ============================================================================

if (!exists("PUBLISH_DRY_RUN")) PUBLISH_DRY_RUN <- FALSE
if (!exists("PUBLISH_VINTAGE")) PUBLISH_VINTAGE <- format(Sys.Date(), "%Y_%m")

source(here::here("R", "config.R"))
source(here::here("R", "utils", "logging.R"))
source(here::here("R", "manifest.R"))
source(here::here("R", "publish_lookups.R"))

log_phase_start("PUBLISH LOOKUPS")
publish_bmf_lookups(vintage = PUBLISH_VINTAGE, dry_run = PUBLISH_DRY_RUN)
