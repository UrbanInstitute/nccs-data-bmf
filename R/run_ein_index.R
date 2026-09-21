# ============================================================================
# run_ein_index.R
#
# Standalone runner for the EIN index (nccs-contracts ADR 0050). Builds the
# per-prefix JSON shards from the geocoded Unified BMF and publishes them to
# s3://nccsdata/unified/bmf/ein-index/latest/.
#
# Usage (in R):
#   source("R/run_ein_index.R")
#
# Optional flags before sourcing:
#   ENABLE_S3_UPLOAD    <- FALSE   # build only
#   EIN_INDEX_DRY_RUN   <- TRUE    # print the upload plan, upload nothing
# ============================================================================

if (!exists("ENABLE_S3_UPLOAD"))  ENABLE_S3_UPLOAD  <- TRUE
if (!exists("EIN_INDEX_DRY_RUN")) EIN_INDEX_DRY_RUN <- FALSE

GEOCODED_MASTER_PATH <- here::here("data", "geocoding", "master", "merged",
                                   "bmf_unified_geocoded.parquet")

source(here::here("R", "config.R"))
source(here::here("R", "utils", "logging.R"))
source(here::here("R", "manifest.R"))
source(here::here("R", "build_ein_index.R"))

log_phase_start("EIN INDEX")
ein_index_build <- build_ein_index(geocoded_path = GEOCODED_MASTER_PATH)

if (ENABLE_S3_UPLOAD) {
  publish_ein_index(output_dir = ein_index_build$output_dir, dry_run = EIN_INDEX_DRY_RUN)
}
