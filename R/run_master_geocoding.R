# ============================================================================
# run_master_geocoding.R
#
# Orchestrator for the Master BMF geocoding workflow. Two modes:
#
#   MASTER_GEOCODING_MODE <- "export"  ; source("R/run_master_geocoding.R")
#   # ... submit batches to the automated geocoder service, retrieve outputs ...
#   MASTER_GEOCODING_MODE <- "merge"   ; source("R/run_master_geocoding.R")
#
# Mirrors the per-month run_geocoding.R interface. The middle step needs no
# manual activation: the geocoder is an S3-event-driven service (drop batch
# CSVs in s3://geocoding-codestar-prod/data/input-data/, poll
# data/output-data/ for results). See docs/reference/geocoder-service.md.
# ============================================================================

if (!exists("MASTER_GEOCODING_MODE")) MASTER_GEOCODING_MODE <- "export"

ENABLE_S3_UPLOAD     <- TRUE
UNIFIED_PARQUET_PATH <- here::here("data", "master", "bmf_unified.parquet")
MASTER_GEOCODING_DIR <- here::here("data", "geocoding", "master")

source(here::here("R", "config.R"))
source(here::here("R", "utils", "logging.R"))
source(here::here("R", "quality", "post_checks.R"))         # generate_data_dictionary()
source(here::here("R", "manifest.R"))                        # write_manifest() (ADR 0014)
source(here::here("R", "master_geocoding.R"))
source(here::here("R", "quality", "geocoding_checks.R"))

if (MASTER_GEOCODING_MODE == "export") {
  log_phase_start("MASTER GEOCODING -- EXPORT")
  prepare_master_geocoder_batches(
    master_path   = UNIFIED_PARQUET_PATH,
    geocoding_dir = MASTER_GEOCODING_DIR,
    batch_size    = GEOCODER_BATCH_SIZE,
    s3_upload     = ENABLE_S3_UPLOAD
  )

} else if (MASTER_GEOCODING_MODE == "merge") {
  log_phase_start("MASTER GEOCODING -- MERGE")
  merge_master_geocoded_results(
    master_path   = UNIFIED_PARQUET_PATH,
    geocoding_dir = MASTER_GEOCODING_DIR,
    s3_upload     = ENABLE_S3_UPLOAD
  )

} else {
  stop(sprintf("Unknown MASTER_GEOCODING_MODE: '%s'. Use 'export' or 'merge'.",
               MASTER_GEOCODING_MODE))
}
