# ============================================================================
# run_unified_geocoding.R
#
# Orchestrator for the Unified BMF geocoding workflow (called the Master BMF
# before ADR 0037). Modes:
#
#   UNIFIED_GEOCODING_MODE <- "export"  ; source("R/run_unified_geocoding.R")
#   # ... submit batches to the automated geocoder service, retrieve outputs ...
#   UNIFIED_GEOCODING_MODE <- "merge"   ; source("R/run_unified_geocoding.R")
#
# Mirrors the per-month run_geocoding.R interface. The middle step needs no
# manual activation: the geocoder is an S3-event-driven service (drop batch
# CSVs in s3://geocoding-codestar-prod/data/input-data/, poll
# data/output-data/ for results). See docs/reference/geocoder-service.md.
# ============================================================================

# The mode flag was called MASTER_GEOCODING_MODE before the ADR 0037 rename
# reached this file. The old name is still honoured, with a warning, so an
# older runbook or shell history does not silently fall through to "export".
if (!exists("UNIFIED_GEOCODING_MODE") && exists("MASTER_GEOCODING_MODE")) {

  warning("MASTER_GEOCODING_MODE is now UNIFIED_GEOCODING_MODE; using the value given under the old name.")
  UNIFIED_GEOCODING_MODE <- MASTER_GEOCODING_MODE

}

if (!exists("UNIFIED_GEOCODING_MODE")) UNIFIED_GEOCODING_MODE <- "export"

if (!exists("ENABLE_S3_UPLOAD")) ENABLE_S3_UPLOAD <- TRUE  # default TRUE unless the caller set it first, so a verification build can run with uploads off without editing this file
UNIFIED_PARQUET_PATH <- here::here("data", "master", "bmf_unified.parquet")
UNIFIED_GEOCODING_DIR <- here::here("data", "geocoding", "unified")

# The working folder was data/geocoding/master/ before the rename. A machine
# that still has the old folder and not the new one would otherwise start
# from an empty folder and lose sight of its staged batches and ledger.
# Moving the folder is the whole changeover: the files inside keep their old
# bmf_master_geocoder_* names, and the retrieve and merge steps accept those
# (R/geocoder_working_files.R). Do NOT rename the files inside by hand; the
# run's ledger and manifest refer to them by the names they have.
LEGACY_GEOCODING_DIR <- here::here("data", "geocoding", "master")

if (dir.exists(LEGACY_GEOCODING_DIR) && !dir.exists(UNIFIED_GEOCODING_DIR)) {

  stop(sprintf(
    "The geocoding working folder was renamed. Move the existing folder first (leave the files inside as they are):\n  mv %s %s",
    LEGACY_GEOCODING_DIR,
    UNIFIED_GEOCODING_DIR
  ))

}

source(here::here("R", "config.R"))
source(here::here("R", "utils", "logging.R"))
source(here::here("R", "quality", "post_checks.R"))         # generate_data_dictionary()
source(here::here("R", "manifest.R"))                        # write_manifest() (ADR 0014)
source(here::here("R", "unified_geocoding.R"))
source(here::here("R", "quality", "geocoding_checks.R"))

if (UNIFIED_GEOCODING_MODE == "delta") {
  # Delta export (Z8): carry forward results for every address already in
  # the published geocoded artifact; submit only new addresses to the
  # service. Set DELTA_SUBMIT <- TRUE to actually submit (default stages
  # locally and prints the plan).
  source(here::here("R", "unified_geocoding_delta.R"))
  log_phase_start("UNIFIED BMF GEOCODING -- DELTA EXPORT")
  prepare_unified_geocoder_delta(
    unified_path   = UNIFIED_PARQUET_PATH,
    geocoding_dir = UNIFIED_GEOCODING_DIR,
    submit        = exists("DELTA_SUBMIT") && isTRUE(DELTA_SUBMIT)
  )

} else if (UNIFIED_GEOCODING_MODE == "retrieve") {
  source(here::here("R", "unified_geocoding_delta.R"))
  log_phase_start("UNIFIED BMF GEOCODING -- DELTA RETRIEVE")
  retrieve_unified_geocoder_delta(
    geocoding_dir = UNIFIED_GEOCODING_DIR,
    wait          = !exists("RETRIEVE_NO_WAIT") || !isTRUE(RETRIEVE_NO_WAIT)
  )

} else if (UNIFIED_GEOCODING_MODE == "export") {
  log_phase_start("UNIFIED BMF GEOCODING -- EXPORT")
  prepare_unified_geocoder_batches(
    unified_path   = UNIFIED_PARQUET_PATH,
    geocoding_dir = UNIFIED_GEOCODING_DIR,
    batch_size    = GEOCODER_BATCH_SIZE,
    s3_upload     = ENABLE_S3_UPLOAD
  )

} else if (UNIFIED_GEOCODING_MODE == "merge") {
  log_phase_start("UNIFIED BMF GEOCODING -- MERGE")
  merge_unified_geocoded_results(
    unified_path   = UNIFIED_PARQUET_PATH,
    geocoding_dir = UNIFIED_GEOCODING_DIR,
    s3_upload     = ENABLE_S3_UPLOAD
  )

  # The EIN index (ADR 0050) is cut from the geocoded file just published,
  # so it is rebuilt here, after every merge. Set BUILD_EIN_INDEX <- FALSE
  # before sourcing to skip it.
  if (!exists("BUILD_EIN_INDEX") || isTRUE(BUILD_EIN_INDEX)) {
    source(here::here("R", "run_ein_index.R"))
  }

} else {
  stop(sprintf("Unknown UNIFIED_GEOCODING_MODE: '%s'. Use 'delta', 'retrieve', 'export' or 'merge'.",
               UNIFIED_GEOCODING_MODE))
}
