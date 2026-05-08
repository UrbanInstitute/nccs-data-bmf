# ============================================================================
# run_master_state_marts.R
#
# Orchestrator for building per-state data marts from the geocoded
# Master BMF. Splits the 3.1 GB unified file into smaller parquet
# (Hive-partitioned) + per-state CSV outputs that end users can pull
# selectively.
#
#   source("R/run_master_state_marts.R")
# ============================================================================

ENABLE_S3_UPLOAD <- TRUE

GEOCODED_MASTER_PATH <- here::here("data", "geocoding", "master", "merged",
                                   "bmf_master_geocoded.parquet")
STATE_MARTS_DIR      <- here::here("data", "master", "state_marts")

source(here::here("R", "config.R"))
source(here::here("R", "utils", "logging.R"))
source(here::here("R", "master_state_marts.R"))

log_phase_start("MASTER STATE MARTS")
build_master_state_marts(
  geocoded_path = GEOCODED_MASTER_PATH,
  output_dir    = STATE_MARTS_DIR,
  s3_upload     = ENABLE_S3_UPLOAD
)
