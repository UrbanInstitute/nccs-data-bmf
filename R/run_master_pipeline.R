# ============================================================================
# run_master_pipeline.R
#
# Build the Master BMF: one row per EIN, drawn from the most-recent vintage
# in which that EIN appears across both the current monthly BMF pipeline
# and the legacy 501CX-NONPROFIT-PX pipeline.
#
# Outputs:
#   data/master/bmf_master.parquet          (full schema, zstd-compressed)
#   data/master/bmf_master.csv              (same rows, CSV form)
#   data/master/bmf_master_data_dictionary.csv
#   data/quality/bmf_master_quality_report.json
#
# S3 (if ENABLE_S3_UPLOAD):
#   s3://nccsdata/master/bmf/bmf_master.parquet
#   s3://nccsdata/master/bmf/bmf_master.csv
#   s3://nccsdata/master/bmf/bmf_master_data_dictionary.csv
#   s3://nccsdata/master/bmf/bmf_master_quality_report.json
#
# Usage (in R):
#   source("R/run_master_pipeline.R")
# ============================================================================

# ============================================================================
# Pipeline Configuration
#
# Defaults are tuned for AWS c5.18xlarge (72 vCPU / 144 GB RAM). On any
# smaller host, override DUCKDB_MEMORY_LIMIT before sourcing this file.
# Suggested starting points (target ~70% of system RAM):
#   c5.18xlarge  (144 GB)  -> "100GB"   (default below)
#   m6i.4xlarge  ( 64 GB)  ->  "45GB"
#   m6i.2xlarge  ( 32 GB)  ->  "22GB"
#   m6i.xlarge   ( 16 GB)  ->  "10GB"
#   laptop       (  8 GB)  ->   "5GB"
# Undersizing makes DuckDB spill to disk (slower) but will not OOM.
# See docs/11-master-bmf.qmd for the full sizing guide.
# ============================================================================

ENABLE_S3_UPLOAD     <- TRUE
DUCKDB_MEMORY_LIMIT  <- "100GB"   # c5.18xlarge has 144 GB; leave headroom
DUCKDB_THREADS       <- NULL      # NULL = use all cores
DUCKDB_DB_PATH       <- NULL      # NULL = in-memory; set a path for debugging
# Spill directory for DuckDB. The stacked CSVs (~150M VARCHAR rows)
# can exceed the in-memory limit; DuckDB offloads unused blocks here.
# Needs an EBS volume with ~50-100 GB free.
DUCKDB_TEMP_DIR      <- "data/master/duckdb-tmp"

MASTER_OUTPUT_DIR    <- "data/master"
MASTER_STAGING_DIR   <- "data/master/staging"
MASTER_QUALITY_DIR   <- "data/quality"
MASTER_S3_PREFIX     <- "master/bmf/"
# `aws s3 sync` is always incremental — files matching size+mtime are
# skipped. To force a fresh download, delete MASTER_STAGING_DIR before
# running. MIRROR_DELETE adds --delete to remove local files no longer
# present in S3 (use if you want strict mirror semantics).
MASTER_MIRROR_DELETE <- FALSE

# ============================================================================
# Library Loading
# ============================================================================

library(data.table)

# ============================================================================
# Source Helper Scripts
# ============================================================================

source(here::here("R", "config.R"))
source(here::here("R", "utils", "logging.R"))
source(here::here("R", "quality", "post_checks.R"))            # generate_data_dictionary()
source(here::here("R", "master_bmf_builder.R"))
source(here::here("R", "quality", "master_post_checks.R"))

# ============================================================================
# PHASE 1: DISCOVERY
# ============================================================================

log_phase_start("DISCOVERY")
inputs <- discover_master_inputs()
if (nrow(inputs) == 0) stop("No input files discovered.")

# ============================================================================
# PHASE 1.5: DOWNLOAD INPUTS TO LOCAL STAGING
# ============================================================================

log_phase_start("DOWNLOAD INPUTS")
log_info(sprintf("Staging dir: %s (mirror_delete=%s)",
                 MASTER_STAGING_DIR, MASTER_MIRROR_DELETE))
download_master_inputs(
  inputs,
  dest_dir      = MASTER_STAGING_DIR,
  mirror_delete = MASTER_MIRROR_DELETE
)

# ============================================================================
# PHASE 2: DUCKDB CONNECT
# ============================================================================

log_phase_start("DUCKDB CONNECT")
log_info(sprintf("memory_limit = %s, threads = %s, db = %s",
                 DUCKDB_MEMORY_LIMIT,
                 ifelse(is.null(DUCKDB_THREADS), "all", as.character(DUCKDB_THREADS)),
                 ifelse(is.null(DUCKDB_DB_PATH), "in-memory", DUCKDB_DB_PATH)))

# Inline the connection setup at top-level. Wrapping it in
# duckdb_connect_for_master() and returning con caused the connection
# to invalidate across the function boundary on duckdb 0.10.1 with
# error "rapi_prepare: Invalid connection". Keeping con scoped here
# keeps both the connection and the driver alive at the same level.
if (!requireNamespace("duckdb", quietly = TRUE)) {
  stop("Package 'duckdb' is required. install.packages('duckdb').")
}
.master_dbdir <- if (is.null(DUCKDB_DB_PATH)) ":memory:" else DUCKDB_DB_PATH
.master_drv   <- duckdb::duckdb()  # retain driver in this scope
con <- DBI::dbConnect(.master_drv, dbdir = .master_dbdir)
DBI::dbExecute(con, sprintf("SET memory_limit = '%s'", DUCKDB_MEMORY_LIMIT))
if (!is.null(DUCKDB_THREADS)) {
  DBI::dbExecute(con, sprintf("SET threads = %d", as.integer(DUCKDB_THREADS)))
}
if (!is.null(DUCKDB_TEMP_DIR)) {
  if (!dir.exists(DUCKDB_TEMP_DIR)) dir.create(DUCKDB_TEMP_DIR, recursive = TRUE)
  DBI::dbExecute(con, sprintf("SET temp_directory = '%s'",
                              normalizePath(DUCKDB_TEMP_DIR, mustWork = FALSE)))
  log_info(sprintf("temp_directory = %s", DUCKDB_TEMP_DIR))
}

# ============================================================================
# PHASE 3: BUILD
# ============================================================================

log_phase_start("BUILD")
build_master_bmf(con, inputs, staging_dir = MASTER_STAGING_DIR)

# ============================================================================
# PHASE 4: QUALITY CHECKS
# ============================================================================

log_phase_start("QUALITY CHECKS")
quality_report <- generate_master_quality_report(con, inputs)
print_master_quality_report(quality_report)

quality_report_path <- file.path(
  MASTER_QUALITY_DIR, "bmf_master_quality_report.json"
)
save_master_quality_report(quality_report, quality_report_path)

if (!quality_report$passed) {
  stop("Master BMF quality gate FAILED. See report above.")
}

# ============================================================================
# PHASE 5: WRITE OUTPUTS
# ============================================================================

log_phase_start("WRITE OUTPUTS")
out_paths <- write_master_outputs(con, out_dir = MASTER_OUTPUT_DIR)
for (nm in names(out_paths)) {
  log_info(sprintf("  %-11s -> %s", nm, out_paths[[nm]]))
}

# ============================================================================
# PHASE 6: S3 UPLOAD
# ============================================================================

if (ENABLE_S3_UPLOAD) {
  log_phase_start("S3 UPLOAD")
  upload_to_s3(out_paths$parquet,
               paste0(MASTER_S3_PREFIX, basename(out_paths$parquet)))
  upload_to_s3(out_paths$csv,
               paste0(MASTER_S3_PREFIX, basename(out_paths$csv)))
  upload_to_s3(out_paths$dictionary,
               paste0(MASTER_S3_PREFIX, basename(out_paths$dictionary)))
  upload_to_s3(quality_report_path,
               paste0(MASTER_S3_PREFIX, basename(quality_report_path)))
}

# ============================================================================
# PIPELINE COMPLETE
# ============================================================================

log_phase_start("PIPELINE COMPLETE")
log_info(sprintf("Master BMF rows (unique EINs): %s",
                 format(quality_report$total_rows, big.mark = ",")))
log_info(sprintf("Source files stacked: %d (current=%d, legacy=%d)",
                 quality_report$n_input_files,
                 sum(inputs$bmf_source == "current"),
                 sum(inputs$bmf_source == "legacy")))
log_info(sprintf("Quality gate: %s",
                 ifelse(quality_report$passed, "PASSED", "FAILED")))

# Cleanup
try(DBI::dbDisconnect(con, shutdown = TRUE), silent = TRUE)
