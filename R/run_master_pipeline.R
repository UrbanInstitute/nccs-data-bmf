# ============================================================================
# run_master_pipeline.R
#
# Build the Master BMF: one row per EIN, drawn from the most-recent vintage
# in which that EIN appears across both the current monthly BMF pipeline
# and the legacy 501CX-NONPROFIT-PX pipeline.
#
# Renamed to the "Unified BMF" (ADR 0037); carries additive coercion-safe EIN
# columns ein_prefixed + EIN2 (ADR 0036) and a per-build _manifest.json.
#
# Outputs (stem = UNIFIED_STEM, default "bmf_unified"):
#   data/master/<stem>.parquet              (full schema, zstd-compressed)
#   data/master/<stem>.csv                  (same rows, CSV form)
#   data/master/<stem>_data_dictionary.csv
#   data/master/_manifest.json              (ADR 0014 per-build manifest)
#   data/quality/<stem>_quality_report.json
#
# S3 (if ENABLE_S3_UPLOAD), under UNIFIED_S3_PREFIX (default "unified/"):
#   s3://nccsdata/unified/<stem>.parquet
#   s3://nccsdata/unified/<stem>.csv
#   s3://nccsdata/unified/<stem>_data_dictionary.csv
#   s3://nccsdata/unified/_manifest.json
#   s3://nccsdata/unified/<stem>_quality_report.json
#
# NOTE: the exact unified path/stem are PENDING contracts ratification
# (needs-ADR-review) — see the ADR 0037 config block below. The prior
# master/bmf/ artifact stays reachable for 90 days, then archives (not deleted).
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

# ---------------------------------------------------------------------------
# ADR 0037 — Master BMF -> Unified BMF rename (non-silent supersession).
#
# The master artifact is renamed to the restored community name "Unified BMF"
# and published under unified/. The prior master/bmf/ artifact stays live and
# reachable for the 90-day deprecation window (do NOT delete or silently move
# it), then moves to a retained, reachable archive at cutover (~2026-09-28).
#
#   >>> needs-ADR-review (escalation gate): the EXACT unified path layout and
#   >>> filenames are NOT pinned by ADR 0037 (§5 defers them to the in-flight
#   >>> ADR 0013 versioning + /latest work). The values below are the proposed
#   >>> default; they must be ratified in a nccs-contracts session BEFORE the
#   >>> first public publish to unified/. Open questions for ratification:
#   >>>   - prefix: "unified/" vs "unified/bmf/"
#   >>>   - stem:   snake_case "bmf_unified" (house style) vs the historical
#   >>>             uppercase/versioned "UNIFIED_BMF_V1.2" form
#   >>>   - versioned subdir + latest/ mirror (ADR 0013) yes/no
# ---------------------------------------------------------------------------
UNIFIED_S3_PREFIX    <- "unified/"      # PENDING RATIFICATION (see above)
UNIFIED_STEM         <- "bmf_unified"   # PENDING RATIFICATION (see above)
# Prior path kept live as the 90-day fallback; archived (not deleted) at cutover.
MASTER_LEGACY_S3_PREFIX <- "master/bmf/"
MASTER_DEPRECATION_CUTOVER <- "2026-09-28"  # 90 days from 2026-06-30
# Build-vintage tag recorded in the manifest (newest current vintage).
MASTER_VINTAGE       <- format(Sys.Date(), "%Y_%m")
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
source(here::here("R", "manifest.R"))
source(here::here("R", "publish_lookups.R"))

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
  MASTER_QUALITY_DIR, paste0(UNIFIED_STEM, "_quality_report.json")
)
save_master_quality_report(quality_report, quality_report_path)

quality_report_html <- file.path(
  here::here("docs", "quality-reports"),
  paste0(UNIFIED_STEM, "_quality_report.html")
)
render_master_quality_report(quality_report, quality_report_html)

source(here::here("R", "utils", "render_quality_report_index.R"))
render_quality_report_index(dirname(quality_report_html))

if (!quality_report$passed) {
  stop("Master BMF quality gate FAILED. See report above.")
}

# ============================================================================
# PHASE 5: WRITE OUTPUTS
# ============================================================================

log_phase_start("WRITE OUTPUTS")
# ADR 0014/0037 manifest inputs: the two processed-BMF prefixes the build read.
master_manifest_inputs <- list(
  list(uri  = sprintf("s3://%s/%s", BMF_S3_BUCKET, BMF_S3_PROCESSED_PREFIX),
       note = "current-pipeline processed CSVs (all vintages)"),
  list(uri  = sprintf("s3://%s/%s", BMF_S3_BUCKET, BMF_S3_LEGACY_PROCESSED_PREFIX),
       note = "legacy-pipeline processed CSVs (all vintages)")
)
out_paths <- write_master_outputs(
  con, out_dir = MASTER_OUTPUT_DIR, stem = UNIFIED_STEM,
  inputs = master_manifest_inputs, vintage = MASTER_VINTAGE
)
for (nm in names(out_paths)) {
  log_info(sprintf("  %-11s -> %s", nm, out_paths[[nm]]))
}

# ============================================================================
# PHASE 6: S3 UPLOAD
# ============================================================================

if (ENABLE_S3_UPLOAD) {
  log_phase_start("S3 UPLOAD")
  # ADR 0037: publish the renamed Unified BMF under UNIFIED_S3_PREFIX.
  # NOTE (needs-ADR-review): the prefix/stem are PENDING contracts ratification
  # — see the ADR 0037 block at the top of this file. Confirm the public path
  # before the first publish; the prior master/bmf/ artifact stays reachable for
  # the 90-day window and is archived (not deleted) at MASTER_DEPRECATION_CUTOVER.
  upload_to_s3(out_paths$parquet,
               paste0(UNIFIED_S3_PREFIX, basename(out_paths$parquet)))
  upload_to_s3(out_paths$csv,
               paste0(UNIFIED_S3_PREFIX, basename(out_paths$csv)))
  upload_to_s3(out_paths$dictionary,
               paste0(UNIFIED_S3_PREFIX, basename(out_paths$dictionary)))
  if (!is.null(out_paths$manifest) && file.exists(out_paths$manifest)) {
    upload_to_s3(out_paths$manifest,
                 paste0(UNIFIED_S3_PREFIX, basename(out_paths$manifest)))
  }
  upload_to_s3(quality_report_path,
               paste0(UNIFIED_S3_PREFIX, basename(quality_report_path)))
  if (file.exists(quality_report_html)) {
    upload_to_s3(quality_report_html,
                 paste0(UNIFIED_S3_PREFIX, basename(quality_report_html)))
  }
}

# ============================================================================
# PHASE 7: PUBLISH LOOKUPS
# ============================================================================
# Mirror the canonical BMF lookup tables (lookup_ls) to S3 so downstream
# consumers (e.g. the nccsdata R package) can pull a stable, versioned
# snapshot. See R/publish_lookups.R and the "Published artifacts" section
# of CLAUDE.md for the path contract.

if (ENABLE_S3_UPLOAD) {
  log_phase_start("PUBLISH LOOKUPS")
  publish_bmf_lookups()
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
