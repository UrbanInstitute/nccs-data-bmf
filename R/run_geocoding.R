# ============================================================================
# run_geocoding.R
# Geocoding Workflow Orchestrator
# ============================================================================
#
# Usage:
#   # Phase 1: Export batches for geocoding
#   GEOCODING_MODE <- "export"
#   source("R/run_geocoding.R")
#
#   # ... upload to Urban Institute geocoder, download results ...
#
#   # Phase 2: Merge geocoded results back into BMF
#   GEOCODING_MODE <- "merge"
#   source("R/run_geocoding.R")
#
#   # Override year/month (optional):
#   GEOCODING_YEAR <- 2026
#   GEOCODING_MONTH <- 3
#   GEOCODING_MODE <- "export"
#   source("R/run_geocoding.R")
# ============================================================================

# ============================================================================
# Configuration
# ============================================================================

# BMF year/month to geocode - NULL auto-detects latest processed BMF
if (!exists("GEOCODING_YEAR"))  GEOCODING_YEAR  <- NULL
if (!exists("GEOCODING_MONTH")) GEOCODING_MONTH <- NULL

# Workflow mode: "export" (Phase 1) or "merge" (Phase 2)
if (!exists("GEOCODING_MODE"))  GEOCODING_MODE  <- "export"

# Upload results to S3
ENABLE_S3_UPLOAD <- TRUE

# ============================================================================
# Library Loading
# ============================================================================

library(data.table)

# ============================================================================
# Source Helper Scripts
# ============================================================================

source(here::here("R", "config.R"))
source(here::here("R", "utils", "logging.R"))
source(here::here("R", "geocoding_export.R"))
source(here::here("R", "geocoding_merge.R"))

# ============================================================================
# Auto-detect latest processed BMF
# ============================================================================

#' Detect the most recent processed BMF file
#'
#' Scans data/processed/ for bmf_*_processed.csv files and returns the
#' year/month of the most recent one.
#'
#' @return List with \code{year} and \code{month} as integers
#' @noRd
detect_latest_processed_bmf <- function() {
  processed_dir <- here::here("data", "processed")
  files <- list.files(processed_dir, pattern = "^bmf_\\d{4}_\\d{2}_processed\\.csv$")

  if (length(files) == 0) {
    stop(paste0(
      "No processed BMF files found in ", processed_dir, "\n",
      "Run the BMF pipeline first: source('R/run_pipeline.R')"
    ))
  }

  # Extract year and month from filenames
  matches <- regmatches(files, regexpr("\\d{4}_\\d{2}", files))
  parts <- strsplit(matches, "_")
  years  <- as.integer(sapply(parts, `[`, 1))
  months <- as.integer(sapply(parts, `[`, 2))

  # Find the most recent (sort by year then month)
  idx <- order(years, months, decreasing = TRUE)[1]

  list(year = years[idx], month = months[idx])
}

# ============================================================================
# Resolve year/month
# ============================================================================

if (is.null(GEOCODING_YEAR) || is.null(GEOCODING_MONTH)) {
  detected <- detect_latest_processed_bmf()
  if (is.null(GEOCODING_YEAR))  GEOCODING_YEAR  <- detected$year
  if (is.null(GEOCODING_MONTH)) GEOCODING_MONTH <- detected$month
  log_info(sprintf("Auto-detected BMF: %04d-%02d", GEOCODING_YEAR, GEOCODING_MONTH))
}

year  <- GEOCODING_YEAR
month <- GEOCODING_MONTH

# ============================================================================
# Mode Dispatch
# ============================================================================

if (GEOCODING_MODE == "export") {
  log_phase_start("GEOCODING EXPORT")
  log_info(sprintf("Processing BMF %04d-%02d", as.integer(year), as.integer(month)))

  prepare_geocoder_batches(year, month, s3_upload = ENABLE_S3_UPLOAD)

  log_info("Next step: upload batches to geocoder, download results, then run with GEOCODING_MODE <- \"merge\"")

} else if (GEOCODING_MODE == "merge") {
  log_phase_start("GEOCODING MERGE")
  log_info(sprintf("Processing BMF %04d-%02d", as.integer(year), as.integer(month)))

  merged <- merge_geocoded_results(year, month, s3_upload = ENABLE_S3_UPLOAD)

  log_info(sprintf(
    "Geocoded BMF: %s rows, %d columns",
    format(nrow(merged), big.mark = ","), ncol(merged)
  ))
  log_info("Geocoding workflow complete")

} else {
  stop(sprintf(
    "Invalid GEOCODING_MODE: '%s'. Use 'export' or 'merge'.",
    GEOCODING_MODE
  ))
}
