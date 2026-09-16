# ============================================================================
# master_state_marts.R
#
# Splits the geocoded Master BMF into per-state data marts so end users
# can pull only the rows they need instead of the full 3.1 GB file.
#
# Partition key is `org_addr_state` (cleaned mailing state). Rows with
# NA/missing state are bucketed into `ZZ` so they remain discoverable.
#
#   build_master_state_marts()
#
# Outputs:
#   data/master/state_marts/parquet/state=XX/part-0.parquet  (Hive-partitioned)
#   data/master/state_marts/csv/bmf_unified_XX.csv           (one per state)
#
# S3 (if enabled), dual-written for the ADR 0039 90-day deprecation window:
#   s3://nccsdata/unified/bmf/state_marts/parquet/state=XX/...   (current)
#   s3://nccsdata/unified/bmf/state_marts/csv/bmf_unified_XX.csv (current; ADR 0039 file name, first published 2026-09-16)
#   s3://nccsdata/unified/bmf/state_marts/csv/bmf_master_XX.csv  (old file name, written as a copy through STATE_MART_OLD_STEM_CUTOVER)
#   s3://nccsdata/master/bmf/state_marts/...                     (old folder, written through STATE_MART_OLD_PREFIX_CUTOVER)
# ============================================================================

#' Build per-state data marts from the geocoded Unified BMF
#'
#' @param geocoded_path  Path to bmf_unified_geocoded.parquet
#' @param output_dir     Output root (default: data/master/state_marts)
#' @param s3_upload      Upload marts to S3 (default: TRUE)
#' @param missing_state_bucket  Bucket name for rows with NA state (default: "ZZ")
#' @return Invisibly: data.table of per-state row counts and paths
#' @export
# TEMPORARY (backlog Z19): remove this block, state_mart_s3_roots(),
# state_mart_write_old_stem(), the retired_name argument and the matching
# upload lines once both dates below have passed (from 2026-12-16).
#
# Two retirement dates (ADR 0033: 90 days each), both inclusive:
#
# STATE_MART_OLD_PREFIX_CUTOVER: the old folder master/bmf/state_marts/ was
#   given its own 90-day clock from 2026-07-02 (ADR 0039), so it is written
#   through 2026-09-30 and not from 2026-10-01. (The plain Unified BMF's
#   master/bmf/ date is 2026-09-28; that is a different clock.)
#
# STATE_MART_OLD_STEM_CUTOVER: the per-state CSV was published as
#   bmf_master_XX.csv until 2026-09-16 even though the contract (ADR 0039)
#   names it bmf_unified_XX.csv. Both names are written through 2026-12-15
#   so existing links keep working; from 2026-12-16 only bmf_unified_XX.csv
#   is written. Backlog Z19. If the one-time copy of the live files to the
#   new name lands after 2026-09-16, move this date to 90 days after it.
STATE_MART_OLD_PREFIX_CUTOVER <- "2026-09-30"
STATE_MART_OLD_STEM_CUTOVER   <- "2026-12-15"

#' File name of the per-state CSV for one state code
#' @param state Two-letter state / territory code (or "ZZ")
#' @param retired_name TRUE returns the retired name (bmf_master_XX.csv),
#'   used only while the old name is still written. TEMPORARY, see above.
#' @noRd
state_mart_csv_name <- function(state, retired_name = FALSE) {
  sprintf(if (retired_name) "bmf_master_%s.csv" else "bmf_unified_%s.csv", state)
}

#' Should the old bmf_master_XX.csv copy still be written? (TRUE through the cutover date)
#' @noRd
state_mart_write_old_stem <- function(today = Sys.Date()) {
  as.Date(today) <= as.Date(STATE_MART_OLD_STEM_CUTOVER)
}

#' S3 folders the marts are uploaded to on a given date
#' @return Character vector: always unified/bmf/state_marts; plus the old
#'   master/bmf/state_marts through STATE_MART_OLD_PREFIX_CUTOVER.
#' @noRd
state_mart_s3_roots <- function(today = Sys.Date()) {
  roots <- "unified/bmf/state_marts"
  if (as.Date(today) <= as.Date(STATE_MART_OLD_PREFIX_CUTOVER)) {
    roots <- c(roots, "master/bmf/state_marts")
  }
  roots
}

build_master_state_marts <- function(
    geocoded_path = here::here("data", "geocoding", "master", "merged",
                               "bmf_unified_geocoded.parquet"),
    output_dir    = here::here("data", "master", "state_marts"),
    s3_upload     = TRUE,
    missing_state_bucket = "ZZ"
  ) {

  if (!file.exists(geocoded_path)) {
    stop(sprintf("Geocoded master BMF not found: %s", geocoded_path))
  }

  parquet_dir <- file.path(output_dir, "parquet")
  csv_dir     <- file.path(output_dir, "csv")
  dir.create(parquet_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(csv_dir,     recursive = TRUE, showWarnings = FALSE)

  log_info(sprintf("Reading geocoded master BMF: %s", geocoded_path))
  bmf <- arrow::read_parquet(geocoded_path) |> data.table::as.data.table()
  log_info(sprintf("Total rows: %s", format(nrow(bmf), big.mark = ",")))

  if (!"org_addr_state" %in% names(bmf)) {
    stop("Geocoded master is missing `org_addr_state`.")
  }

  bmf[, state := data.table::fifelse(
    is.na(org_addr_state) | nchar(org_addr_state) == 0,
    missing_state_bucket,
    toupper(org_addr_state)
  )]

  state_counts <- bmf[, .N, by = state][order(state)]
  log_info(sprintf("Distinct states: %d (incl. '%s' bucket if present)",
                   nrow(state_counts), missing_state_bucket))

  # --------------------------------------------------------------------------
  # Parquet: Hive-partitioned write
  # --------------------------------------------------------------------------
  log_info(sprintf("Writing Hive-partitioned parquet to %s", parquet_dir))
  unlink(parquet_dir, recursive = TRUE)
  dir.create(parquet_dir, recursive = TRUE, showWarnings = FALSE)
  arrow::write_dataset(
    dataset    = bmf,
    path       = parquet_dir,
    format     = "parquet",
    partitioning = "state",
    basename_template = "part-{i}.parquet"
  )

  # --------------------------------------------------------------------------
  # CSV: one file per state
  # --------------------------------------------------------------------------
  log_info(sprintf("Writing per-state CSVs to %s", csv_dir))
  csv_paths <- character(nrow(state_counts))
  for (i in seq_len(nrow(state_counts))) {
    st <- state_counts$state[i]
    csv_path <- file.path(csv_dir, state_mart_csv_name(st))
    data.table::fwrite(bmf[state == st], csv_path)
    csv_paths[i] <- csv_path
  }
  log_info(sprintf("Wrote %d state CSVs", length(csv_paths)))

  # --------------------------------------------------------------------------
  # S3 upload
  # --------------------------------------------------------------------------
  if (s3_upload) {
    log_info("Uploading state marts to S3")
    # TEMPORARY (backlog Z19, remove from 2026-12-16): the old master/
    # folder is written only through its retirement date and the old file
    # name only through its own; after that, upload only to
    # unified/bmf/state_marts with the bmf_unified_XX.csv name.
    s3_roots <- state_mart_s3_roots()
    write_old_stem <- state_mart_write_old_stem()
    log_info(sprintf("Uploading to: %s", paste(s3_roots, collapse = ", ")))
    if (write_old_stem) {
      log_info(sprintf("Also writing the old bmf_master_XX.csv file name (through %s)",
                       STATE_MART_OLD_STEM_CUTOVER))
    }

    parquet_files <- list.files(parquet_dir, recursive = TRUE, full.names = TRUE)
    for (s3_root in s3_roots) {
      for (pf in parquet_files) {
        rel <- sub(paste0("^", parquet_dir, "/"), "", pf)
        upload_to_s3(pf, file.path(s3_root, "parquet", rel))
      }
      for (i in seq_along(csv_paths)) {
        cf <- csv_paths[i]
        st <- state_counts$state[i]
        if (s3_root == "unified/bmf/state_marts") {
          upload_to_s3(cf, file.path(s3_root, "csv", state_mart_csv_name(st)))
        }
        # TEMPORARY (backlog Z19): old file name, kept under both folders
        # while each is still written.
        if (write_old_stem) {
          upload_to_s3(cf, file.path(s3_root, "csv", state_mart_csv_name(st, retired_name = TRUE)))
        }
      }
    }
  }

  state_counts[, parquet_partition := file.path(parquet_dir,
                                                paste0("state=", state))]
  state_counts[, csv_path := file.path(csv_dir, state_mart_csv_name(state))]
  invisible(state_counts)
}
