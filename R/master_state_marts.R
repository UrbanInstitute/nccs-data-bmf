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
#   data/master/state_marts/csv/bmf_master_XX.csv            (one per state)
#
# S3 (if enabled), dual-written for the ADR 0039 90-day deprecation window:
#   s3://nccsdata/unified/bmf/state_marts/parquet/state=XX/...  (new)
#   s3://nccsdata/unified/bmf/state_marts/csv/bmf_master_XX.csv (new)
#   s3://nccsdata/master/bmf/state_marts/parquet/state=XX/...   (old, unchanged)
#   s3://nccsdata/master/bmf/state_marts/csv/bmf_master_XX.csv  (old, unchanged)
# ============================================================================

#' Build per-state data marts from the geocoded Unified BMF
#'
#' @param geocoded_path  Path to bmf_unified_geocoded.parquet
#' @param output_dir     Output root (default: data/master/state_marts)
#' @param s3_upload      Upload marts to S3 (default: TRUE)
#' @param missing_state_bucket  Bucket name for rows with NA state (default: "ZZ")
#' @return Invisibly: data.table of per-state row counts and paths
#' @export
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
    csv_path <- file.path(csv_dir, sprintf("bmf_master_%s.csv", st))
    data.table::fwrite(bmf[state == st], csv_path)
    csv_paths[i] <- csv_path
  }
  log_info(sprintf("Wrote %d state CSVs", length(csv_paths)))

  # --------------------------------------------------------------------------
  # S3 upload
  # --------------------------------------------------------------------------
  if (s3_upload) {
    log_info("Uploading state marts to S3")
    # ADR 0039 dual-write: new unified/ prefix + old master/ prefix
    # (unchanged), same 90-day deprecation window as the geocoded merge.
    s3_roots <- c("unified/bmf/state_marts", "master/bmf/state_marts")

    parquet_files <- list.files(parquet_dir, recursive = TRUE, full.names = TRUE)
    for (s3_root in s3_roots) {
      for (pf in parquet_files) {
        rel <- sub(paste0("^", parquet_dir, "/"), "", pf)
        upload_to_s3(pf, file.path(s3_root, "parquet", rel))
      }
      for (cf in csv_paths) {
        upload_to_s3(cf, file.path(s3_root, "csv", basename(cf)))
      }
    }
  }

  state_counts[, parquet_partition := file.path(parquet_dir,
                                                paste0("state=", state))]
  state_counts[, csv_path := file.path(csv_dir,
                                       sprintf("bmf_master_%s.csv", state))]
  invisible(state_counts)
}
