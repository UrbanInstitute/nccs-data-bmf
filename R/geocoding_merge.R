# ============================================================================
# geocoding_merge.R
# Merge geocoded results back into processed BMF
# ============================================================================

source(here::here("R/config.R"))
source(here::here("R/utils/logging.R"))
source(here::here("R/quality/geocoding_checks.R"))

#' Merge geocoded results with processed BMF
#'
#' @description
#' Reads geocoder output batch files, validates them against the manifest,
#' renames columns, derives precision tiers, and left-joins with the
#' processed BMF. Produces merged parquet + CSV, quality report, and
#' data dictionary.
#'
#' @param year Character or numeric year (YYYY)
#' @param month Character or numeric month (MM)
#' @param s3_upload Whether to upload results to S3 (default: TRUE)
#'
#' @return Invisible data.table of merged geocoded BMF
#'
#' @export
merge_geocoded_results <- function(year, month, s3_upload = TRUE) {

  year <- sprintf("%04d", as.integer(year))
  month <- sprintf("%02d", as.integer(month))

  geocoding_dir <- here::here(sprintf("data/geocoding/%s_%s", year, month))
  input_dir <- file.path(geocoding_dir, "input")
  output_dir <- file.path(geocoding_dir, "output")
  merged_dir <- file.path(geocoding_dir, "merged")

  # -------------------------------------------------------------------------
  # Read and validate manifest
  # -------------------------------------------------------------------------
  manifest_path <- file.path(
    input_dir,
    sprintf("bmf_%s_%s_geocoder_manifest.json", year, month)
  )

  if (!file.exists(manifest_path)) {
    stop(sprintf(
      "Manifest not found: %s\nRun prepare_geocoder_batches() first.",
      manifest_path
    ))
  }

  manifest <- jsonlite::read_json(manifest_path)
  n_batches <- manifest$num_batches
  log_info(sprintf("Manifest loaded: %d batches expected", n_batches))

  # -------------------------------------------------------------------------
  # Check all expected output files exist
  # -------------------------------------------------------------------------
  expected_files <- sapply(manifest$batches, function(b) b$expected_output_filename)
  existing_files <- list.files(output_dir, pattern = "\\.csv$")
  missing_files <- setdiff(expected_files, existing_files)

  if (length(missing_files) > 0) {
    stop(sprintf(
      "Missing geocoded output files in %s:\n  %s\n\nUpload these to the geocoder and save results with the exact filenames above.",
      output_dir,
      paste(missing_files, collapse = "\n  ")
    ))
  }

  # -------------------------------------------------------------------------
  # Read and combine all geocoded batches
  # -------------------------------------------------------------------------
  log_info("Reading geocoded batch files")
  batch_list <- vector("list", n_batches)

  for (i in seq_len(n_batches)) {
    filepath <- file.path(output_dir, expected_files[i])
    batch_list[[i]] <- data.table::fread(filepath, colClasses = c(ein = "character"))
    log_info(sprintf("  Batch %d: %s rows", i, format(nrow(batch_list[[i]]), big.mark = ",")))
  }

  geocoded <- data.table::rbindlist(batch_list, fill = TRUE)
  log_info(sprintf("Combined geocoded records: %s", format(nrow(geocoded), big.mark = ",")))

  # -------------------------------------------------------------------------
  # Validate geocoded data
  # -------------------------------------------------------------------------
  # Check ein column exists
  if (!"ein" %in% names(geocoded)) {
    stop("Geocoded data is missing 'ein' column. Check geocoder output format.")
  }

  # Check for duplicate EINs
  dup_count <- sum(duplicated(geocoded$ein))
  if (dup_count > 0) {
    warning(sprintf(
      "Found %s duplicate EINs in geocoded data. Keeping first occurrence.",
      format(dup_count, big.mark = ",")
    ))
    geocoded <- geocoded[!duplicated(geocoded$ein), ]
  }

  # Check row count against manifest
  expected_rows <- manifest$geocodable_records
  actual_rows <- nrow(geocoded)
  if (actual_rows != expected_rows) {
    warning(sprintf(
      "Row count mismatch: manifest expects %s, got %s",
      format(expected_rows, big.mark = ","),
      format(actual_rows, big.mark = ",")
    ))
  }

  # -------------------------------------------------------------------------
  # Validate expected geocoder columns exist
  # -------------------------------------------------------------------------
  available_geo_cols <- intersect(names(GEOCODER_COLUMN_MAP), names(geocoded))
  missing_geo_cols <- setdiff(names(GEOCODER_COLUMN_MAP), names(geocoded))

  if (length(missing_geo_cols) > 0) {
    warning(sprintf(
      "Geocoder output missing expected columns: %s\nThese will be NA in merged output.",
      paste(missing_geo_cols, collapse = ", ")
    ))
  }

  # -------------------------------------------------------------------------
  # Select and rename geocoder columns
  # -------------------------------------------------------------------------
  # Keep only ein + known geocoder columns
  cols_to_keep <- c("ein", available_geo_cols)
  geocoded_clean <- geocoded[, ..cols_to_keep]

  # Rename columns
  for (old_name in available_geo_cols) {
    new_name <- GEOCODER_COLUMN_MAP[old_name]
    data.table::setnames(geocoded_clean, old_name, new_name)
  }

  # Add columns that were missing as NA
  for (old_name in missing_geo_cols) {
    new_name <- GEOCODER_COLUMN_MAP[old_name]
    geocoded_clean[, (new_name) := NA_character_]
  }

  # Ensure numeric types
  if ("geo_lat" %in% names(geocoded_clean)) {
    geocoded_clean[, geo_lat := as.numeric(geo_lat)]
  }
  if ("geo_lon" %in% names(geocoded_clean)) {
    geocoded_clean[, geo_lon := as.numeric(geo_lon)]
  }
  if ("geo_score" %in% names(geocoded_clean)) {
    geocoded_clean[, geo_score := as.numeric(geo_score)]
  }
  if ("geo_distance" %in% names(geocoded_clean)) {
    geocoded_clean[, geo_distance := as.numeric(geo_distance)]
  }

  # -------------------------------------------------------------------------
  # Derive columns
  # -------------------------------------------------------------------------
  geocoded_clean[, geo_is_geocoded := !is.na(geo_lat) & !is.na(geo_lon)]
  geocoded_clean[, geo_precision := derive_geo_precision(geo_addr_type)]

  log_info(sprintf(
    "Geocoded records with coordinates: %s / %s",
    format(sum(geocoded_clean$geo_is_geocoded), big.mark = ","),
    format(nrow(geocoded_clean), big.mark = ",")
  ))

  # -------------------------------------------------------------------------
  # Read processed BMF and left-join
  # -------------------------------------------------------------------------
  processed_path <- here::here(
    sprintf("data/processed/bmf_%s_%s_processed.csv", year, month)
  )

  if (!file.exists(processed_path)) {
    stop(sprintf("Processed BMF not found: %s", processed_path))
  }

  log_info("Reading processed BMF for merge")
  bmf <- data.table::fread(processed_path, colClasses = c(ein = "character"))

  # Left join: all processed records, with geo columns where available
  merged <- merge(
    bmf, geocoded_clean,
    by = "ein",
    all.x = TRUE,
    sort = FALSE
  )

  # Ensure geo_is_geocoded is FALSE (not NA) for unjoined records
  merged[is.na(geo_is_geocoded), geo_is_geocoded := FALSE]

  log_info(sprintf(
    "Merged BMF: %s rows (processed: %s, geocoded: %s)",
    format(nrow(merged), big.mark = ","),
    format(nrow(bmf), big.mark = ","),
    format(sum(merged$geo_is_geocoded), big.mark = ",")
  ))

  # Validate row preservation
  if (nrow(merged) != nrow(bmf)) {
    warning(sprintf(
      "Row count changed after merge: %s -> %s. Possible duplicate EINs in geocoded data.",
      format(nrow(bmf), big.mark = ","),
      format(nrow(merged), big.mark = ",")
    ))
  }

  # -------------------------------------------------------------------------
  # Save merged output
  # -------------------------------------------------------------------------
  if (!dir.exists(merged_dir)) dir.create(merged_dir, recursive = TRUE)

  # Parquet
  parquet_path <- file.path(merged_dir, sprintf("bmf_%s_%s_geocoded.parquet", year, month))
  arrow::write_parquet(merged, parquet_path)
  log_info(sprintf("Merged parquet saved: %s", parquet_path))

  # CSV
  csv_path <- file.path(merged_dir, sprintf("bmf_%s_%s_geocoded.csv", year, month))
  data.table::fwrite(merged, csv_path, quote = TRUE)
  log_info(sprintf("Merged CSV saved: %s", csv_path))

  # -------------------------------------------------------------------------
  # Quality report
  # -------------------------------------------------------------------------
  log_info("Generating geocoding quality report")
  quality_report <- generate_geocoding_quality_report(merged, manifest)
  quality_path <- file.path(
    merged_dir,
    sprintf("bmf_%s_%s_geocoding_quality_report.json", year, month)
  )
  save_geocoding_quality_report(quality_report, quality_path)

  # -------------------------------------------------------------------------
  # Data dictionary
  # -------------------------------------------------------------------------
  log_info("Generating data dictionary")
  data_dictionary <- generate_data_dictionary(merged)
  dictionary_path <- file.path(
    merged_dir,
    sprintf("bmf_%s_%s_geocoding_data_dictionary.csv", year, month)
  )
  data.table::fwrite(data_dictionary, dictionary_path)
  log_info(sprintf("Data dictionary saved: %s", dictionary_path))

  # -------------------------------------------------------------------------
  # Update manifest status
  # -------------------------------------------------------------------------
  manifest$status <- "merged"
  manifest$merged_at <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S")
  manifest$merged_row_count <- nrow(merged)
  manifest$geocoded_with_coordinates <- sum(merged$geo_is_geocoded)
  jsonlite::write_json(manifest, manifest_path, pretty = TRUE, auto_unbox = TRUE)
  log_info("Manifest updated to 'merged' status")

  # -------------------------------------------------------------------------
  # Upload to S3 if enabled
  # -------------------------------------------------------------------------
  if (s3_upload) {
    log_info("Uploading geocoded results to S3")
    upload_geocoded_bmf(
      parquet_path = parquet_path,
      csv_path = csv_path,
      quality_report_path = quality_path,
      dictionary_path = dictionary_path,
      year = year,
      month = month
    )
  }

  log_info("Geocoding merge complete")
  invisible(merged)
}
