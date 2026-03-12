# ============================================================================
# geocoding_export.R
# Prepare geocoder batch files from processed BMF data
# ============================================================================

source(here::here("R/config.R"))
source(here::here("R/utils/logging.R"))

#' Prepare geocoder batch files from processed BMF
#'
#' @description
#' Reads the processed BMF CSV, filters out ungeocodable records, selects
#' the two columns required by the Urban Institute geocoder (ein + address),
#' splits into batches under the 1M row limit, and writes CSVs with a
#' manifest JSON.
#'
#' @param year Character or numeric year (YYYY)
#' @param month Character or numeric month (MM)
#' @param batch_size Maximum rows per batch (default: 900000)
#' @param s3_upload Whether to upload batches to S3 (default: TRUE)
#'
#' @return Invisible list with manifest contents
#'
#' @export
prepare_geocoder_batches <- function(year, month,
                                      batch_size = GEOCODER_BATCH_SIZE,
                                      s3_upload = TRUE) {

  year <- sprintf("%04d", as.integer(year))
  month <- sprintf("%02d", as.integer(month))

  # -------------------------------------------------------------------------
  # Read processed BMF
  # -------------------------------------------------------------------------
  processed_path <- here::here(
    sprintf("data/processed/bmf_%s_%s_processed.csv", year, month)
  )

  if (!file.exists(processed_path)) {
    stop(sprintf("Processed BMF not found: %s\nRun the pipeline first.", processed_path))
  }

  log_info(sprintf("Reading processed BMF: %s", processed_path))
  bmf <- data.table::fread(processed_path)
  total_records <- nrow(bmf)
  log_info(sprintf("Total records: %s", format(total_records, big.mark = ",")))

  # -------------------------------------------------------------------------
  # Validate required columns
  # -------------------------------------------------------------------------
  required_cols <- c("ein", "org_addr_full", "org_addr_is_missing")
  missing_cols <- setdiff(required_cols, names(bmf))
  if (length(missing_cols) > 0) {
    stop(sprintf("Missing required columns: %s", paste(missing_cols, collapse = ", ")))
  }

  # -------------------------------------------------------------------------
  # Filter out ungeocodable records
  # -------------------------------------------------------------------------
  geocodable <- bmf[
    org_addr_is_missing == FALSE & !is.na(org_addr_full) & org_addr_full != ""
  ]
  excluded_count <- total_records - nrow(geocodable)

  log_info(sprintf(
    "Geocodable: %s | Excluded (missing/invalid address): %s",
    format(nrow(geocodable), big.mark = ","),
    format(excluded_count, big.mark = ",")
  ))

  # -------------------------------------------------------------------------
  # Select and rename columns for geocoder
  # -------------------------------------------------------------------------
  # Geocoder requires exactly: ein, f_address
  geocoder_input <- geocodable[, .(ein = ein, f_address = org_addr_full)]

  # Sort by EIN for deterministic splitting
  data.table::setorder(geocoder_input, ein)

  # -------------------------------------------------------------------------
  # Split into batches
  # -------------------------------------------------------------------------
  n_rows <- nrow(geocoder_input)
  n_batches <- ceiling(n_rows / batch_size)

  batch_indices <- rep(seq_len(n_batches), each = batch_size, length.out = n_rows)
  batches <- split(geocoder_input, batch_indices)

  # -------------------------------------------------------------------------
  # Create output directories
  # -------------------------------------------------------------------------
  geocoding_dir <- here::here(sprintf("data/geocoding/%s_%s", year, month))
  input_dir <- file.path(geocoding_dir, "input")
  output_dir <- file.path(geocoding_dir, "output")
  merged_dir <- file.path(geocoding_dir, "merged")

  for (d in c(input_dir, output_dir, merged_dir)) {
    if (!dir.exists(d)) dir.create(d, recursive = TRUE)
  }

  # -------------------------------------------------------------------------
  # Write batch CSVs and build manifest
  # -------------------------------------------------------------------------
  batch_details <- vector("list", n_batches)
  batch_filenames <- character(n_batches)
  expected_output_filenames <- character(n_batches)

  for (i in seq_len(n_batches)) {
    batch_num <- sprintf("%02d", i)
    filename <- sprintf("bmf_%s_%s_geocoder_batch_%s.csv", year, month, batch_num)
    output_filename <- sprintf("bmf_%s_%s_geocoder_batch_%s_geocoded.csv", year, month, batch_num)
    filepath <- file.path(input_dir, filename)

    batch_data <- batches[[i]]

    # Write with EIN quoted to prevent leading-zero stripping
    data.table::fwrite(batch_data, filepath, quote = TRUE)

    batch_filenames[i] <- filename
    expected_output_filenames[i] <- output_filename

    batch_details[[i]] <- list(
      batch_number = i,
      filename = filename,
      expected_output_filename = output_filename,
      row_count = nrow(batch_data),
      first_ein = batch_data$ein[1],
      last_ein = batch_data$ein[nrow(batch_data)]
    )

    log_info(sprintf("  Batch %d: %s (%s rows)",
                     i, filename, format(nrow(batch_data), big.mark = ",")))
  }

  # -------------------------------------------------------------------------
  # Write manifest JSON
  # -------------------------------------------------------------------------
  manifest <- list(
    year = year,
    month = month,
    created_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"),
    status = "exported",
    total_records = total_records,
    geocodable_records = n_rows,
    excluded_records = excluded_count,
    batch_size = batch_size,
    num_batches = n_batches,
    batches = batch_details
  )

  manifest_path <- file.path(input_dir, sprintf("bmf_%s_%s_geocoder_manifest.json", year, month))
  jsonlite::write_json(manifest, manifest_path, pretty = TRUE, auto_unbox = TRUE)
  log_info(sprintf("Manifest saved: %s", manifest_path))

  # -------------------------------------------------------------------------
  # Upload to S3 if enabled
  # -------------------------------------------------------------------------
  if (s3_upload) {
    log_info("Uploading geocoder input batches to S3")
    s3_input_dir <- sprintf("%s%s_%s/input/", BMF_S3_GEOCODING_PREFIX, year, month)

    for (filename in batch_filenames) {
      upload_to_s3(
        local_file = file.path(input_dir, filename),
        s3_key = paste0(s3_input_dir, filename)
      )
    }

    # Upload manifest
    upload_to_s3(
      local_file = manifest_path,
      s3_key = paste0(s3_input_dir, basename(manifest_path))
    )
  }

  # -------------------------------------------------------------------------
  # Print operator instructions
  # -------------------------------------------------------------------------
  cat("\n")
  cat("=== GEOCODER INSTRUCTIONS ===\n")
  cat(sprintf("%d batch file(s) prepared in %s\n", n_batches, input_dir))
  for (i in seq_len(n_batches)) {
    cat(sprintf("  Batch %d: %s (%s rows)\n",
                i, batch_filenames[i],
                format(batch_details[[i]]$row_count, big.mark = ",")))
  }
  cat(sprintf("  Excluded: %s records with missing/invalid addresses\n",
              format(excluded_count, big.mark = ",")))
  cat("\n")
  cat("MANUAL STEPS:\n")
  cat("  1. Upload each CSV to the Urban Institute geocoder\n")
  cat("  2. Download geocoded results\n")
  cat(sprintf("  3. Save to %s as:\n", output_dir))
  for (i in seq_len(n_batches)) {
    cat(sprintf("     - %s\n", expected_output_filenames[i]))
  }
  cat(sprintf('  4. Run: merge_geocoded_results("%s", "%s")\n', year, month))
  cat("=============================\n")
  cat("\n")

  invisible(manifest)
}
