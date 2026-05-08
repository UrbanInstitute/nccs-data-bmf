# ============================================================================
# master_geocoding.R
#
# Geocoding for the Master BMF. Two functions mirroring the per-month
# geocoding workflow but tuned for a single bmf_master.parquet input:
#
#   prepare_master_geocoder_batches()  -- export mode
#   merge_master_geocoded_results()    -- merge mode
#
# Address dedup before export typically cuts ~30-40 % of geocoder rows
# (multiple EINs share addresses). One representative EIN is chosen per
# distinct org_addr_full; lat/lon are joined back to all EINs sharing
# that address in the merge step.
# ============================================================================

#' Export master BMF addresses as geocoder batches
#'
#' Reads `data/master/bmf_master.parquet` (or the path you pass), keeps
#' only rows with usable addresses, dedups on `org_addr_full`, splits
#' into batches under the geocoder's row limit, and writes one CSV per
#' batch plus an address-lookup manifest used by the merge step.
#'
#' @param master_path     Path to bmf_master.parquet
#' @param geocoding_dir   Output directory (default: data/geocoding/master)
#' @param batch_size      Max rows per batch (default: GEOCODER_BATCH_SIZE)
#' @param s3_upload       Upload batches and manifest to S3 (default: TRUE)
#' @return Invisibly: list with batch counts and paths
#' @export
prepare_master_geocoder_batches <- function(
    master_path   = here::here("data", "master", "bmf_master.parquet"),
    geocoding_dir = here::here("data", "geocoding", "master"),
    batch_size    = GEOCODER_BATCH_SIZE,
    s3_upload     = TRUE
  ) {

  if (!file.exists(master_path)) {
    stop(sprintf("Master BMF not found: %s\nRun the master pipeline first.",
                 master_path))
  }
  log_info(sprintf("Reading master BMF: %s", master_path))
  bmf <- arrow::read_parquet(master_path) |> data.table::as.data.table()
  total_records <- nrow(bmf)
  log_info(sprintf("Total master records: %s",
                   format(total_records, big.mark = ",")))

  required_cols <- c("ein", "org_addr_full")
  missing_cols <- setdiff(required_cols, names(bmf))
  if (length(missing_cols) > 0) {
    stop(sprintf("Master BMF missing required columns: %s",
                 paste(missing_cols, collapse = ", ")))
  }

  # --------------------------------------------------------------------------
  # Filter geocodable rows
  # --------------------------------------------------------------------------
  bmf[, org_addr_full := trimws(as.character(org_addr_full))]
  geocodable <- bmf[!is.na(org_addr_full) & nchar(org_addr_full) > 0]
  excluded <- total_records - nrow(geocodable)
  log_info(sprintf("Geocodable rows: %s | Excluded (no address): %s",
                   format(nrow(geocodable), big.mark = ","),
                   format(excluded, big.mark = ",")))

  # --------------------------------------------------------------------------
  # Dedup on org_addr_full -> one representative EIN per unique address
  # --------------------------------------------------------------------------
  data.table::setorder(geocodable, ein)
  unique_addr <- geocodable[, .(ein = ein[1L]),
                            by = .(f_address = org_addr_full)]
  data.table::setorder(unique_addr, ein)
  unique_addr <- unique_addr[, .(ein, f_address)]

  log_info(sprintf("Unique addresses: %s (dedup ratio %.1f %%)",
                   format(nrow(unique_addr), big.mark = ","),
                   100 * (1 - nrow(unique_addr) / nrow(geocodable))))

  # --------------------------------------------------------------------------
  # Output dirs
  # --------------------------------------------------------------------------
  input_dir  <- file.path(geocoding_dir, "input")
  output_dir <- file.path(geocoding_dir, "output")
  merged_dir <- file.path(geocoding_dir, "merged")
  for (d in c(input_dir, output_dir, merged_dir)) {
    if (!dir.exists(d)) dir.create(d, recursive = TRUE)
  }

  # --------------------------------------------------------------------------
  # Address-lookup manifest: f_address -> representative_ein.
  # Used by the merge step to propagate lat/lon to all EINs sharing an
  # address. Saved as parquet because it can be 3-4M rows.
  # --------------------------------------------------------------------------
  addr_lookup <- geocodable[, .(ein, org_addr_full)]
  data.table::setnames(addr_lookup, "ein", "ein_all")
  addr_lookup <- merge(addr_lookup, unique_addr,
                        by.x = "org_addr_full", by.y = "f_address",
                        all.x = TRUE)
  data.table::setnames(addr_lookup, "ein", "representative_ein")

  addr_lookup_path <- file.path(input_dir,
                                "bmf_master_geocoder_addr_lookup.parquet")
  arrow::write_parquet(addr_lookup, addr_lookup_path)
  log_info(sprintf("Address-lookup manifest: %s (%s rows)",
                   addr_lookup_path,
                   format(nrow(addr_lookup), big.mark = ",")))

  # --------------------------------------------------------------------------
  # Split unique_addr into batches
  # --------------------------------------------------------------------------
  n_rows    <- nrow(unique_addr)
  n_batches <- ceiling(n_rows / batch_size)
  batch_indices <- rep(seq_len(n_batches), each = batch_size,
                       length.out = n_rows)
  batches <- split(unique_addr, batch_indices)

  batch_filenames <- character(n_batches)
  expected_outputs <- character(n_batches)
  batch_details   <- vector("list", n_batches)

  for (i in seq_len(n_batches)) {
    batch_num <- sprintf("%02d", i)
    fn  <- sprintf("bmf_master_geocoder_batch_%s.csv", batch_num)
    ofn <- sprintf("bmf_master_geocoder_batch_%s_geocoded.csv", batch_num)
    fp  <- file.path(input_dir, fn)
    bd  <- batches[[i]]

    # Quote EIN to preserve any leading-character formatting.
    data.table::fwrite(bd, fp, quote = TRUE)

    batch_filenames[i]  <- fn
    expected_outputs[i] <- ofn
    batch_details[[i]] <- list(
      batch_number = i,
      filename     = fn,
      expected_output_filename = ofn,
      row_count    = nrow(bd),
      first_ein    = bd$ein[1L],
      last_ein     = bd$ein[nrow(bd)]
    )
    log_info(sprintf("  Batch %d: %s (%s rows)",
                     i, fn, format(nrow(bd), big.mark = ",")))
  }

  # --------------------------------------------------------------------------
  # Manifest JSON
  # --------------------------------------------------------------------------
  manifest <- list(
    pipeline           = "master",
    created_at         = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"),
    status             = "exported",
    master_source      = master_path,
    total_records      = total_records,
    geocodable_records = nrow(geocodable),
    excluded_records   = excluded,
    unique_addresses   = nrow(unique_addr),
    batch_size         = batch_size,
    num_batches        = n_batches,
    batches            = batch_details
  )
  manifest_path <- file.path(input_dir, "bmf_master_geocoder_manifest.json")
  jsonlite::write_json(manifest, manifest_path,
                       pretty = TRUE, auto_unbox = TRUE)
  log_info(sprintf("Manifest saved: %s", manifest_path))

  # --------------------------------------------------------------------------
  # Optional S3 upload of inputs + manifest + addr_lookup
  # --------------------------------------------------------------------------
  if (s3_upload) {
    s3_input_prefix <- paste0(BMF_S3_MASTER_GEOCODING_PREFIX, "input/")
    log_info(sprintf("Uploading geocoder inputs to s3://%s/%s",
                     BMF_S3_BUCKET, s3_input_prefix))
    for (fn in batch_filenames) {
      upload_to_s3(file.path(input_dir, fn),
                   paste0(s3_input_prefix, fn))
    }
    upload_to_s3(manifest_path,
                 paste0(s3_input_prefix, basename(manifest_path)))
    upload_to_s3(addr_lookup_path,
                 paste0(s3_input_prefix, basename(addr_lookup_path)))
  }

  # --------------------------------------------------------------------------
  # Operator instructions
  # --------------------------------------------------------------------------
  cat("\n=== MASTER GEOCODER INSTRUCTIONS ===\n")
  cat(sprintf("%d batch file(s) prepared in %s\n", n_batches, input_dir))
  for (i in seq_len(n_batches)) {
    cat(sprintf("  Batch %d: %s (%s rows)\n",
                i, batch_filenames[i],
                format(batch_details[[i]]$row_count, big.mark = ",")))
  }
  cat(sprintf("  Excluded: %s records with missing/invalid addresses\n",
              format(excluded, big.mark = ",")))
  cat("\nMANUAL STEPS:\n")
  cat("  1. Upload each CSV to the Urban Institute geocoder\n")
  cat("  2. Download geocoded results\n")
  cat(sprintf("  3. Save them to %s as:\n", output_dir))
  for (i in seq_len(n_batches)) {
    cat(sprintf("       %s\n", expected_outputs[i]))
  }
  cat("  4. Run: merge_master_geocoded_results()\n\n")

  invisible(list(
    n_batches    = n_batches,
    n_unique     = nrow(unique_addr),
    input_dir    = input_dir,
    addr_lookup  = addr_lookup_path,
    manifest     = manifest_path
  ))
}


#' Merge geocoded results back into bmf_master
#'
#' Reads every geocoded CSV in `<geocoding_dir>/output/`, concatenates,
#' joins lat/lon to bmf_master via the address-lookup manifest written
#' by the export step, and writes a parquet + CSV + quality report.
#'
#' Output schema: bmf_master columns (all VARCHAR) + geo_lat / geo_lon
#' (cast to DOUBLE) + the geocoder's other geo_* columns (kept VARCHAR).
#'
#' @param master_path     Path to bmf_master.parquet
#' @param geocoding_dir   Same directory passed to the export step
#' @param s3_upload       Upload merged outputs to S3 (default: TRUE)
#' @return Invisibly: list with row counts and output paths
#' @export
merge_master_geocoded_results <- function(
    master_path   = here::here("data", "master", "bmf_master.parquet"),
    geocoding_dir = here::here("data", "geocoding", "master"),
    s3_upload     = TRUE
  ) {

  input_dir   <- file.path(geocoding_dir, "input")
  output_dir  <- file.path(geocoding_dir, "output")
  merged_dir  <- file.path(geocoding_dir, "merged")
  if (!dir.exists(output_dir)) {
    stop(sprintf("Geocoder output dir not found: %s", output_dir))
  }

  # Find geocoded CSVs.
  geocoded_files <- list.files(
    output_dir,
    pattern = "^bmf_master_geocoder_batch_\\d{2}_geocoded\\.csv$",
    full.names = TRUE
  )
  if (length(geocoded_files) == 0) {
    stop(sprintf("No geocoded CSVs in %s. Did you upload + download yet?",
                 output_dir))
  }
  log_info(sprintf("Found %d geocoded batch file(s)", length(geocoded_files)))

  # Stack all geocoder outputs.
  geocoded <- data.table::rbindlist(
    lapply(geocoded_files,
           function(p) data.table::fread(p, colClasses = "character")),
    fill = TRUE
  )
  log_info(sprintf("Stacked geocoded rows: %s",
                   format(nrow(geocoded), big.mark = ",")))

  # Rename geocoder columns (Latitude -> geo_lat, etc.). Mirrors the
  # per-month merge in R/geocoding_merge.R.
  available_geo_cols <- intersect(names(GEOCODER_COLUMN_MAP), names(geocoded))
  cols_to_keep <- c("ein", available_geo_cols)
  geocoded <- geocoded[, ..cols_to_keep]
  for (old_name in available_geo_cols) {
    data.table::setnames(geocoded, old_name, GEOCODER_COLUMN_MAP[old_name])
  }

  # Cast lat/lon to numeric (the only non-VARCHAR columns we keep).
  if ("geo_lat" %in% names(geocoded)) {
    geocoded[, geo_lat := suppressWarnings(as.numeric(geo_lat))]
  }
  if ("geo_lon" %in% names(geocoded)) {
    geocoded[, geo_lon := suppressWarnings(as.numeric(geo_lon))]
  }
  geocoded[, geo_is_geocoded := !is.na(geo_lat) & !is.na(geo_lon)]

  match_n <- sum(geocoded$geo_is_geocoded)
  log_info(sprintf("Geocoder match rate (unique addresses): %s / %s (%.2f %%)",
                   format(match_n, big.mark = ","),
                   format(nrow(geocoded), big.mark = ","),
                   100 * match_n / nrow(geocoded)))

  # Load address-lookup manifest.
  addr_lookup_path <- file.path(input_dir,
                                "bmf_master_geocoder_addr_lookup.parquet")
  if (!file.exists(addr_lookup_path)) {
    stop(sprintf("Address-lookup manifest not found: %s", addr_lookup_path))
  }
  addr_lookup <- arrow::read_parquet(addr_lookup_path) |>
                  data.table::as.data.table()

  # geocoded has (representative_ein = ein, geo_*). Join to addr_lookup
  # via representative_ein to expand back to all EINs.
  data.table::setnames(geocoded, "ein", "representative_ein")
  expanded <- merge(addr_lookup, geocoded,
                     by = "representative_ein", all.x = TRUE)
  log_info(sprintf("Expanded geocoded set to %s EIN rows",
                   format(nrow(expanded), big.mark = ",")))

  # Read master and join.
  log_info(sprintf("Reading master BMF: %s", master_path))
  bmf <- arrow::read_parquet(master_path) |> data.table::as.data.table()
  data.table::setnames(expanded, "ein_all", "ein")

  # Drop org_addr_full, representative_ein from the join payload
  # (already in master / no longer needed).
  geo_cols <- setdiff(names(expanded),
                      c("ein", "org_addr_full", "representative_ein"))
  expanded <- expanded[, c("ein", geo_cols), with = FALSE]

  master_geo <- merge(bmf, expanded, by = "ein", all.x = TRUE)

  log_info(sprintf("Master+geo rows: %s | with lat/lon: %s",
                   format(nrow(master_geo), big.mark = ","),
                   format(sum(master_geo$geo_is_geocoded, na.rm = TRUE),
                          big.mark = ",")))

  # ---------------------------------------------------------------- write
  parquet_path <- file.path(merged_dir, "bmf_master_geocoded.parquet")
  csv_path     <- file.path(merged_dir, "bmf_master_geocoded.csv")
  qr_path      <- file.path(merged_dir, "bmf_master_geocoded_quality_report.json")
  dict_path    <- file.path(merged_dir, "bmf_master_geocoded_data_dictionary.csv")

  arrow::write_parquet(master_geo, parquet_path, compression = "zstd")
  log_info(sprintf("Parquet written: %s", parquet_path))
  data.table::fwrite(master_geo, csv_path)
  log_info(sprintf("CSV written:     %s", csv_path))

  log_info("Generating data dictionary")
  data_dictionary <- generate_data_dictionary(master_geo)
  data.table::fwrite(data_dictionary, dict_path)
  log_info(sprintf("Dictionary:      %s", dict_path))

  quality_report <- list(
    timestamp                = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
    total_master_rows        = nrow(bmf),
    geocodable_rows          = nrow(addr_lookup),
    unique_addresses_geocoded = nrow(geocoded),
    rows_with_lat_lon        = sum(master_geo$geo_is_geocoded, na.rm = TRUE),
    geocoder_match_rate_pct  = round(100 * match_n / nrow(geocoded), 2)
  )
  jsonlite::write_json(quality_report, qr_path,
                       pretty = TRUE, auto_unbox = TRUE)
  log_info(sprintf("Quality report:  %s", qr_path))

  if (s3_upload) {
    s3_merged_prefix <- paste0(BMF_S3_MASTER_GEOCODING_PREFIX, "merged/")
    upload_to_s3(parquet_path, paste0(s3_merged_prefix, basename(parquet_path)))
    upload_to_s3(csv_path,     paste0(s3_merged_prefix, basename(csv_path)))
    upload_to_s3(qr_path,      paste0(s3_merged_prefix, basename(qr_path)))
    upload_to_s3(dict_path,    paste0(s3_merged_prefix, basename(dict_path)))
  }

  invisible(list(
    rows         = nrow(master_geo),
    matched      = sum(master_geo$geo_is_geocoded, na.rm = TRUE),
    parquet      = parquet_path,
    csv          = csv_path,
    quality      = qr_path,
    dictionary   = dict_path
  ))
}
