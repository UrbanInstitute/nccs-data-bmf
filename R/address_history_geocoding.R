# ============================================================================
# address_history_geocoding.R
#
# Geocoding for the address history (nccs-contracts ADR 0051, backlog Z15).
#
# The address-resolved crosswalk lists every address an organization has had
# (one row per spell). The geocoded Unified BMF only carries coordinates for
# each organization's current address. This file sends the earlier addresses
# that have a street to the shared Urban geocoder, once per distinct address,
# and reuses what the Unified BMF already knows.
#
# It reuses the Unified BMF delta machinery in R/master_geocoding_delta.R
# unchanged for everything operational: the ledger, the submission window
# (at most three batches in flight), the run-stamped archive on S3, and the
# resume-from-ledger retrieval. A run started here uses the same runs/
# folder and the same LATEST_RUN pointer as the monthly delta, so neither
# job can start while the other has batches in flight.
#
# Three steps, mirroring the delta workflow:
#   1. prepare_address_history_geocoder_run()   distinct addresses, carryover,
#                                               batches, ledger; optional submit
#   2. retrieve_master_geocoder_delta(geocoding_dir = <address history dir>)
#                                               poll, archive, download, keep
#                                               the window full (unchanged)
#   3. read_address_history_geocodes()          one row per distinct address
#                                               with the geocoder's columns,
#                                               consumed by
#                                               scripts/build_address_geo_resolved_crosswalk.R
#
# Working directory: data/geocoding/address_history/ (gitignored), with the
# same input/ and output/ layout as the Unified BMF delta.
# ============================================================================

ADDRESS_HISTORY_GEOCODING_DIR <- here::here("data", "geocoding", "address_history")

ADDRESS_HISTORY_ADDRESS_LOOKUP_FILE <- "address_history_geocoder_addr_lookup.parquet"
ADDRESS_HISTORY_CARRYOVER_FILE      <- "address_history_geocoder_batch_00_geocoded.csv"

# The persistent address cache (ADR 0051 §7; geocoder etiquette rule 7):
# every historical address ever attempted, with its geocoder result. Kept on
# S3 so the next build sends only addresses never seen before. A working
# file of this producer, not a contracted surface.
ADDRESS_HISTORY_CACHE_KEY  <- "geocoding/address-history/cache/latest/address_geocode_cache.parquet"
ADDRESS_HISTORY_CACHE_FILE <- "address_geocode_cache.parquet"


# ---------------------------------------------------------------------------
# 1. Distinct addresses
# ---------------------------------------------------------------------------

#' One row per distinct street address in the address history.
#'
#' Spells without a street (the pre-2009 legacy records) are not geocodable
#' and are left out here; they are published with geo_match_level
#' "not_geocoded" by the build script.
#'
#' @param address_spells The address-resolved crosswalk (any rows).
#' @return Tibble with street, city, state, zip5 and f_address, the one-line
#'   form the geocoder takes, built with the same helper the Unified BMF
#'   uses so the two agree on formatting.
address_history_distinct_addresses <- function(address_spells) {

  distinct_addresses <- address_spells |>
    dplyr::filter(!is.na(street)) |>
    dplyr::distinct(street, city, state, zip5) |>
    dplyr::arrange(state, city, zip5, street)

  distinct_addresses |>
    dplyr::mutate(f_address = .build_full_address(street, city, state, zip5))
}


# ---------------------------------------------------------------------------
# 2. Carryover from the geocoded Unified BMF
# ---------------------------------------------------------------------------

#' Geocoder results the Unified BMF already holds, keyed by normalized address.
#'
#' The Unified BMF carries the raw address fields, so the same normalization
#' that defines a spell (upper case, trimmed, 5-digit ZIP) is applied to them
#' and the result is matched to the address history exactly. Where the
#' Unified BMF holds several rows for one address, the geocoded one wins.
#'
#' @param geocoded_unified The geocoded Unified BMF: raw address columns plus
#'   the geo_* columns of GEOCODER_COLUMN_MAP and org_addr_is_po_box.
#' @return Tibble, one row per normalized address, with the geo_* columns.
address_history_prior_geocodes <- function(geocoded_unified) {

  geo_columns <- intersect(unname(GEOCODER_COLUMN_MAP), names(geocoded_unified))

  geocoded_unified |>
    dplyr::transmute(
      street = address_normalize_text(org_addr_street_raw),
      city   = address_normalize_text(org_addr_city_raw),
      state  = address_normalize_text(org_addr_state_raw),
      zip5   = address_normalize_zip5(org_addr_zip_raw),
      dplyr::across(dplyr::all_of(geo_columns))
    ) |>
    dplyr::filter(!is.na(street)) |>
    dplyr::arrange(street, city, state, zip5, is.na(geo_lat)) |>
    dplyr::distinct(street, city, state, zip5, .keep_all = TRUE)
}

#' The persistent cache of historical addresses already attempted, if any.
#'
#' Downloads the cache from S3 when it exists. Returns NULL on the first
#' round, when no cache has been written yet.
#'
#' @return Tibble keyed by street, city, state, zip5 with the geo_* columns,
#'   or NULL.
address_history_read_cache <- function(geocoding_dir = ADDRESS_HISTORY_GEOCODING_DIR,
                                       bucket        = BMF_S3_BUCKET,
                                       cache_key     = ADDRESS_HISTORY_CACHE_KEY) {

  local_path <- file.path(geocoding_dir, ADDRESS_HISTORY_CACHE_FILE)
  copy_rc    <- suppressWarnings(system2(
    "aws", c("s3", "cp", paste0("s3://", bucket, "/", cache_key), local_path, "--only-show-errors"),
    stdout = FALSE, stderr = FALSE))

  if (copy_rc != 0L || !file.exists(local_path)) {
    log_info("No address cache on S3 yet; carryover comes from the Unified BMF only.")
    return(NULL)
  }

  arrow::read_parquet(local_path) |>
    tibble::as_tibble()
}

#' Split the distinct addresses into those already attempted and those to send.
#'
#' An address counts as already attempted when the Unified BMF or the cache
#' from an earlier round has a row for it, whether or not the geocoder
#' matched it (same rule as the monthly delta: failures are not retried on
#' a delta run). Where both hold the address, the cache row wins, since it
#' is the result of the most recent attempt.
#'
#' @param distinct_addresses From address_history_distinct_addresses().
#' @param prior_geocodes     From address_history_prior_geocodes() (Unified BMF).
#' @param cached_geocodes    From address_history_read_cache(); NULL on the first round.
#' @return list(carryover = addresses with prior geo_* columns,
#'              delta     = addresses to submit)
address_history_split_carryover <- function(distinct_addresses, prior_geocodes, cached_geocodes = NULL) {

  address_key <- c("street", "city", "state", "zip5")

  known_geocodes <- if (is.null(cached_geocodes)) {
    prior_geocodes
  } else {
    dplyr::bind_rows(cached_geocodes, prior_geocodes) |>
      dplyr::distinct(dplyr::across(dplyr::all_of(address_key)), .keep_all = TRUE)
  }

  carryover <- distinct_addresses |>
    dplyr::inner_join(known_geocodes, by = address_key)

  delta <- distinct_addresses |>
    dplyr::anti_join(known_geocodes, by = address_key)

  list(carryover = carryover, delta = delta)
}


# ---------------------------------------------------------------------------
# 3. Stage a run: batches, ledger, manifest, optional submission
# ---------------------------------------------------------------------------

#' Stage the address-history geocoding run and, optionally, open the window.
#'
#' Writes, under `geocoding_dir`:
#'   input/address_history_geocoder_addr_lookup.parquet   every distinct address
#'   input/address_history_geocoder_batch_NN.csv          addresses to submit
#'   input/<stem>.json                                    service form per batch
#'   input/bmf_master_geocoder_manifest.json              run manifest (the name
#'                                                        retrieve_master_geocoder_delta() reads)
#'   output/address_history_geocoder_batch_00_geocoded.csv carryover, raw geocoder column names
#'   geocode_ledger.tsv                                   mirrored to S3 runs/{run_id}/
#'
#' @param address_crosswalk_path  Local address-resolved crosswalk parquet.
#' @param geocoded_unified_path   Local geocoded Unified BMF parquet.
#' @param geocoding_dir           Working directory.
#' @param batch_size              Addresses per batch (GEOCODER_BATCH_SIZE).
#' @param urbanid                 Stem prefix for the service filenames.
#' @param email                   Notification email in the form JSON.
#' @param submit                  If TRUE, submit up to MAX_IN_FLIGHT batches now.
#' @return Invisibly list(run_id, n_distinct, n_carryover, n_delta, stems).
prepare_address_history_geocoder_run <- function(
    address_crosswalk_path = here::here("data", "crosswalks", "address_resolved_crosswalk.parquet"),
    geocoded_unified_path  = here::here("data", "geocoding", "master", "merged", "bmf_unified_geocoded.parquet"),
    geocoding_dir          = ADDRESS_HISTORY_GEOCODING_DIR,
    batch_size             = GEOCODER_BATCH_SIZE,
    urbanid                = "thiya",
    email                  = "TPoongundranar@urban.org",
    submit                 = FALSE) {

  stopifnot(file.exists(address_crosswalk_path), file.exists(geocoded_unified_path))

  run_id     <- sprintf("addrhist_%s_%s", format(Sys.time(), "%Y_%m_%d_%H%M%S"), urbanid)
  input_dir  <- file.path(geocoding_dir, "input")
  output_dir <- file.path(geocoding_dir, "output")
  purrr::walk(c(input_dir, output_dir), dir.create, recursive = TRUE, showWarnings = FALSE)

  # Same guard as the monthly delta, against the same shared run pointer.
  delta_stop_if_runs_pending(geocoding_dir)

  # Leftover outputs from an earlier run would be read as this run's.
  stale_outputs <- list.files(output_dir, pattern = "_geocoded\\.csv$", full.names = TRUE)
  if (length(stale_outputs) > 0L) {
    unlink(stale_outputs)
    log_info(sprintf("Cleared %d stale geocoded file(s) from %s", length(stale_outputs), output_dir))
  }

  # ---- distinct addresses and carryover --------------------------------------

  log_info(sprintf("Reading the address history: %s", address_crosswalk_path))
  address_spells <- arrow::read_parquet(
    address_crosswalk_path,
    col_select = c("spell_rank", "street", "city", "state", "zip5")
  )

  distinct_addresses <- address_history_distinct_addresses(address_spells)
  log_info(sprintf("Spells with a street: %s; distinct addresses: %s",
                   format(sum(!is.na(address_spells$street)), big.mark = ","),
                   format(nrow(distinct_addresses), big.mark = ",")))

  log_info(sprintf("Reading the geocoded Unified BMF: %s", geocoded_unified_path))
  geocoded_unified <- arrow::read_parquet(
    geocoded_unified_path,
    col_select = c("org_addr_street_raw", "org_addr_city_raw", "org_addr_state_raw", "org_addr_zip_raw",
                   unname(GEOCODER_COLUMN_MAP))
  )
  prior_geocodes <- address_history_prior_geocodes(geocoded_unified)
  rm(geocoded_unified)

  cached_geocodes <- address_history_read_cache(geocoding_dir)
  if (!is.null(cached_geocodes)) {
    log_info(sprintf("Address cache from an earlier round: %s addresses", format(nrow(cached_geocodes), big.mark = ",")))
  }

  split     <- address_history_split_carryover(distinct_addresses, prior_geocodes, cached_geocodes)
  carryover <- split$carryover
  delta     <- split$delta
  log_info(sprintf("Carryover (already attempted): %s | To submit: %s",
                   format(nrow(carryover), big.mark = ","),
                   format(nrow(delta), big.mark = ",")))

  arrow::write_parquet(distinct_addresses, file.path(input_dir, ADDRESS_HISTORY_ADDRESS_LOOKUP_FILE))

  # Carryover uses the RAW geocoder column names so the build script renames
  # it and the fresh service outputs with the same GEOCODER_COLUMN_MAP.
  geo_columns_present <- intersect(unname(GEOCODER_COLUMN_MAP), names(carryover))
  raw_geocoder_names  <- names(GEOCODER_COLUMN_MAP)[match(geo_columns_present, GEOCODER_COLUMN_MAP)]
  carryover_out <- carryover |>
    dplyr::select(f_address, dplyr::all_of(geo_columns_present)) |>
    dplyr::rename_with(~ raw_geocoder_names, dplyr::all_of(geo_columns_present))
  data.table::fwrite(carryover_out, file.path(output_dir, ADDRESS_HISTORY_CARRYOVER_FILE), quote = TRUE)

  # ---- batches, forms, ledger, manifest ---------------------------------------

  n_batches      <- ceiling(nrow(delta) / batch_size)
  batch_number   <- rep(seq_len(n_batches), each = batch_size, length.out = nrow(delta))
  batches        <- split(delta, batch_number)
  stem_timestamp <- as.integer(Sys.time())

  stage_one_batch <- function(batch, index) {
    batch_filename <- sprintf("address_history_geocoder_batch_%02d.csv", index)
    stem           <- sprintf("%s-%d-addrhist", urbanid, stem_timestamp + index - 1L)

    data.table::fwrite(dplyr::select(batch, f_address), file.path(input_dir, batch_filename), quote = TRUE)
    delta_write_form_json(input_dir, stem, batch_filename, email)

    # Mirror the staged inputs so a clean-checkout resume can submit them.
    batch_mirrored <- upload_to_s3(file.path(input_dir, batch_filename),
                                   paste0(delta_runs_prefix(run_id), "staged/", batch_filename))
    form_mirrored  <- upload_to_s3(file.path(input_dir, paste0(stem, ".json")),
                                   paste0(delta_runs_prefix(run_id), "staged/", stem, ".json"))
    if (!isTRUE(batch_mirrored) || !isTRUE(form_mirrored)) {
      stop(sprintf("Staged-input mirror failed for batch %02d (%s)", index, stem))
    }

    list(batch_number = index, filename = batch_filename,
         expected_output_filename = sprintf("address_history_geocoder_batch_%02d_geocoded.csv", index),
         service_stem = stem, row_count = nrow(batch))
  }

  batch_details <- purrr::imap(unname(batches), function(batch, index) stage_one_batch(batch, index))
  stems         <- purrr::map_chr(batch_details, "service_stem")

  manifest <- list(
    pipeline               = "address-history",
    mode                   = "address-history",
    run_id                 = run_id,
    created_at             = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"),
    status                 = if (submit && length(stems) > 0L) "submitting" else if (length(stems) > 0L) "staged" else "complete-no-delta",
    address_crosswalk_path = address_crosswalk_path,
    geocoded_unified_path  = geocoded_unified_path,
    spells_with_street     = sum(!is.na(address_spells$street)),
    distinct_addresses     = nrow(distinct_addresses),
    carryover_addresses    = nrow(carryover),
    cached_addresses       = if (is.null(cached_geocodes)) 0L else nrow(cached_geocodes),
    delta_addresses        = nrow(delta),
    batch_size             = batch_size,
    num_batches            = length(stems),
    max_in_flight          = MAX_IN_FLIGHT,
    batches                = batch_details
  )
  manifest_path <- file.path(input_dir, "bmf_master_geocoder_manifest.json")
  jsonlite::write_json(manifest, manifest_path, pretty = TRUE, auto_unbox = TRUE)

  if (length(stems) == 0L) {
    log_info("No new addresses; nothing to submit. The build script can run now.")
    return(invisible(list(run_id = run_id, n_distinct = nrow(distinct_addresses),
                          n_carryover = nrow(carryover), n_delta = 0L, stems = stems)))
  }

  manifest_mirrored <- upload_to_s3(manifest_path,
                                    paste0(delta_runs_prefix(run_id), "bmf_master_geocoder_manifest.json"))
  if (!isTRUE(manifest_mirrored)) stop("Run-manifest mirror upload failed.")

  ledger <- purrr::map(batch_details, function(batch) {
    data.table::data.table(
      batch_id     = sprintf("%s_%02d", run_id, batch$batch_number),
      service_stem = batch$service_stem,
      batch_file   = batch$filename,
      output_file  = batch$expected_output_filename,
      n_addresses  = as.character(batch$row_count),
      submitted_at = "", output_seen_at = "", downloaded_at = "",
      status       = "staged")
  }) |>
    data.table::rbindlist()

  delta_ledger_write(ledger, geocoding_dir, run_id)

  if (submit) {
    delta_submit_window(geocoding_dir, run_id)
  } else {
    log_info(sprintf("DRY RUN: %d batch(es) staged; retrieve_master_geocoder_delta(geocoding_dir = ...) submits and polls.",
                     length(stems)))
  }

  invisible(list(run_id = run_id, n_distinct = nrow(distinct_addresses),
                 n_carryover = nrow(carryover), n_delta = nrow(delta), stems = stems))
}


# ---------------------------------------------------------------------------
# 4. Is the run complete? (ADR 0051 Acceptance: every submitted address is
#    accounted for). Run before the results are read, so an interrupted or
#    partial retrieval can never be published as "no match".
# ---------------------------------------------------------------------------

#' Stop unless every batch was retrieved and every submitted address came back.
#'
#' @param geocoding_dir Working directory of the run.
#' @param geocodes      Output of read_address_history_geocodes(), keyed by f_address.
#' @return Invisibly a list with the counts checked.
address_history_stop_unless_complete <- function(geocoding_dir, geocodes) {

  input_dir     <- file.path(geocoding_dir, "input")
  manifest_path <- file.path(input_dir, "bmf_master_geocoder_manifest.json")
  ledger_path   <- file.path(geocoding_dir, "geocode_ledger.tsv")
  stopifnot("run manifest missing" = file.exists(manifest_path))
  manifest <- jsonlite::read_json(manifest_path)

  # Nothing was submitted: only the carryover file is expected.
  if (identical(manifest$num_batches, 0L) || length(manifest$batches) == 0L) {
    return(invisible(list(batches = 0L, submitted = 0L, missing = 0L)))
  }

  stopifnot("run ledger missing" = file.exists(ledger_path))
  ledger <- data.table::fread(ledger_path, sep = "\t", colClasses = "character")

  not_retrieved <- ledger[ledger$status != "retrieved", ]
  if (nrow(not_retrieved) > 0L) {
    stop(sprintf("Geocoder run %s is not complete: %d batch(es) not retrieved (%s). Finish retrieval before building.",
                 manifest$run_id, nrow(not_retrieved),
                 paste(sprintf("%s=%s", not_retrieved$service_stem, not_retrieved$status), collapse = ", ")))
  }

  # One-column CSV: readr handles the quoting a lone quoted column needs
  # (the addresses contain commas), which data.table's reader does not.
  read_submitted_addresses <- function(batch_filename) {
    readr::read_csv(file.path(input_dir, batch_filename), col_types = readr::cols(.default = "c"),
                    progress = FALSE)$f_address
  }
  submitted_addresses <- purrr::map(ledger$batch_file, read_submitted_addresses) |>
    purrr::list_c()

  missing_addresses <- setdiff(submitted_addresses, geocodes$f_address)
  if (length(missing_addresses) > 0L) {
    stop(sprintf("%s submitted address(es) have no row in the geocoder outputs (e.g. %s). The outputs are incomplete; do not build.",
                 format(length(missing_addresses), big.mark = ","), missing_addresses[[1]]))
  }

  invisible(list(batches = nrow(ledger), submitted = length(submitted_addresses), missing = 0L))
}


# ---------------------------------------------------------------------------
# 5. Persist the cache for the next round
# ---------------------------------------------------------------------------

#' Write every attempted address with its result to the cache, locally and on S3.
#'
#' Called by the build script once the run is known to be complete. An
#' address that was attempted and not matched is kept with empty geo_*
#' columns, so it is not resubmitted on a later round (failures ride the
#' occasional full refresh, as for the monthly delta).
#'
#' @param address_geocodes One row per distinct address: the address key
#'   columns plus the geo_* columns (NA where the geocoder returned nothing).
#' @return Invisibly the local path written.
address_history_write_cache <- function(address_geocodes,
                                        geocoding_dir = ADDRESS_HISTORY_GEOCODING_DIR,
                                        bucket        = BMF_S3_BUCKET,
                                        cache_key     = ADDRESS_HISTORY_CACHE_KEY,
                                        uploader      = upload_to_s3) {

  geo_columns <- intersect(unname(GEOCODER_COLUMN_MAP), names(address_geocodes))
  cache <- address_geocodes |>
    dplyr::select(street, city, state, zip5, dplyr::all_of(geo_columns)) |>
    dplyr::mutate(cached_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"))

  local_path <- file.path(geocoding_dir, ADDRESS_HISTORY_CACHE_FILE)
  arrow::write_parquet(cache, local_path, compression = "zstd")

  uploaded <- uploader(local_path, cache_key, bucket = bucket)
  if (!isTRUE(uploaded)) stop(sprintf("Address cache upload failed: s3://%s/%s", bucket, cache_key))
  log_info(sprintf("Address cache written: %s addresses -> s3://%s/%s", format(nrow(cache), big.mark = ","), bucket, cache_key))

  invisible(local_path)
}


# ---------------------------------------------------------------------------
# 6. Read the results back: one row per distinct address
# ---------------------------------------------------------------------------

#' Geocoder results for every distinct address, carryover and fresh alike.
#'
#' Reads every *_geocoded.csv in output/ (the carryover file and the batch
#' outputs), renames the geocoder's columns with GEOCODER_COLUMN_MAP, and
#' keeps one row per f_address. Addresses that were submitted but for which
#' no output row came back are absent here; the build script treats them as
#' "no_match".
#'
#' @param geocoding_dir Working directory of the run.
#' @return Tibble keyed by f_address with the geo_* columns.
read_address_history_geocodes <- function(geocoding_dir = ADDRESS_HISTORY_GEOCODING_DIR) {

  output_files <- list.files(file.path(geocoding_dir, "output"),
                             pattern = "_geocoded\\.csv$", full.names = TRUE)
  if (length(output_files) == 0L) stop(sprintf("No geocoded output files under %s/output", geocoding_dir))

  read_one_output <- function(path) {
    data.table::fread(path, colClasses = "character", encoding = "UTF-8") |>
      tibble::as_tibble()
  }

  geocodes <- purrr::map(output_files, read_one_output) |>
    purrr::list_rbind()

  raw_names_present <- intersect(names(GEOCODER_COLUMN_MAP), names(geocodes))

  geocodes |>
    dplyr::select(f_address, dplyr::all_of(raw_names_present)) |>
    dplyr::rename_with(~ unname(GEOCODER_COLUMN_MAP[.x]), dplyr::all_of(raw_names_present)) |>
    dplyr::mutate(
      geo_lat   = suppressWarnings(as.numeric(geo_lat)),
      geo_lon   = suppressWarnings(as.numeric(geo_lon)),
      geo_score = suppressWarnings(as.numeric(geo_score)),
      dplyr::across(dplyr::where(is.character), ~ dplyr::na_if(.x, ""))
    ) |>
    dplyr::arrange(f_address, is.na(geo_lat)) |>
    dplyr::distinct(f_address, .keep_all = TRUE)
}
