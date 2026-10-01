# ============================================================================
# publish_address_geo_resolved_crosswalk.R
#
# Publishes the address-geo-resolved crosswalk (nccs-contracts ADR 0051)
# built by scripts/build_address_geo_resolved_crosswalk.R, in the ADR 0042
# layout:
#
#   s3://nccsdata/crosswalks/address-geo-resolved/v{YYYY_MM}/   parquet + manifest (retained)
#   s3://nccsdata/crosswalks/address-geo-resolved/latest/       parquet + CSV + data dictionary
#                                                               + 10,000-row sample + manifest
#
# Thin wrapper over R/publish_crosswalk.R (idempotent, manifest-driven). The
# manifest records the sha256 of the address-resolved crosswalk the table was
# built from, so anyone can check which address history it pairs with.
#
# Run:
#   source("R/config.R"); source("R/manifest.R"); source("R/publish_crosswalk.R")
#   source("R/publish_address_geo_resolved_crosswalk.R")
#   publish_address_geo_resolved_crosswalk(dry_run = TRUE)   # inspect first
#   publish_address_geo_resolved_crosswalk()                 # live write
# ============================================================================

if (!exists("publish_crosswalk")) source(here::here("R", "publish_crosswalk.R"))

#' Publish the address-geo-resolved crosswalk to its vintage folder and latest/.
#'
#' @param crosswalk_path         Character: path of the parquet file on this
#'   machine, as written by scripts/build_address_geo_resolved_crosswalk.R.
#'   It is read locally and not from S3, because this function is the step
#'   that puts it on S3. The CSV, the data dictionary, the sample and the
#'   build summary are expected beside it under the same file stem.
#' @param address_crosswalk_path Character: path on this machine of the
#'   address-resolved crosswalk the build read; its sha256 goes into the
#'   manifest. It is also read locally and not from S3, so the recorded
#'   sha256 is that of the exact file the table was built from. The copy on
#'   S3 may have been replaced by a newer build since.
#' @param s3_root                Character: key prefix ending in "/" under
#'   which v{vintage}/ and latest/ sit.
#' @param bucket                 Character: S3 bucket.
#' @param vintage                Character: build vintage tag (default: this
#'   month's YYYY_MM).
#' @param dry_run                Logical: if TRUE, print the plan and touch
#'   nothing on S3.
#' @param uploader               Function(local_file, s3_key, bucket) returning
#'   TRUE on success (default upload_to_s3); every upload is checked.
#' @return Invisibly `list(vintage, vintage_result, latest_result)`.
#' @export
publish_address_geo_resolved_crosswalk <- function(
    crosswalk_path         = here::here("data", "crosswalks", "address_geo_resolved_crosswalk.parquet"),
    address_crosswalk_path = here::here("data", "crosswalks", "address_resolved_crosswalk.parquet"),
    s3_root                = "crosswalks/address-geo-resolved/",
    bucket                 = BMF_S3_BUCKET,
    vintage                = format(Sys.Date(), "%Y_%m"),
    dry_run                = FALSE,
    uploader               = upload_to_s3) {

  stopifnot(
    "s3_root must end in a slash"                                     = endsWith(s3_root, "/"),
    "address-geo-resolved crosswalk not found at crosswalk_path"      = file.exists(crosswalk_path),
    "address-resolved crosswalk not found at address_crosswalk_path"  = file.exists(address_crosswalk_path)
  )

  stem            <- stringr::str_remove(crosswalk_path, "\\.parquet$")
  summary_path    <- paste0(stem, "_summary.json")
  dictionary_path <- paste0(stem, "_data_dictionary.csv")
  sample_path     <- paste0(stem, "_sample.csv")

  build_summary <- if (file.exists(summary_path)) jsonlite::fromJSON(summary_path) else list()

  tiger_note <- sprintf(
    "TIGER/Line via tigris: blocks %s and %s, ZCTA %s, congressional districts TIGER %s (Congress %s)",
    build_summary$tiger$blocks_2020             %||% 2020,
    build_summary$tiger$blocks_2010             %||% 2010,
    build_summary$tiger$zcta                    %||% 2020,
    build_summary$tiger$congressional_districts %||% 2024,
    build_summary$tiger$congress_session        %||% 119
  )

  inputs <- list(
    list(
      uri    = sprintf("s3://%s/crosswalks/address-resolved/latest/address_resolved_crosswalk.parquet", bucket),
      sha256 = digest::digest(address_crosswalk_path, algo = "sha256", file = TRUE),
      note   = "address-resolved crosswalk this table was built from (join on spell_id); sha256 of the local build file"
    ),
    list(
      uri  = sprintf("s3://%s/geocoding/unified-bmf/latest/bmf_unified_geocoded.parquet", bucket),
      note = "geocoded Unified BMF: geocoder results carried over for addresses it already holds"
    ),
    list(
      uri  = sprintf("s3://%s/crosswalks/census-geo-resolved/latest/census_geo_resolved_crosswalk.parquet", bucket),
      note = "census-geo-resolved crosswalk, compared against for current addresses"
    ),
    list(
      uri  = "https://www2.census.gov/geo/tiger/",
      note = tiger_note
    ),
    manifest_input_repo("R/census_geo_resolved.R"),
    manifest_input_repo("R/census_geo_assign.R"),
    manifest_input_repo("R/address_history_geocoding.R"),
    manifest_input_repo("R/address_spell_id.R"),
    manifest_input_repo("R/ein.R")
  )

  vintage_prefix <- paste0(s3_root, "v", vintage, "/")
  latest_prefix  <- paste0(s3_root, "latest/")

  vintage_result <- publish_crosswalk(
    parquet_path = crosswalk_path,
    s3_prefix    = vintage_prefix,
    inputs       = inputs,
    vintage      = vintage,
    bucket       = bucket,
    dry_run      = dry_run,
    include_csv  = FALSE,
    uploader     = uploader
  )

  latest_result <- publish_crosswalk(
    parquet_path = crosswalk_path,
    s3_prefix    = latest_prefix,
    inputs       = inputs,
    vintage      = vintage,
    bucket       = bucket,
    dry_run      = dry_run,
    include_csv  = TRUE,
    uploader     = uploader
  )

  # The data dictionary and the reviewer sample are supporting files: they
  # describe the table and are not the table. They are uploaded to latest/
  # only, next to the parquet and CSV, and are not listed in the manifest.
  # A supporting file that was not built is skipped.
  upload_supporting_file <- function(local_path) {

    if (!file.exists(local_path)) {

      return(invisible(NULL))

    }

    s3_key <- paste0(latest_prefix, basename(local_path))

    if (dry_run) {

      message(sprintf("  PUT  %s", s3_key))

      return(invisible(s3_key))

    }

    uploaded <- uploader(local_path, s3_key, bucket = bucket)

    if (!isTRUE(uploaded)) {

      stop(sprintf("upload failed for s3://%s/%s", bucket, s3_key))

    }

    return(invisible(s3_key))

  }

  purrr::walk(c(dictionary_path, sample_path), upload_supporting_file)

  return(invisible(list(
    vintage        = vintage,
    vintage_result = vintage_result,
    latest_result  = latest_result
  )))

}
