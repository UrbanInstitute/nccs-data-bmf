# ============================================================================
# publish_census_geo_resolved_crosswalk.R
#
# Publishes the census-geo-resolved crosswalk (nccs-contracts ADR 0045) built
# by scripts/build_census_geo_resolved_crosswalk.R, following the ADR 0042
# layout for rolling artifacts:
#
#   s3://nccsdata/crosswalks/census-geo-resolved/v{YYYY_MM}/   parquet + manifest (retained)
#   s3://nccsdata/crosswalks/census-geo-resolved/latest/       parquet + CSV + data dictionary + manifest
#
# Thin wrapper over R/publish_crosswalk.R (idempotent, manifest-driven). The
# data dictionary is uploaded alongside latest/ so the web catalog can link it.
# ============================================================================

if (!exists("publish_crosswalk")) source(here::here("R", "publish_crosswalk.R"))

#' Publish the census-geo-resolved crosswalk to its vintage folder and latest/
#'
#' @param crosswalk_path Local parquet path (CSV and data dictionary siblings expected).
#' @param s3_root        Key prefix ending in "/" under which v{vintage}/ and latest/ sit.
#' @param bucket         S3 bucket.
#' @param vintage        Build vintage tag; defaults to the source Unified BMF vintage
#'                       recorded in the build summary, else today's YYYY_MM.
#' @param dry_run        If TRUE, print the plan and touch nothing on S3.
#' @param uploader       Upload function (default upload_to_s3); every upload is checked.
#' @return Invisibly `list(vintage, vintage_result, latest_result)`.
#' @export
publish_census_geo_resolved_crosswalk <- function(
    crosswalk_path = here::here("data", "crosswalks", "census_geo_resolved_crosswalk.parquet"),
    s3_root        = "crosswalks/census-geo-resolved/",
    bucket         = BMF_S3_BUCKET,
    vintage        = NULL,
    dry_run        = FALSE,
    uploader       = upload_to_s3) {

  stopifnot(endsWith(s3_root, "/"), file.exists(crosswalk_path))
  stem            <- sub("\\.parquet$", "", crosswalk_path)
  summary_path    <- paste0(stem, "_summary.json")
  dictionary_path <- paste0(stem, "_data_dictionary.csv")
  build_summary   <- if (file.exists(summary_path)) jsonlite::fromJSON(summary_path) else list()

  if (is.null(vintage)) {
    vintage <- if (!is.null(build_summary$source_vintage)) build_summary$source_vintage else format(Sys.Date(), "%Y_%m")
  }

  inputs <- list(
    list(uri  = sprintf("s3://%s/geocoding/unified-bmf/latest/bmf_unified_geocoded.parquet", bucket),
         note = sprintf("geocoded Unified BMF, vintage %s: geo_lat/geo_lon/geo_addr_type", vintage)),
    list(uri  = sprintf("s3://%s/crosswalks/county-fips/county_fips_crosswalk.parquet", bucket),
         note = "county-fips crosswalk, used only for the county-consistency gate"),
    list(uri  = "https://www2.census.gov/geo/tiger/",
         note = sprintf("TIGER/Line via tigris: blocks %s and %s, ZCTA %s, congressional districts TIGER %s (Congress %s)",
                        build_summary$tiger$blocks_2020 %||% 2020, build_summary$tiger$blocks_2010 %||% 2010,
                        build_summary$tiger$zcta %||% 2020, build_summary$tiger$congressional_districts %||% 2024,
                        build_summary$tiger$congress_session %||% 119)),
    manifest_input_repo("R/census_geo_resolved.R"),
    manifest_input_repo("R/ein.R")
  )

  vintage_prefix <- paste0(s3_root, "v", vintage, "/")
  latest_prefix  <- paste0(s3_root, "latest/")

  vintage_result <- publish_crosswalk(parquet_path = crosswalk_path, s3_prefix = vintage_prefix,
                                      inputs = inputs, vintage = vintage, bucket = bucket,
                                      dry_run = dry_run, include_csv = FALSE, uploader = uploader)
  latest_result  <- publish_crosswalk(parquet_path = crosswalk_path, s3_prefix = latest_prefix,
                                      inputs = inputs, vintage = vintage, bucket = bucket,
                                      dry_run = dry_run, include_csv = TRUE, uploader = uploader)

  if (file.exists(dictionary_path)) {
    dictionary_key <- paste0(latest_prefix, basename(dictionary_path))
    if (dry_run) {
      message(sprintf("  PUT  %s", dictionary_key))
    } else {
      dictionary_ok <- uploader(dictionary_path, dictionary_key, bucket = bucket)
      if (!isTRUE(dictionary_ok)) stop(sprintf("data dictionary upload failed for s3://%s/%s", bucket, dictionary_key))
    }
  }

  invisible(list(vintage = vintage, vintage_result = vintage_result, latest_result = latest_result))
}
