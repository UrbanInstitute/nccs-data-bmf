# ============================================================================
# publish_county_fips_crosswalk.R
#
# Publish the county FIPS crosswalk to S3 (a stable contract for downstream
# consumers; sector-in-brief-data left_joins it, no sf/tigris on their side).
# Thin wrapper over R/publish_crosswalk.R::publish_crosswalk().
#
# Outputs (S3) under crosswalks/county-fips/:
#   county_fips_crosswalk.parquet   (machine contract)
#   county_fips_crosswalk.csv       (web catalog / spreadsheet users)
#   _manifest.json                  (ADR 0014; records tiger_year + source master)
#
# Run:
#   source("R/config.R"); source("R/manifest.R")
#   source("R/publish_crosswalk.R"); source("R/publish_county_fips_crosswalk.R")
#   publish_county_fips_crosswalk(dry_run = TRUE)   # inspect first
#   publish_county_fips_crosswalk()                 # live write
# ============================================================================

#' Publish the county FIPS crosswalk (see `publish_crosswalk()`).
#'
#' @param crosswalk_path Local parquet (default: data/crosswalks/county_fips_crosswalk.parquet).
#' @param s3_prefix      S3 key prefix; must end in "/".
#' @param bucket         S3 bucket.
#' @param source_master_etag Optional S3 etag of the geocoded master the points
#'   were derived from, recorded in the manifest `inputs[]`.
#' @param dry_run        If TRUE, print the plan but touch nothing on S3.
#' @return Invisibly `list(manifest, uploaded, skipped)`.
#' @export
publish_county_fips_crosswalk <- function(
    crosswalk_path     = here::here("data", "crosswalks", "county_fips_crosswalk.parquet"),
    s3_prefix          = "crosswalks/county-fips/",
    bucket             = BMF_S3_BUCKET,
    source_master_etag = NULL,
    dry_run            = FALSE) {

  df <- arrow::read_parquet(crosswalk_path)
  tiger_year <- unique(df$tiger_year); tiger_year <- tiger_year[!is.na(tiger_year)][1]

  master_input <- list(
    uri  = "s3://nccsdata/geocoding/bmf-master/merged/bmf_master_geocoded.parquet",
    etag = source_master_etag %||% NA_character_,
    note = sprintf("TIGER counties cb=TRUE year=%s", tiger_year))
  master_input <- master_input[!vapply(master_input,
                                       function(x) length(x) == 0 || is.na(x), logical(1))]

  publish_crosswalk(parquet_path = crosswalk_path, s3_prefix = s3_prefix,
                    inputs = list(master_input), vintage = as.character(tiger_year),
                    bucket = bucket, dry_run = dry_run)
}
