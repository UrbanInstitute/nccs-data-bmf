# ============================================================================
# publish_cbsa_crosswalk.R
#
# Publish the county -> CBSA crosswalk to S3. Thin wrapper over
# R/publish_crosswalk.R::publish_crosswalk().
#
# Outputs (S3) under crosswalks/cbsa/:
#   cbsa_crosswalk.parquet   (machine contract)
#   cbsa_crosswalk.csv       (web catalog / spreadsheet users)
#   _manifest.json           (ADR 0014; records the OMB delineation + county xwalk)
#
# Run:
#   source("R/config.R"); source("R/manifest.R")
#   source("R/publish_crosswalk.R"); source("R/publish_cbsa_crosswalk.R")
#   publish_cbsa_crosswalk(dry_run = TRUE)   # inspect first
#   publish_cbsa_crosswalk()                 # live write
# ============================================================================

#' Publish the CBSA crosswalk (see `publish_crosswalk()`).
#'
#' @param crosswalk_path Local parquet (default: data/crosswalks/cbsa_crosswalk.parquet).
#' @param s3_prefix      S3 key prefix; must end in "/".
#' @param bucket         S3 bucket.
#' @param dry_run        If TRUE, print the plan but touch nothing on S3.
#' @return Invisibly `list(manifest, uploaded, skipped)`.
#' @export
publish_cbsa_crosswalk <- function(
    crosswalk_path = here::here("data", "crosswalks", "cbsa_crosswalk.parquet"),
    s3_prefix      = "crosswalks/cbsa/",
    bucket         = BMF_S3_BUCKET,
    dry_run        = FALSE) {

  df <- arrow::read_parquet(crosswalk_path)
  year <- unique(df$delineation_year); year <- year[!is.na(year)][1]

  inputs <- list(
    manifest_input_repo("data/crosswalks/county_fips_crosswalk.parquet"),
    list(uri = sprintf(paste0("https://www2.census.gov/programs-surveys/metro-micro/",
                              "geographies/reference-files/%s/delineation-files/list1_%s.xlsx"),
                       year, year),
         note = sprintf("OMB %s CBSA delineation (List 1)", year)))

  publish_crosswalk(parquet_path = crosswalk_path, s3_prefix = s3_prefix,
                    inputs = inputs, vintage = as.character(year),
                    bucket = bucket, dry_run = dry_run)
}
