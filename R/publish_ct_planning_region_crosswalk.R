# ============================================================================
# publish_ct_planning_region_crosswalk.R
#
# Publish the Connecticut planning-region companion crosswalk to S3. Thin
# wrapper over R/publish_crosswalk.R::publish_crosswalk().
#
# Outputs (S3) under crosswalks/ct-planning-region/:
#   ct_planning_region_crosswalk.parquet   (machine contract)
#   ct_planning_region_crosswalk.csv       (web catalog / spreadsheet users)
#   _manifest.json                         (ADR 0014; records tiger_year)
#
# Self-contained from TIGER 2023 (no BMF master input), so the only manifest
# `inputs[]` descriptor is the TIGER vintage the grid was cut against.
#
# Run:
#   source("R/config.R"); source("R/manifest.R")
#   source("R/publish_crosswalk.R"); source("R/publish_ct_planning_region_crosswalk.R")
#   publish_ct_planning_region_crosswalk(dry_run = TRUE)   # inspect first
#   publish_ct_planning_region_crosswalk()                 # live write
# ============================================================================

#' Publish the CT planning-region crosswalk (see `publish_crosswalk()`).
#'
#' @param crosswalk_path Local parquet (default:
#'   data/crosswalks/ct_planning_region_crosswalk.parquet).
#' @param s3_prefix      S3 key prefix; must end in "/".
#' @param bucket         S3 bucket.
#' @param dry_run        If TRUE, print the plan but touch nothing on S3.
#' @return Invisibly `list(manifest, uploaded, skipped)`.
#' @export
publish_ct_planning_region_crosswalk <- function(
    crosswalk_path = here::here("data", "crosswalks", "ct_planning_region_crosswalk.parquet"),
    s3_prefix      = "crosswalks/ct-planning-region/",
    bucket         = BMF_S3_BUCKET,
    dry_run        = FALSE) {

  df <- arrow::read_parquet(crosswalk_path)
  tiger_year <- unique(df$tiger_year); tiger_year <- tiger_year[!is.na(tiger_year)][1]

  inputs <- list(list(
    uri  = sprintf("https://www2.census.gov/geo/tiger/GENZ%s/shp/cb_%s_us_county_500k.zip",
                   tiger_year, tiger_year),
    note = sprintf("TIGER counties cb=TRUE year=%s, CT planning regions (STATEFP 09)",
                   tiger_year)))

  publish_crosswalk(parquet_path = crosswalk_path, s3_prefix = s3_prefix,
                    inputs = inputs, vintage = as.character(tiger_year),
                    bucket = bucket, dry_run = dry_run)
}
