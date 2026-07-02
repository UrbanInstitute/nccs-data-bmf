# ============================================================================
# publish_ntee_resolved_crosswalk.R
#
# Publish the NTEE-resolved crosswalk to S3. Thin wrapper over
# R/publish_crosswalk.R::publish_crosswalk().
#
# Outputs (S3) under crosswalks/ntee-resolved/:
#   ntee_resolved_crosswalk.parquet   (machine contract)
#   ntee_resolved_crosswalk.csv       (web catalog / spreadsheet users)
#   _manifest.json                    (ADR 0014)
#
# Per ADR 0034 + ADR 0016 (consumer-composes): this is a SEPARATE per-EIN
# artifact consumers join to the master by `ein`; NTEE-resolution columns are
# deliberately NOT added to the Master BMF.
#
# Run:
#   source("R/config.R"); source("R/manifest.R")
#   source("R/publish_crosswalk.R"); source("R/publish_ntee_resolved_crosswalk.R")
#   publish_ntee_resolved_crosswalk(dry_run = TRUE)   # inspect first
#   publish_ntee_resolved_crosswalk()                 # live write
# ============================================================================

#' Publish the NTEE-resolved crosswalk (see `publish_crosswalk()`).
#'
#' @param crosswalk_path Local parquet (default: data/crosswalks/ntee_resolved_crosswalk.parquet).
#' @param s3_prefix      S3 key prefix; must end in "/".
#' @param bucket         S3 bucket.
#' @param vintage        Build vintage tag (default: today's YYYY_MM).
#' @param dry_run        If TRUE, print the plan but touch nothing on S3.
#' @return Invisibly `list(manifest, uploaded, skipped)`.
#' @export
publish_ntee_resolved_crosswalk <- function(
    crosswalk_path = here::here("data", "crosswalks", "ntee_resolved_crosswalk.parquet"),
    s3_prefix      = "crosswalks/ntee-resolved/",
    bucket         = BMF_S3_BUCKET,
    vintage        = format(Sys.Date(), "%Y_%m"),
    dry_run        = FALSE) {

  # Inputs: the crosswalk is derived from the intermediate parquets of BOTH
  # pipelines (raw NTEE across all vintages) cleaned by transform_ntee_code.
  inputs <- list(
    list(uri  = sprintf("s3://%s/intermediate/bmf/", bucket),
         note = "current-pipeline intermediate parquets (all vintages): ntee_code_raw"),
    list(uri  = sprintf("s3://%s/intermediate/bmf-legacy/", bucket),
         note = "legacy-pipeline intermediate parquets (all vintages): ntee_code_raw"),
    manifest_input_repo("R/transform_ntee_code.R"),
    manifest_input_repo("R/ein.R"),   # ADR 0036: ein_prefixed/EIN2 renderings
    manifest_input_repo("data/lookup/ntee_legacy_5char_lookup.csv"))

  publish_crosswalk(parquet_path = crosswalk_path, s3_prefix = s3_prefix,
                    inputs = inputs, vintage = vintage,
                    bucket = bucket, dry_run = dry_run)
}
