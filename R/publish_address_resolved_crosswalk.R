# ============================================================================
# publish_address_resolved_crosswalk.R
#
# Publish the address-resolved crosswalk to S3. Thin wrapper over
# R/publish_crosswalk.R::publish_crosswalk().
#
# Outputs (S3) under crosswalks/address-resolved/, ADR 0042 layout:
#   v{YYYY_MM}/address_resolved_crosswalk.parquet   (permanent vintage; parquet
#                                                    only per Decision A)
#   v{YYYY_MM}/_manifest.json                        (ADR 0014)
#   latest/address_resolved_crosswalk.parquet        (stable consumer mirror)
#   latest/address_resolved_crosswalk.csv            (web catalog / spreadsheets)
#   latest/_manifest.json
#
# Per ADR 0041 §4 + ADR 0016 (consumer-composes): a SEPARATE per-EIN artifact
# consumers join to the master by `ein`; address-history columns are
# deliberately NOT added to the Unified BMF. Contract:
# nccs-contracts/contracts/address-resolved-crosswalk.yml.
#
# Run:
#   source("R/config.R"); source("R/manifest.R")
#   source("R/publish_crosswalk.R"); source("R/publish_address_resolved_crosswalk.R")
#   publish_address_resolved_crosswalk(dry_run = TRUE)   # inspect first
#   publish_address_resolved_crosswalk()                 # live write
# ============================================================================

#' Publish the address-resolved crosswalk (see `publish_crosswalk()`).
#'
#' @param crosswalk_path Local parquet (default: data/crosswalks/address_resolved_crosswalk.parquet).
#' @param s3_base_prefix Base S3 prefix; `v{vintage}/` and `latest/` are
#'                       written beneath it (ADR 0042). Must end in "/".
#' @param bucket         S3 bucket.
#' @param vintage        Build vintage tag (default: today's YYYY_MM).
#' @param dry_run        If TRUE, print the plan but touch nothing on S3.
#' @return Invisibly `list(vintage = ..., latest = ...)`, each a
#'         `publish_crosswalk()` result.
#' @export
publish_address_resolved_crosswalk <- function(
    crosswalk_path = here::here("data", "crosswalks", "address_resolved_crosswalk.parquet"),
    s3_base_prefix = "crosswalks/address-resolved/",
    bucket         = BMF_S3_BUCKET,
    vintage        = format(Sys.Date(), "%Y_%m"),
    dry_run        = FALSE) {

  # Inputs: the raw address fields from the intermediate parquets of BOTH
  # pipelines, aggregated verbatim (no cleaner dependency to pin — the raw
  # fields are vintage-invariant). Legacy street coverage requires the
  # ADR 0041 street re-publish to have landed.
  inputs <- list(
    list(uri  = sprintf("s3://%s/intermediate/bmf/", bucket),
         note = "current-pipeline intermediate parquets (all vintages): org_addr_*_raw"),
    list(uri  = sprintf("s3://%s/intermediate/bmf-legacy/", bucket),
         note = "legacy-pipeline intermediate parquets (all vintages, post-ADR-0041 street re-publish): org_addr_*_raw"),
    manifest_input_repo("R/ein.R"))   # ADR 0036: ein_prefixed/EIN2 renderings

  stopifnot(endsWith(s3_base_prefix, "/"))

  # ADR 0042: permanent parquet-only vintage folder + full latest/ mirror.
  vintage_publish_result <- publish_crosswalk(
    parquet_path = crosswalk_path,
    s3_prefix    = paste0(s3_base_prefix, "v", vintage, "/"),
    inputs = inputs, vintage = vintage,
    bucket = bucket, dry_run = dry_run, include_csv = FALSE)
  latest_publish_result <- publish_crosswalk(
    parquet_path = crosswalk_path,
    s3_prefix    = paste0(s3_base_prefix, "latest/"),
    inputs = inputs, vintage = vintage,
    bucket = bucket, dry_run = dry_run)

  invisible(list(vintage = vintage_publish_result, latest = latest_publish_result))
}
