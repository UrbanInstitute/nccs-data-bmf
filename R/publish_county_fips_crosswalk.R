# ============================================================================
# publish_county_fips_crosswalk.R
#
# Publish the county FIPS crosswalk to S3 as a stable contract for downstream
# consumers (sector-in-brief-data is the first; they left_join it, no
# sf/tigris on their side). Reuses the ADR 0014 _manifest.json reference
# implementation in R/manifest.R.
#
# Outputs (S3):
#   {s3_prefix}county_fips_crosswalk.parquet   (machine contract)
#   {s3_prefix}county_fips_crosswalk.csv       (web catalog / spreadsheet users)
#   {s3_prefix}_manifest.json   (records tiger_year + source master etag)
#
# Idempotency: sha256 of the parquet is compared to the existing remote
# _manifest.json; unchanged -> skip. All S3 traffic via aws.s3::put_object
# (upload_to_s3), consistent with the rest of this repo. No aws s3 sync.
#
# Run:
#   source("R/config.R")                 # for BMF_S3_BUCKET + upload_to_s3
#   source("R/manifest.R")
#   source("R/publish_county_fips_crosswalk.R")
#   publish_county_fips_crosswalk(dry_run = TRUE)   # inspect the plan first
#   publish_county_fips_crosswalk()                 # live write
# ============================================================================

#' Publish the county FIPS crosswalk parquet + ADR 0014 manifest to S3.
#'
#' @param crosswalk_path Local parquet (default: data/crosswalks/county_fips_crosswalk.parquet).
#' @param s3_prefix      S3 key prefix; must end in "/".
#' @param bucket         S3 bucket.
#' @param source_master_etag Optional S3 etag of the geocoded master the points
#'   were derived from, recorded in manifest inputs[]. NULL omits it.
#' @param dry_run        If TRUE, build the manifest locally and print the plan
#'   but touch nothing on S3.
#' @return Invisibly list(manifest, uploaded, skipped).
#' @export
publish_county_fips_crosswalk <- function(
    crosswalk_path     = here::here("data", "crosswalks", "county_fips_crosswalk.parquet"),
    s3_prefix          = "crosswalks/county-fips/",
    bucket             = BMF_S3_BUCKET,
    source_master_etag = NULL,
    dry_run            = FALSE) {

  csv_path <- sub("\\.parquet$", ".csv", crosswalk_path)
  stopifnot(file.exists(crosswalk_path), file.exists(csv_path), endsWith(s3_prefix, "/"))
  if (!requireNamespace("digest",   quietly = TRUE)) stop("Package 'digest' required.")
  if (!requireNamespace("jsonlite", quietly = TRUE)) stop("Package 'jsonlite' required.")

  df  <- arrow::read_parquet(crosswalk_path)
  out_dir <- dirname(crosswalk_path)

  tiger_year <- tryCatch(unique(df$tiger_year), error = function(e) NA)
  tiger_year <- tiger_year[!is.na(tiger_year)][1]

  # Both formats are published; parquet is the machine contract, CSV is the
  # human/web-catalog mirror.
  outputs <- list(
    list(path = crosswalk_path, row_count = nrow(df), columns = names(df)),
    list(path = csv_path,       row_count = nrow(df), columns = names(df)))

  inputs <- list(list(
    uri = "s3://nccsdata/geocoding/bmf-master/merged/bmf_master_geocoded.parquet",
    etag = source_master_etag %||% NA_character_,
    note = sprintf("TIGER counties cb=TRUE year=%s", tiger_year)
  ))
  inputs[[1]] <- inputs[[1]][!vapply(inputs[[1]], function(x) length(x) == 0 || is.na(x), logical(1))]

  mw <- write_manifest(vintage = as.character(tiger_year), out_dir = out_dir,
                       outputs = outputs, inputs = inputs)
  manifest      <- mw$manifest
  manifest_path <- mw$path

  files <- c(basename(crosswalk_path), basename(csv_path))
  shas  <- vapply(files, function(f) manifest$files[[f]]$sha256, character(1))

  message(sprintf("Built manifest for %d rows (tiger_year=%s): %s",
                  nrow(df), tiger_year, paste(files, collapse = ", ")))

  remote <- read_existing_manifest(paste0(s3_prefix, "_manifest.json"), bucket)

  put_one <- function(local, key, dry) {
    if (manifest_unchanged(remote, key, shas[[key]])) {
      message(sprintf("SKIP (unchanged): s3://%s/%s%s", bucket, s3_prefix, key)); return("skip")
    }
    if (dry) message(sprintf("  PUT  %s%s", s3_prefix, key))
    else     upload_to_s3(local, paste0(s3_prefix, key), bucket = bucket)
    "put"
  }

  if (dry_run) {
    message("DRY RUN: target s3://", bucket, "/", s3_prefix)
    invisible(Map(function(l, k) put_one(l, k, TRUE),
                  c(crosswalk_path, csv_path), files))
    message(sprintf("  PUT  %s_manifest.json", s3_prefix))
    return(invisible(list(manifest = manifest, uploaded = character(), skipped = character())))
  }

  acts <- Map(function(l, k) put_one(l, k, FALSE), c(crosswalk_path, csv_path), files)
  upload_to_s3(manifest_path, paste0(s3_prefix, "_manifest.json"), bucket = bucket)

  uploaded <- files[unlist(acts) == "put"]; skipped <- files[unlist(acts) == "skip"]
  message(sprintf("Publish complete: %d uploaded, %d skipped",
                  length(uploaded), length(skipped)))
  invisible(list(manifest = manifest, uploaded = uploaded, skipped = skipped))
}
