# ============================================================================
# publish_crosswalk.R
#
# Generic publisher for a crosswalk artifact (parquet + CSV mirror + ADR 0014
# _manifest.json) to S3, used by both the county FIPS crosswalk and the CBSA
# crosswalk (see R/publish_county_fips_crosswalk.R and
# R/publish_cbsa_crosswalk.R, which are thin wrappers supplying their own
# s3_prefix / inputs / vintage).
#
# Parquet is the machine contract; the CSV sibling is the human / web-catalog
# mirror. Idempotency is manifest-driven: each file's sha256 is compared to the
# existing remote _manifest.json and unchanged files are skipped. All S3
# traffic goes through aws.s3::put_object (upload_to_s3); no aws s3 sync.
# ============================================================================

#' Publish a crosswalk parquet + its CSV sibling + an ADR 0014 manifest to S3.
#'
#' @param parquet_path Local `.parquet` path; its `.csv` sibling must also exist.
#' @param s3_prefix    S3 key prefix, must end in "/".
#' @param inputs       List of manifest `inputs[]` descriptors (see R/manifest.R).
#' @param vintage      Vintage tag recorded in the manifest (e.g. "2023").
#' @param bucket       S3 bucket.
#' @param dry_run      If TRUE, build the manifest and print the upload plan
#'                     (one S3 HEAD for the existing manifest) but write nothing.
#' @param include_csv  If FALSE, publish the parquet + manifest only (ADR 0042
#'                     Decision A: vintage folders retain parquet only).
#' @return Invisibly `list(manifest, uploaded, skipped)`.
#' @export
publish_crosswalk <- function(parquet_path, s3_prefix, inputs = list(),
                              vintage, bucket = BMF_S3_BUCKET, dry_run = FALSE,
                              include_csv = TRUE) {
  csv_path <- sub("\\.parquet$", ".csv", parquet_path)
  stopifnot(file.exists(parquet_path), endsWith(s3_prefix, "/"))
  if (include_csv) stopifnot(file.exists(csv_path))
  if (!requireNamespace("digest",   quietly = TRUE)) stop("Package 'digest' required.")
  if (!requireNamespace("jsonlite", quietly = TRUE)) stop("Package 'jsonlite' required.")

  df      <- arrow::read_parquet(parquet_path)
  out_dir <- dirname(parquet_path)
  outputs <- list(
    list(path = parquet_path, row_count = nrow(df), columns = names(df)))
  if (include_csv) {
    outputs <- c(outputs,
      list(list(path = csv_path, row_count = nrow(df), columns = names(df))))
  }
  local_paths <- vapply(outputs, `[[`, character(1), "path")

  mw   <- write_manifest(vintage = vintage, out_dir = out_dir,
                         outputs = outputs, inputs = inputs)
  manifest      <- mw$manifest
  manifest_path <- mw$path
  files <- basename(local_paths)
  shas  <- vapply(files, function(f) manifest$files[[f]]$sha256, character(1))

  message(sprintf("Built manifest for %d rows (vintage=%s): %s",
                  nrow(df), vintage, paste(files, collapse = ", ")))

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
    Map(function(l, k) put_one(l, k, TRUE), local_paths, files)
    message(sprintf("  PUT  %s_manifest.json", s3_prefix))
    return(invisible(list(manifest = manifest, uploaded = character(), skipped = character())))
  }

  acts <- Map(function(l, k) put_one(l, k, FALSE), local_paths, files)
  upload_to_s3(manifest_path, paste0(s3_prefix, "_manifest.json"), bucket = bucket)

  uploaded <- files[unlist(acts) == "put"]; skipped <- files[unlist(acts) == "skip"]
  message(sprintf("Publish complete: %d uploaded, %d skipped",
                  length(uploaded), length(skipped)))
  invisible(list(manifest = manifest, uploaded = uploaded, skipped = skipped))
}
