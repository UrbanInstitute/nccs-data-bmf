# ============================================================================
# publish_lookups.R
#
# Publish the canonical BMF lookup tables to S3 as a stable contract for
# downstream consumers (notably the `nccsdata` R package).
#
# Source of truth: lookup_ls in R/config.R, which is itself built from
#   data/lookup/bmf_code_lookup.xlsx (all sheets) +
#   data/lookup/ntee_legacy_5char_lookup.csv
#
# Output:
#   s3://nccsdata/lookups/bmf/{YYYY_MM}/{name}.csv
#   s3://nccsdata/lookups/bmf/{YYYY_MM}/MANIFEST.json
#   s3://nccsdata/lookups/bmf/latest/{name}.csv   (mirror of most-recent vintage)
#   s3://nccsdata/lookups/bmf/latest/MANIFEST.json
#
# Idempotency: each file's sha256 is recorded in MANIFEST.json. On re-run,
# the existing remote manifest is fetched; any file whose hash is unchanged
# is skipped. The convention `aws s3 sync` referenced elsewhere in the
# repo is not used here because all existing S3 traffic in this codebase
# goes through aws.s3::put_object — staying consistent with that pattern.
# ============================================================================

#' Publish BMF lookup tables to S3
#'
#' Writes each entry of `lookup_ls` to a CSV under `lookup_dir`, builds a
#' MANIFEST.json (file name, row count, columns, sha256), and uploads to
#' s3://{bucket}/{s3_prefix}{vintage}/. Then mirrors the same files to
#' s3://{bucket}/{s3_prefix}latest/.
#'
#' @param lookups     Named list of data.tables/data.frames (default: lookup_ls).
#' @param vintage     YYYY_MM stamp for this publication (default: current month).
#' @param lookup_dir  Local staging directory for CSV writes.
#' @param s3_prefix   S3 key prefix; must end in "/".
#' @param bucket      S3 bucket name.
#' @param dry_run     If TRUE, write CSVs locally and print the upload plan
#'                    but do not touch S3.
#'
#' @return Invisibly: a list with `manifest`, `vintage`, `uploaded`, `skipped`.
#'
#' @export
publish_bmf_lookups <- function(lookups    = lookup_ls,
                                vintage    = format(Sys.Date(), "%Y_%m"),
                                lookup_dir = here::here("data", "lookup", "published"),
                                s3_prefix  = "lookups/bmf/",
                                bucket     = BMF_S3_BUCKET,
                                dry_run    = FALSE) {

  stopifnot(is.list(lookups), length(lookups) > 0)
  if (!requireNamespace("digest",   quietly = TRUE)) stop("Package 'digest' required.")
  if (!requireNamespace("jsonlite", quietly = TRUE)) stop("Package 'jsonlite' required.")

  out_dir <- file.path(lookup_dir, vintage)
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

  # --- Write CSVs and build manifest entries ---------------------------------
  manifest_entries <- vector("list", length(lookups))
  names(manifest_entries) <- names(lookups)

  for (nm in names(lookups)) {
    df <- lookups[[nm]]
    csv_path <- file.path(out_dir, paste0(nm, ".csv"))
    data.table::fwrite(df, csv_path)
    manifest_entries[[nm]] <- list(
      file     = paste0(nm, ".csv"),
      rows     = as.integer(nrow(df)),
      cols     = as.integer(ncol(df)),
      columns  = as.list(names(df)),
      sha256   = digest::digest(file = csv_path, algo = "sha256"),
      bytes    = as.integer(file.size(csv_path))
    )
  }

  manifest <- list(
    vintage      = vintage,
    generated_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    source       = list(
      workbook   = "data/lookup/bmf_code_lookup.xlsx",
      extra_csv  = "data/lookup/ntee_legacy_5char_lookup.csv"
    ),
    files        = manifest_entries
  )
  manifest_path <- file.path(out_dir, "MANIFEST.json")
  jsonlite::write_json(manifest, manifest_path, pretty = TRUE, auto_unbox = TRUE)

  message(sprintf("Wrote %d lookup CSVs + MANIFEST.json to %s",
                  length(lookups), out_dir))

  # --- Fetch existing remote manifest for skip decisions ---------------------
  vintage_prefix <- paste0(s3_prefix, vintage, "/")
  latest_prefix  <- paste0(s3_prefix, "latest/")
  remote_manifest <- .read_remote_manifest(
    paste0(vintage_prefix, "MANIFEST.json"), bucket
  )

  uploaded <- character()
  skipped  <- character()

  if (dry_run) {
    message("DRY RUN: would upload to s3://", bucket, "/", vintage_prefix)
    for (nm in names(lookups)) {
      action <- if (.hash_unchanged(remote_manifest, nm, manifest_entries[[nm]]$sha256)) {
        skipped  <- c(skipped, nm); "SKIP"
      } else {
        uploaded <- c(uploaded, nm); "PUT "
      }
      message(sprintf("  %s %s%s.csv", action, vintage_prefix, nm))
    }
    message(sprintf("  PUT  %sMANIFEST.json", vintage_prefix))
    message("DRY RUN: latest/ mirror would copy the same files.")
    return(invisible(list(manifest = manifest, vintage = vintage,
                          uploaded = uploaded, skipped = skipped)))
  }

  # --- Upload to vintage prefix ----------------------------------------------
  for (nm in names(lookups)) {
    csv_local <- file.path(out_dir, paste0(nm, ".csv"))
    csv_key   <- paste0(vintage_prefix, nm, ".csv")
    if (.hash_unchanged(remote_manifest, nm, manifest_entries[[nm]]$sha256)) {
      message(sprintf("SKIP (unchanged): s3://%s/%s", bucket, csv_key))
      skipped <- c(skipped, nm)
      next
    }
    upload_to_s3(csv_local, csv_key, bucket = bucket)
    uploaded <- c(uploaded, nm)
  }
  # Always refresh the manifest — it carries the generated_at timestamp.
  upload_to_s3(manifest_path,
               paste0(vintage_prefix, "MANIFEST.json"),
               bucket = bucket)

  # --- Mirror to latest/ -----------------------------------------------------
  # Compare against latest/MANIFEST.json so we only re-PUT files that
  # actually changed relative to whatever "latest" currently points at.
  remote_latest <- .read_remote_manifest(
    paste0(latest_prefix, "MANIFEST.json"), bucket
  )
  for (nm in names(lookups)) {
    if (.hash_unchanged(remote_latest, nm, manifest_entries[[nm]]$sha256)) {
      message(sprintf("SKIP latest (unchanged): %s%s.csv", latest_prefix, nm))
      next
    }
    csv_local <- file.path(out_dir, paste0(nm, ".csv"))
    upload_to_s3(csv_local, paste0(latest_prefix, nm, ".csv"), bucket = bucket)
  }
  upload_to_s3(manifest_path,
               paste0(latest_prefix, "MANIFEST.json"),
               bucket = bucket)

  message(sprintf("Publish complete: %d uploaded, %d skipped (vintage=%s)",
                  length(uploaded), length(skipped), vintage))

  invisible(list(manifest = manifest, vintage = vintage,
                 uploaded = uploaded, skipped = skipped))
}

# Returns parsed manifest list or NULL if not present.
.read_remote_manifest <- function(s3_key, bucket) {
  exists <- tryCatch(
    aws.s3::object_exists(object = s3_key, bucket = bucket),
    error = function(e) FALSE
  )
  if (!isTRUE(exists)) return(NULL)
  tryCatch({
    raw <- aws.s3::get_object(object = s3_key, bucket = bucket)
    jsonlite::fromJSON(rawToChar(raw), simplifyVector = FALSE)
  }, error = function(e) {
    message(sprintf("Could not read remote manifest s3://%s/%s: %s",
                    bucket, s3_key, e$message))
    NULL
  })
}

.hash_unchanged <- function(remote_manifest, name, sha256) {
  if (is.null(remote_manifest)) return(FALSE)
  entry <- remote_manifest$files[[name]]
  if (is.null(entry) || is.null(entry$sha256)) return(FALSE)
  identical(entry$sha256, sha256)
}
