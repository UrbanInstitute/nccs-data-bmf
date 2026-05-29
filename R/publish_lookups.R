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
#   s3://nccsdata/lookups/bmf/{YYYY_MM}/_manifest.json   (ADR 0014 shape)
#   s3://nccsdata/lookups/bmf/{YYYY_MM}/MANIFEST.json     (deprecated; byte
#                                                          copy, 90-day window)
#   s3://nccsdata/lookups/bmf/latest/{name}.csv          (mirror of newest vintage)
#   s3://nccsdata/lookups/bmf/latest/_manifest.json
#   s3://nccsdata/lookups/bmf/latest/MANIFEST.json        (deprecated copy)
#
# Manifest: the contract-standard `_manifest.json` (nccs-contracts ADR 0014),
# built via R/manifest.R::write_manifest(). This file is the reference
# implementation other BMF producers copy. The legacy `MANIFEST.json` name
# is dual-written (byte-identical) for one deprecation window per ADR 0014.
#
# Idempotency: each file's sha256 is recorded in _manifest.json. On re-run,
# the existing remote manifest is fetched; any file whose hash is unchanged
# is skipped. All S3 traffic goes through aws.s3::put_object (upload_to_s3),
# consistent with the rest of this codebase.
#
# Note (ADR 0013): the vintage path format flip from YYYY_MM to v{YYYY.MM}
# is intentionally NOT done here — it is a breaking path change handled in a
# separate, consumer-coordinated migration. This change is manifest-shape
# only (ADR 0014).
# ============================================================================

#' Publish BMF lookup tables to S3
#'
#' Writes each entry of `lookup_ls` to a CSV under `lookup_dir`, builds a
#' contract-standard `_manifest.json` (ADR 0014, via `write_manifest()`),
#' and uploads to s3://{bucket}/{s3_prefix}{vintage}/. Then mirrors the same
#' files to s3://{bucket}/{s3_prefix}latest/.
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

  # --- Write CSVs ------------------------------------------------------------
  outputs <- vector("list", length(lookups))
  names(outputs) <- names(lookups)
  for (nm in names(lookups)) {
    df <- lookups[[nm]]
    csv_path <- file.path(out_dir, paste0(nm, ".csv"))
    data.table::fwrite(df, csv_path)
    outputs[[nm]] <- list(path = csv_path, row_count = nrow(df), columns = names(df))
  }

  # --- Build the contract-standard _manifest.json (ADR 0014) -----------------
  inputs <- list(
    manifest_input_repo("data/lookup/bmf_code_lookup.xlsx"),
    manifest_input_repo("data/lookup/ntee_legacy_5char_lookup.csv")
  )
  mw <- write_manifest(vintage = vintage, out_dir = out_dir,
                       outputs = outputs, inputs = inputs)
  manifest      <- mw$manifest
  manifest_path <- mw$path

  # sha256 per output file, keyed by basename (as in manifest$files).
  shas <- vapply(names(lookups),
                 function(nm) manifest$files[[paste0(nm, ".csv")]]$sha256,
                 character(1))
  names(shas) <- paste0(names(lookups), ".csv")

  message(sprintf("Wrote %d lookup CSVs + _manifest.json to %s",
                  length(lookups), out_dir))

  # --- Skip-decision inputs --------------------------------------------------
  vintage_prefix <- paste0(s3_prefix, vintage, "/")
  latest_prefix  <- paste0(s3_prefix, "latest/")
  remote_manifest <- read_existing_manifest(
    paste0(vintage_prefix, "_manifest.json"), bucket
  )

  uploaded <- character()
  skipped  <- character()

  # Dual-write the manifest during the ADR 0014 deprecation window:
  # _manifest.json is canonical; MANIFEST.json is a byte-identical copy kept
  # readable for one window for any consumer still on the old path.
  put_manifest <- function(prefix) {
    upload_to_s3(manifest_path, paste0(prefix, "_manifest.json"), bucket = bucket)
    upload_to_s3(manifest_path, paste0(prefix, "MANIFEST.json"),  bucket = bucket)
  }

  if (dry_run) {
    message("DRY RUN: would upload to s3://", bucket, "/", vintage_prefix)
    for (nm in names(lookups)) {
      key <- paste0(nm, ".csv")
      action <- if (manifest_unchanged(remote_manifest, key, shas[[key]])) {
        skipped  <- c(skipped, nm); "SKIP"
      } else {
        uploaded <- c(uploaded, nm); "PUT "
      }
      message(sprintf("  %s %s%s", action, vintage_prefix, key))
    }
    message(sprintf("  PUT  %s_manifest.json (+ MANIFEST.json copy)", vintage_prefix))
    message("DRY RUN: latest/ mirror would copy the same files.")
    return(invisible(list(manifest = manifest, vintage = vintage,
                          uploaded = uploaded, skipped = skipped)))
  }

  # --- Upload to vintage prefix ----------------------------------------------
  for (nm in names(lookups)) {
    key       <- paste0(nm, ".csv")
    csv_local <- file.path(out_dir, key)
    if (manifest_unchanged(remote_manifest, key, shas[[key]])) {
      message(sprintf("SKIP (unchanged): s3://%s/%s%s", bucket, vintage_prefix, key))
      skipped <- c(skipped, nm)
      next
    }
    upload_to_s3(csv_local, paste0(vintage_prefix, key), bucket = bucket)
    uploaded <- c(uploaded, nm)
  }
  put_manifest(vintage_prefix)  # always refresh — carries the built_at timestamp

  # --- Mirror to latest/ -----------------------------------------------------
  # Compare against latest/_manifest.json so we only re-PUT files that
  # actually changed relative to whatever "latest" currently points at.
  remote_latest <- read_existing_manifest(
    paste0(latest_prefix, "_manifest.json"), bucket
  )
  for (nm in names(lookups)) {
    key <- paste0(nm, ".csv")
    if (manifest_unchanged(remote_latest, key, shas[[key]])) {
      message(sprintf("SKIP latest (unchanged): %s%s", latest_prefix, key))
      next
    }
    upload_to_s3(file.path(out_dir, key), paste0(latest_prefix, key), bucket = bucket)
  }
  put_manifest(latest_prefix)

  message(sprintf("Publish complete: %d uploaded, %d skipped (vintage=%s)",
                  length(uploaded), length(skipped), vintage))

  invisible(list(manifest = manifest, vintage = vintage,
                 uploaded = uploaded, skipped = skipped))
}
