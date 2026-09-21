# ============================================================================
# build_ein_index.R
#
# Build and publish the EIN index (nccs-contracts ADR 0050): one small JSON
# file per three-digit EIN prefix, cut from the geocoded Unified BMF, so the
# NCCS website can look up one organization by EIN in the browser without a
# server. Each shard holds identity, NTEE and status columns only.
#
# Output (local):  data/master/ein_index/{prefix}.json  (gzip-compressed bytes)
#                  data/master/ein_index/_manifest.json  (ADR 0014 shape)
# Output (S3):     s3://nccsdata/unified/bmf/ein-index/latest/{prefix}.json
#                  s3://nccsdata/unified/bmf/ein-index/latest/_manifest.json
#
# Shards are uploaded with Content-Encoding: gzip so browsers inflate them
# transparently. Uploads are idempotent: a shard whose sha256 matches the
# remote manifest is skipped.
#
# Depends on R/config.R (BMF_S3_BUCKET) and R/manifest.R (write_manifest,
# read_existing_manifest, manifest_unchanged).
# ============================================================================

EIN_INDEX_S3_PREFIX <- "unified/bmf/ein-index/latest/"

EIN_INDEX_COLUMNS <- c(
  "ein", "org_name_display", "org_addr_city", "org_addr_state", "org_addr_zip5",
  "ntee_code_clean", "ntee_code_definition", "nteev2", "ruling_date",
  "subsection_code", "exempt_organization_type", "status_code_definition",
  "first_vintage_ym", "last_vintage_ym"
)

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

# The first three digits of the nine-digit EIN, which decide the shard a
# record lands in. "53-0196572" -> "530".
ein_index_prefix <- function(ein) {
  digits_only <- gsub("[^0-9]", "", ein)
  substr(digits_only, 1, 3)
}

# Read the vintage stamp from the geocoded build's manifest when it sits next
# to the parquet; fall back to the current month.
read_source_vintage <- function(geocoded_path) {
  manifest_path <- file.path(dirname(geocoded_path), "_manifest.json")
  if (!file.exists(manifest_path)) return(format(Sys.Date(), "%Y_%m"))
  manifest <- jsonlite::fromJSON(manifest_path, simplifyVector = FALSE)
  if (is.null(manifest$vintage)) format(Sys.Date(), "%Y_%m") else manifest$vintage
}

# Write one shard: a JSON object with the column names once and the records
# as arrays in that order, gzip-compressed on disk.
write_ein_index_shard <- function(shard_rows, prefix, vintage, built_at, output_dir) {
  shard_path <- file.path(output_dir, paste0(prefix, ".json"))
  payload <- list(
    vintage  = vintage,
    built_at = built_at,
    prefix   = prefix,
    fields   = EIN_INDEX_COLUMNS,
    records  = dplyr::select(shard_rows, dplyr::all_of(EIN_INDEX_COLUMNS))
  )
  json_text <- jsonlite::toJSON(payload, dataframe = "values", na = "null",
                                auto_unbox = TRUE, null = "null")
  connection <- gzfile(shard_path, open = "wb")
  on.exit(close(connection), add = TRUE)
  writeLines(json_text, connection, useBytes = TRUE)
  list(path = shard_path, row_count = nrow(shard_rows), columns = EIN_INDEX_COLUMNS)
}

# Upload one shard with the headers browsers need to inflate it.
upload_ein_index_shard <- function(local_path, s3_key, bucket) {
  aws.s3::put_object(
    file    = local_path,
    object  = s3_key,
    bucket  = bucket,
    headers = list(
      `Content-Type`     = "application/json",
      `Content-Encoding` = "gzip"
    )
  )
}

# ---------------------------------------------------------------------------
# Build
# ---------------------------------------------------------------------------

#' Build the EIN index shards and manifest from the geocoded Unified BMF
#'
#' @param geocoded_path Path to bmf_unified_geocoded.parquet.
#' @param output_dir    Directory that receives {prefix}.json and _manifest.json.
#' @param vintage       Vintage stamp (YYYY_MM); defaults to the source manifest's.
#' @param source_uri    S3 URI recorded as the manifest input.
#' @return Invisibly: list(manifest, outputs, output_dir, vintage).
build_ein_index <- function(geocoded_path,
                            output_dir = here::here("data", "master", "ein_index"),
                            vintage    = read_source_vintage(geocoded_path),
                            source_uri = "s3://nccsdata/geocoding/unified-bmf/latest/bmf_unified_geocoded.parquet") {

  stopifnot(file.exists(geocoded_path))
  if (dir.exists(output_dir)) unlink(output_dir, recursive = TRUE)
  dir.create(output_dir, recursive = TRUE)

  built_at <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")

  unified_rows <- arrow::open_dataset(geocoded_path) |>
    dplyr::select(dplyr::all_of(EIN_INDEX_COLUMNS)) |>
    dplyr::collect() |>
    tibble::as_tibble() |>   # config.R makes arrow return data.tables; keep plain frames here
    dplyr::mutate(dplyr::across(dplyr::everything(), as.character)) |>
    dplyr::mutate(shard_prefix = ein_index_prefix(ein)) |>
    dplyr::filter(nchar(shard_prefix) == 3) |>
    dplyr::arrange(ein)

  rows_by_prefix <- split(unified_rows, unified_rows$shard_prefix)

  outputs <- purrr::imap(rows_by_prefix, function(shard_rows, prefix) {
    write_ein_index_shard(shard_rows, prefix, vintage, built_at, output_dir)
  })
  names(outputs) <- paste0(names(rows_by_prefix), ".json")

  inputs <- list(list(uri = source_uri, sha256 = digest::digest(file = geocoded_path, algo = "sha256")))
  manifest_written <- write_manifest(vintage = vintage, out_dir = output_dir,
                                     outputs = outputs, inputs = inputs)

  message(sprintf("EIN index: %d shards, %s organizations, vintage %s -> %s",
                  length(outputs), format(nrow(unified_rows), big.mark = ","), vintage, output_dir))

  invisible(list(manifest = manifest_written$manifest, outputs = outputs,
                 output_dir = output_dir, vintage = vintage))
}

# ---------------------------------------------------------------------------
# Publish
# ---------------------------------------------------------------------------

#' Upload the built EIN index to S3, skipping shards whose sha256 is unchanged
#'
#' @param output_dir Directory produced by build_ein_index().
#' @param s3_prefix  Key prefix ending in "/".
#' @param bucket     Bucket name.
#' @param dry_run    If TRUE, print the plan and upload nothing.
#' @return Invisibly: list(uploaded, skipped).
publish_ein_index <- function(output_dir = here::here("data", "master", "ein_index"),
                              s3_prefix  = EIN_INDEX_S3_PREFIX,
                              bucket     = BMF_S3_BUCKET,
                              dry_run    = FALSE) {

  stopifnot(endsWith(s3_prefix, "/"))
  manifest_path <- file.path(output_dir, "_manifest.json")
  stopifnot(file.exists(manifest_path))
  local_manifest  <- jsonlite::fromJSON(manifest_path, simplifyVector = FALSE)
  remote_manifest <- if (dry_run) NULL else read_existing_manifest(paste0(s3_prefix, "_manifest.json"), bucket)

  uploaded <- character()
  skipped  <- character()

  for (file_name in names(local_manifest$files)) {
    local_path <- file.path(output_dir, file_name)
    sha256     <- local_manifest$files[[file_name]]$sha256
    s3_key     <- paste0(s3_prefix, file_name)
    if (manifest_unchanged(remote_manifest, file_name, sha256)) {
      skipped <- c(skipped, file_name)
      next
    }
    if (dry_run) {
      message(sprintf("  PUT  %s", s3_key))
    } else {
      upload_ein_index_shard(local_path, s3_key, bucket)
    }
    uploaded <- c(uploaded, file_name)
  }

  if (dry_run) {
    message(sprintf("  PUT  %s_manifest.json", s3_prefix))
  } else {
    aws.s3::put_object(file = manifest_path, object = paste0(s3_prefix, "_manifest.json"),
                       bucket = bucket, headers = list(`Content-Type` = "application/json"))
  }

  message(sprintf("EIN index publish %s: %d uploaded, %d skipped",
                  if (dry_run) "(dry run)" else "complete", length(uploaded), length(skipped)))
  invisible(list(uploaded = uploaded, skipped = skipped))
}
