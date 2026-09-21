# ============================================================================
# build_ein_index.R
#
# Build and publish the EIN index (nccs-contracts ADR 0050): one small JSON
# file per four-digit EIN prefix, cut from the geocoded Unified BMF, so the
# NCCS website can look up one organization by EIN in the browser without a
# server. Each shard holds identity, NTEE and status columns only.
#
# Output (local):  data/master/ein_index/{prefix}.json  (gzip-compressed bytes)
#                  data/master/ein_index/_manifest.json  (ADR 0014 shape)
# Output (S3):     s3://nccsdata/unified/bmf/ein-index/v{YYYY_MM}/{prefix}.json  (retained, ADR 0042)
#                  s3://nccsdata/unified/bmf/ein-index/latest/{prefix}.json      (mirror)
#                  ... plus _manifest.json in each folder
#
# Shards are uploaded with Content-Encoding: gzip so browsers inflate them
# transparently. Shards carry no build time, and gzip output here is
# deterministic, so an unchanged shard has the same bytes and hash on every
# rebuild; uploads skip any shard whose sha256 matches the remote manifest.
#
# The build stops rather than publishing if any source EIN is not in the
# XX-XXXXXXX form, if an EIN repeats, or if the shard rows do not add up to
# the source rows.
#
# Depends on R/config.R (BMF_S3_BUCKET) and R/manifest.R (write_manifest,
# read_existing_manifest, manifest_unchanged).
# ============================================================================

EIN_INDEX_S3_ROOT <- "unified/bmf/ein-index/"
EIN_PATTERN       <- "^[0-9]{2}-[0-9]{7}$"

EIN_INDEX_COLUMNS <- c(
  "ein", "org_name_display", "org_addr_city", "org_addr_state", "org_addr_zip5",
  "ntee_code_clean", "ntee_code_definition", "nteev2", "ruling_date",
  "subsection_code", "exempt_organization_type", "status_code_definition",
  "first_vintage_ym", "last_vintage_ym"
)

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

# The first four digits of the nine-digit EIN, which decide the shard a
# record lands in. "53-0196572" -> "5301". Three digits gave shards of up to
# 209,000 organizations (6 MB compressed); four digits caps the largest at
# about 48,000 (under 2 MB) with a typical shard well under 10 KB.
EIN_INDEX_PREFIX_LENGTH <- 4L

ein_index_prefix <- function(ein) {
  digits_only <- gsub("[^0-9]", "", ein)
  substr(digits_only, 1, EIN_INDEX_PREFIX_LENGTH)
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
# as arrays in that order, gzip-compressed on disk. No build time is written,
# so identical records give identical bytes.
write_ein_index_shard <- function(shard_rows, prefix, vintage, output_dir) {
  shard_path <- file.path(output_dir, paste0(prefix, ".json"))
  payload <- list(
    vintage  = vintage,
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

# Upload one shard with the headers browsers need to inflate it. Stops on
# failure: aws.s3::put_object() returns FALSE rather than erroring, and a
# shard counted as uploaded but missing would be skipped forever afterwards
# because its hash would sit in the manifest.
upload_ein_index_shard <- function(local_path, s3_key, bucket, put_object = aws.s3::put_object) {
  result <- put_object(
    file    = local_path,
    object  = s3_key,
    bucket  = bucket,
    headers = list(
      `Content-Type`     = "application/json",
      `Content-Encoding` = "gzip"
    )
  )
  if (!isTRUE(result)) {
    stop(sprintf("EIN index: upload failed for s3://%s/%s; manifest not written", bucket, s3_key))
  }
  invisible(TRUE)
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

  unified_rows <- arrow::open_dataset(geocoded_path) |>
    dplyr::select(dplyr::all_of(EIN_INDEX_COLUMNS)) |>
    dplyr::collect() |>
    tibble::as_tibble() |>   # config.R makes arrow return data.tables; keep plain frames here
    dplyr::mutate(dplyr::across(dplyr::everything(), as.character)) |>
    dplyr::arrange(ein)

  # Every row must carry a canonical EIN and each EIN must appear once;
  # otherwise a lookup could silently miss an organization.
  malformed_eins <- unified_rows$ein[is.na(unified_rows$ein) | !grepl(EIN_PATTERN, unified_rows$ein)]
  if (length(malformed_eins) > 0) {
    stop(sprintf("EIN index: %d source rows have an EIN not in XX-XXXXXXX form, e.g. %s",
                 length(malformed_eins), paste(utils::head(malformed_eins, 5), collapse = ", ")))
  }
  duplicated_eins <- unique(unified_rows$ein[duplicated(unified_rows$ein)])
  if (length(duplicated_eins) > 0) {
    stop(sprintf("EIN index: %d EINs appear more than once, e.g. %s",
                 length(duplicated_eins), paste(utils::head(duplicated_eins, 5), collapse = ", ")))
  }

  unified_rows <- dplyr::mutate(unified_rows, shard_prefix = ein_index_prefix(ein))
  rows_by_prefix <- split(unified_rows, unified_rows$shard_prefix)

  outputs <- purrr::imap(rows_by_prefix, function(shard_rows, prefix) {
    write_ein_index_shard(shard_rows, prefix, vintage, output_dir)
  })
  names(outputs) <- paste0(names(rows_by_prefix), ".json")

  shard_row_total <- sum(vapply(outputs, function(o) o$row_count, numeric(1)))
  if (shard_row_total != nrow(unified_rows)) {
    stop(sprintf("EIN index: shards hold %d rows but the source has %d", shard_row_total, nrow(unified_rows)))
  }

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

#' Upload the built EIN index to S3: the vintage folder, then the latest/ mirror
#'
#' Each folder is compared against its own remote manifest and only changed
#' shards are sent (ADR 0042 retention; ADR 0014 idempotency).
#'
#' @param output_dir Directory produced by build_ein_index().
#' @param s3_root    Key prefix ending in "/" under which v{vintage}/ and latest/ sit.
#' @param bucket     Bucket name.
#' @param dry_run    If TRUE, print the plan and upload nothing.
#' @param put_object The upload function (aws.s3::put_object); replaceable in tests.
#' @return Invisibly: list(vintage_prefix, latest_prefix, uploaded, skipped) with
#'   per-folder counts.
publish_ein_index <- function(output_dir = here::here("data", "master", "ein_index"),
                              s3_root    = EIN_INDEX_S3_ROOT,
                              bucket     = BMF_S3_BUCKET,
                              dry_run    = FALSE,
                              put_object = aws.s3::put_object) {

  stopifnot(endsWith(s3_root, "/"))
  manifest_path <- file.path(output_dir, "_manifest.json")
  stopifnot(file.exists(manifest_path))
  local_manifest <- jsonlite::fromJSON(manifest_path, simplifyVector = FALSE)

  vintage_prefix <- paste0(s3_root, "v", local_manifest$vintage, "/")
  latest_prefix  <- paste0(s3_root, "latest/")

  upload_folder <- function(s3_prefix) {
    remote_manifest <- if (dry_run) NULL else read_existing_manifest(paste0(s3_prefix, "_manifest.json"), bucket)
    uploaded <- character()
    skipped  <- character()
    for (file_name in names(local_manifest$files)) {
      sha256 <- local_manifest$files[[file_name]]$sha256
      if (manifest_unchanged(remote_manifest, file_name, sha256)) {
        skipped <- c(skipped, file_name)
        next
      }
      if (dry_run) {
        message(sprintf("  PUT  %s%s", s3_prefix, file_name))
      } else {
        upload_ein_index_shard(file.path(output_dir, file_name), paste0(s3_prefix, file_name), bucket, put_object)
      }
      uploaded <- c(uploaded, file_name)
    }
    if (dry_run) {
      message(sprintf("  PUT  %s_manifest.json", s3_prefix))
    } else {
      # Every shard above succeeded (a failure stops the run), so the manifest
      # can now be written. Its own upload is checked too.
      manifest_result <- put_object(file = manifest_path, object = paste0(s3_prefix, "_manifest.json"),
                                    bucket = bucket, headers = list(`Content-Type` = "application/json"))
      if (!isTRUE(manifest_result)) {
        stop(sprintf("EIN index: manifest upload failed for s3://%s/%s_manifest.json", bucket, s3_prefix))
      }
    }
    message(sprintf("EIN index -> %s %s: %d uploaded, %d skipped", s3_prefix,
                    if (dry_run) "(dry run)" else "done", length(uploaded), length(skipped)))
    list(uploaded = uploaded, skipped = skipped)
  }

  vintage_result <- upload_folder(vintage_prefix)
  latest_result  <- upload_folder(latest_prefix)

  invisible(list(vintage_prefix = vintage_prefix, latest_prefix = latest_prefix,
                 uploaded = c(vintage = length(vintage_result$uploaded), latest = length(latest_result$uploaded)),
                 skipped  = c(vintage = length(vintage_result$skipped),  latest = length(latest_result$skipped))))
}
