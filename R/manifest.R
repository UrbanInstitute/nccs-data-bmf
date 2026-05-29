# ============================================================================
# manifest.R
#
# Reference implementation of the contract-standard `_manifest.json` shape
# defined in nccs-contracts ADR 0014 ("Standardize Manifest Shape Across
# Producers"). Every NCCS producer emits this same shape so a single
# drift-detection validator and a single cross-repo agent prompt can read
# any producer's manifest.
#
# This file is the canonical reference: bmf-lookups (R/publish_lookups.R)
# uses it directly; bmf-master / bmf-master-geocoded / bmf-legacy copy the
# same call pattern (adapting only their own outputs + inputs).
#
# Schema (ADR 0014 §Schema):
#   {
#     "vintage":  "<tag>",                # e.g. "2026_05" (until the ADR 0013
#                                         #   v{YYYY.MM} flip) — matches the path
#     "built_at": "2026-05-29T13:09:18Z", # ISO 8601, UTC, Z suffix
#     "git_sha":  "0a7048d",              # short SHA (omitted if no git context)
#     "inputs":   [ {uri, etag|sha256}, ... ],
#     "files":    { "<basename>": {file, sha256, bytes, row_count?, columns?, year_counts?}, ... }
#   }
# ============================================================================

#' Short git SHA of the repo containing the working directory.
#'
#' @return Character short SHA, or `NA_character_` when there is no git
#'   context (ADR 0014 permits NA / omission in that case).
#' @export
git_short_sha <- function() {
  out <- tryCatch(
    system2("git", c("rev-parse", "--short", "HEAD"),
            stdout = TRUE, stderr = FALSE),
    error = function(e) NA_character_
  )
  if (length(out) == 0 || is.na(out[[1]]) || !nzchar(out[[1]])) {
    return(NA_character_)
  }
  out[[1]]
}

#' Build and write a contract-standard `_manifest.json` (ADR 0014).
#'
#' Writes `_manifest.json` into `out_dir` and returns the manifest list.
#' Callers own the S3 upload (and any `latest/` mirror) because that part
#' differs per producer — `bmf-lookups` mirrors to `latest/`, `bmf-legacy`
#' does not (it is an append-only archive; see its contract and ADR 0013).
#'
#' @param vintage Vintage tag string. Use the same value that appears in
#'   the S3 path (e.g. `"2026_05"`; flips to `"v2026.05"` when ADR 0013's
#'   vintage-format migration runs).
#' @param out_dir Local directory the output files live in; the manifest
#'   is written here as `_manifest.json`.
#' @param outputs Named list, one entry per output artifact. Each entry is
#'   a list with `path` (local file path, required) and optionally
#'   `row_count` (int), `columns` (character vector), `year_counts`
#'   (named int). `sha256` and `bytes` are computed here.
#' @param inputs List of input descriptors, each a list with `uri`
#'   (required) plus `etag` (S3 inputs) or `sha256` (repo:// / http
#'   inputs). Pass `list()` when none are recorded.
#' @param git_sha Short git SHA (default: `git_short_sha()`).
#'
#' @return Invisibly, `list(manifest = <list>, path = <chr>)`.
#' @export
write_manifest <- function(vintage, out_dir, outputs, inputs = list(),
                           git_sha = git_short_sha()) {
  if (!requireNamespace("digest",   quietly = TRUE)) stop("Package 'digest' required.")
  if (!requireNamespace("jsonlite", quietly = TRUE)) stop("Package 'jsonlite' required.")
  stopifnot(is.list(outputs), length(outputs) > 0)

  drop_null <- function(x) x[!vapply(x, is.null, logical(1))]

  file_entries <- lapply(outputs, function(o) {
    if (is.null(o$path) || !file.exists(o$path)) {
      stop(sprintf("write_manifest: output path missing or unreadable: %s",
                   if (is.null(o$path)) "<NULL>" else o$path))
    }
    drop_null(list(
      file        = basename(o$path),
      sha256      = digest::digest(file = o$path, algo = "sha256"),
      bytes       = as.integer(file.size(o$path)),
      row_count   = if (!is.null(o$row_count))   as.integer(o$row_count) else NULL,
      columns     = if (!is.null(o$columns))     as.list(o$columns)      else NULL,
      year_counts = if (!is.null(o$year_counts)) as.list(o$year_counts)  else NULL
    ))
  })
  # Key the files object by basename, as it appears at the vintage prefix.
  names(file_entries) <- vapply(outputs, function(o) basename(o$path), character(1))

  manifest <- drop_null(list(
    vintage  = vintage,
    built_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    git_sha  = if (length(git_sha) && !is.na(git_sha)) git_sha else NULL,
    inputs   = inputs,
    files    = file_entries
  ))

  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  path <- file.path(out_dir, "_manifest.json")
  jsonlite::write_json(manifest, path, pretty = TRUE, auto_unbox = TRUE)
  invisible(list(manifest = manifest, path = path))
}

#' Describe one repo-local input for the manifest `inputs[]` array.
#'
#' @param repo_relpath Path inside the producer repo (e.g.
#'   `"data/lookup/bmf_code_lookup.xlsx"`). Recorded as a `repo://` URI.
#' @param abs_path Absolute path used to compute the sha256 (default:
#'   `here::here(repo_relpath)`).
#' @return A list `{uri, sha256}` for one `inputs[]` entry.
#' @export
manifest_input_repo <- function(repo_relpath, abs_path = here::here(repo_relpath)) {
  list(
    uri    = paste0("repo://", repo_relpath),
    sha256 = digest::digest(file = abs_path, algo = "sha256")
  )
}

#' Fetch an existing remote `_manifest.json` for idempotent-skip decisions.
#'
#' @param s3_key Full S3 key of the manifest.
#' @param bucket Bucket name.
#' @return Parsed manifest list, or `NULL` if absent/unreadable.
#' @export
read_existing_manifest <- function(s3_key, bucket) {
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

#' Has `file_key` (a basename) the same sha256 in the remote manifest?
#'
#' @param remote_manifest Parsed manifest list from `read_existing_manifest()`.
#' @param file_key The file's basename, as keyed in `manifest$files`.
#' @param sha256 The freshly computed sha256 to compare.
#' @return `TRUE` only when the remote records an identical sha256.
#' @export
manifest_unchanged <- function(remote_manifest, file_key, sha256) {
  if (is.null(remote_manifest)) return(FALSE)
  entry <- remote_manifest$files[[file_key]]
  if (is.null(entry) || is.null(entry$sha256)) return(FALSE)
  identical(entry$sha256, sha256)
}
