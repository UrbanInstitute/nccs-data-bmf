# ============================================================================
# geocoder_working_files.R
#
# Names of the working files a Unified BMF geocoding run keeps in its input/
# and output/ folders (run manifest, address lookup, batches, batch outputs).
#
# These files were named bmf_master_geocoder_* before the ADR 0037 rename
# reached the geocoding code, and are named bmf_unified_geocoder_* since. A
# run that was staged or retrieved before the rename still has its files
# under the old names, on disk and in its S3 run folder, and its ledger and
# manifest refer to them by those names. Renaming such files by hand would
# leave the ledger pointing at names that no longer exist. So new files are
# always WRITTEN under the new names, and the steps that READ working files
# (retrieve, merge) accept either name through the helpers below.
# ============================================================================

GEOCODER_WORKING_FILE_PREFIX        <- "bmf_unified_geocoder_"
GEOCODER_LEGACY_WORKING_FILE_PREFIX <- "bmf_master_geocoder_"

# Matches a batch output under either name, for example
# bmf_unified_geocoder_batch_03_geocoded.csv or
# bmf_master_geocoder_batch_03_geocoded.csv.
GEOCODER_BATCH_OUTPUT_PATTERN <- "^bmf_(unified|master)_geocoder_batch_\\d{2}_geocoded\\.csv$"

#' The pre-rename name of a geocoder working file.
#'
#' @param file_name Character: a working file name under the current naming,
#'   for example "bmf_unified_geocoder_manifest.json".
#' @return Character: the same file under the old naming, for example
#'   "bmf_master_geocoder_manifest.json".
geocoder_legacy_file_name <- function(file_name) {

  legacy_file_name <- stringr::str_replace(
    file_name,
    stringr::fixed(GEOCODER_WORKING_FILE_PREFIX),
    GEOCODER_LEGACY_WORKING_FILE_PREFIX
  )

  return(legacy_file_name)

}

#' Path of a geocoder working file to READ, under whichever name it has.
#'
#' Returns the path under the current name when that file exists. When it
#' does not and the same file exists under the pre-rename name, returns that
#' path, so a run started before the rename can still be retrieved and
#' merged. When neither exists, returns the path under the current name, so
#' the caller's "file not found" message names the file a new run would have.
#'
#' @param folder Character: the folder holding the file (a run's input/ or
#'   output/ folder).
#' @param file_name Character: the file's name under the current naming.
#' @return Character: the path to read.
geocoder_working_file_path <- function(folder, file_name) {

  current_path <- file.path(folder, file_name)
  legacy_path  <- file.path(folder, geocoder_legacy_file_name(file_name))

  only_legacy_exists <- !file.exists(current_path) && file.exists(legacy_path)

  if (only_legacy_exists) {

    message(sprintf("Using a working file from before the rename: %s", legacy_path))

    working_file_path <- legacy_path

  } else {

    working_file_path <- current_path

  }

  return(working_file_path)

}

#' The batch output files in a run's output/ folder, under either naming.
#'
#' Stops when the folder holds outputs under BOTH namings. That means outputs
#' of two different runs are sitting side by side (the old run's files were
#' not cleared), and merging them together would mix two runs' results.
#'
#' @param output_dir Character: the run's output/ folder.
#' @return Character vector: full paths of the batch output files, possibly
#'   empty.
geocoder_batch_output_files <- function(output_dir) {

  output_files <- list.files(
    output_dir,
    pattern    = GEOCODER_BATCH_OUTPUT_PATTERN,
    full.names = TRUE
  )

  is_legacy_name <- stringr::str_starts(basename(output_files), GEOCODER_LEGACY_WORKING_FILE_PREFIX)

  has_both_namings <- any(is_legacy_name) && any(!is_legacy_name)

  if (has_both_namings) {

    stop(sprintf(
      paste0(
        "%s holds batch outputs under both the old (bmf_master_geocoder_*) and ",
        "the new (bmf_unified_geocoder_*) names. These come from two different ",
        "runs. Move the outputs of the run you are not merging out of the folder, ",
        "then run the merge again."
      ),
      output_dir
    ))

  }

  return(output_files)

}
