# ============================================================================
# checkpoints.R
# Save and load pipeline checkpoints for recovery and debugging
# ============================================================================

#' Save Checkpoint to Parquet
#'
#' @description
#' Saves a data.table checkpoint to parquet format. Checkpoints allow
#' pipeline recovery and debugging by preserving intermediate states.
#'
#' @param dt data.table to save
#' @param checkpoint_name character name for the checkpoint (e.g., "01_raw", "02_identity")
#'
#' @return invisible NULL
#'
#' @details
#' Checkpoints are only saved if ENABLE_CHECKPOINTS is TRUE (set in main pipeline).
#' Files are saved to CHECKPOINT_DIR with naming pattern:
#' bmf_{PROCESSING_YEAR}_{PROCESSING_MONTH}_{checkpoint_name}.parquet
#'
#' @export
save_checkpoint <- function(dt, checkpoint_name) {


if (!exists("ENABLE_CHECKPOINTS") || !ENABLE_CHECKPOINTS) {
    return(invisible(NULL))
  }

  if (!dir.exists(CHECKPOINT_DIR)) {
    dir.create(CHECKPOINT_DIR, recursive = TRUE)
  }

  filename <- sprintf("bmf_%s_%s_%s.parquet",
                      PROCESSING_YEAR, PROCESSING_MONTH, checkpoint_name)
  path <- file.path(CHECKPOINT_DIR, filename)

  arrow::write_parquet(dt, path)

  log_info(sprintf("Checkpoint saved: %s (%s rows)",
                   checkpoint_name,
                   format(nrow(dt), big.mark = ",")))

  invisible(NULL)
}

#' Load Checkpoint from Parquet
#'
#' @description
#' Loads a previously saved checkpoint. Useful for resuming pipeline
#' from a specific point or debugging transformations.
#'
#' @param checkpoint_name character name of the checkpoint to load
#'
#' @return data.table if checkpoint exists, NULL otherwise
#'
#' @details
#' Looks for checkpoint file in CHECKPOINT_DIR with naming pattern:
#' bmf_{PROCESSING_YEAR}_{PROCESSING_MONTH}_{checkpoint_name}.parquet
#'
#' @export
load_checkpoint <- function(checkpoint_name) {

  filename <- sprintf("bmf_%s_%s_%s.parquet",
                      PROCESSING_YEAR, PROCESSING_MONTH, checkpoint_name)
  path <- file.path(CHECKPOINT_DIR, filename)

  if (!file.exists(path)) {
    log_warn(sprintf("Checkpoint not found: %s", path))
    return(NULL)
  }

  dt <- arrow::read_parquet(path)
  data.table::setDT(dt)

  log_info(sprintf("Checkpoint loaded: %s (%s rows)",
                   checkpoint_name,
                   format(nrow(dt), big.mark = ",")))

  return(dt)
}

#' List Available Checkpoints
#'
#' @description
#' Lists all available checkpoints for the current processing year/month.
#'
#' @return character vector of checkpoint names
#'
#' @export
list_checkpoints <- function() {

  if (!dir.exists(CHECKPOINT_DIR)) {
    return(character(0))
  }

  pattern <- sprintf("bmf_%s_%s_.*\\.parquet$",
                     PROCESSING_YEAR, PROCESSING_MONTH)

  files <- list.files(CHECKPOINT_DIR, pattern = pattern)

  # Extract checkpoint names from filenames
  checkpoint_names <- gsub(
    sprintf("bmf_%s_%s_", PROCESSING_YEAR, PROCESSING_MONTH),
    "",
    files
  )
  checkpoint_names <- gsub("\\.parquet$", "", checkpoint_names)

  return(checkpoint_names)
}

#' Clear Checkpoints
#'
#' @description
#' Removes all checkpoint files for the current processing year/month.
#' Use with caution.
#'
#' @param confirm logical must be TRUE to actually delete files
#'
#' @return invisible NULL
#'
#' @export
clear_checkpoints <- function(confirm = FALSE) {

  if (!confirm) {
    message("Set confirm = TRUE to delete checkpoint files")
    return(invisible(NULL))
  }

  if (!dir.exists(CHECKPOINT_DIR)) {
    return(invisible(NULL))
  }

  pattern <- sprintf("bmf_%s_%s_.*\\.parquet$",
                     PROCESSING_YEAR, PROCESSING_MONTH)

  files <- list.files(CHECKPOINT_DIR, pattern = pattern, full.names = TRUE)

  if (length(files) > 0) {
    unlink(files)
    log_info(sprintf("Cleared %d checkpoint files", length(files)))
  }

  invisible(NULL)
}
