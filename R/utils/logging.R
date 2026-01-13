# ============================================================================
# logging.R
# Simple logging utilities for BMF transformation pipeline
# ============================================================================

# ============================================================================
# Configuration
# ============================================================================

# Global log level: "DEBUG", "INFO", "WARN", "ERROR"
LOG_LEVEL <- Sys.getenv("BMF_LOG_LEVEL", "INFO")

# Log levels in order of severity
.LOG_LEVELS <- c("DEBUG" = 1, "INFO" = 2, "WARN" = 3, "ERROR" = 4)

# ============================================================================
# Core Logging Functions
# ============================================================================

#' Check if a message should be logged at the current level
#' @noRd
.should_log <- function(level) {
  current_level <- .LOG_LEVELS[[LOG_LEVEL]]
  message_level <- .LOG_LEVELS[[level]]
  return(message_level >= current_level)
}

#' Format a log message with timestamp and level
#' @noRd
.format_log_message <- function(level, msg) {
  sprintf("[%s] [%s] %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), level, msg)
}

#' Log a debug message
#'
#' @param msg character message to log
#' @export
log_debug <- function(msg) {
  if (.should_log("DEBUG")) {
    message(.format_log_message("DEBUG", msg))
  }
  invisible(NULL)
}

#' Log an info message
#'
#' @param msg character message to log
#' @export
log_info <- function(msg) {
  if (.should_log("INFO")) {
    message(.format_log_message("INFO", msg))
  }
  invisible(NULL)
}

#' Log a warning message
#'
#' @param msg character message to log
#' @export
log_warn <- function(msg) {
  if (.should_log("WARN")) {
    warning(.format_log_message("WARN", msg), call. = FALSE)
  }
  invisible(NULL)
}

#' Log an error message and stop execution
#'
#' @param msg character message to log
#' @export
log_error <- function(msg) {
  stop(.format_log_message("ERROR", msg), call. = FALSE)
}

# ============================================================================
# Transformation Logging
# ============================================================================

#' Log the start of a transformation
#'
#' @param transform_name character name of the transformation
#' @param input_rows integer number of input rows
#' @export
log_transform_start <- function(transform_name, input_rows = NULL) {
  if (is.null(input_rows)) {
    log_info(sprintf("Starting transformation: %s", transform_name))
  } else {
    log_info(sprintf("Starting transformation: %s (%s rows)",
                     transform_name, format(input_rows, big.mark = ",")))
  }
  invisible(NULL)
}

#' Log the completion of a transformation
#'
#' @param transform_name character name of the transformation
#' @param output_rows integer number of output rows
#' @param duration_secs numeric duration in seconds (optional)
#' @export
log_transform_complete <- function(transform_name, output_rows = NULL, duration_secs = NULL) {
  parts <- sprintf("Completed transformation: %s", transform_name)

  if (!is.null(output_rows)) {
    parts <- sprintf("%s (%s rows)", parts, format(output_rows, big.mark = ","))
  }

  if (!is.null(duration_secs)) {
    parts <- sprintf("%s in %.2f seconds", parts, duration_secs)
  }

  log_info(parts)
  invisible(NULL)
}

# ============================================================================
# Quality Reporting
# ============================================================================

#' Log a data quality report for a transformation
#'
#' @param transform_name character name of the transformation
#' @param total integer total record count
#' @param valid integer count of valid records
#' @param invalid integer count of invalid records (optional)
#' @param undefined integer count of undefined/missing records (optional)
#' @export
log_quality_report <- function(transform_name,
                               total,
                               valid,
                               invalid = NULL,
                               undefined = NULL) {

  message("")
  message(sprintf("--- %s Quality Report ---", transform_name))
  message(sprintf("  Total Records:    %s", format(total, big.mark = ",")))
  message(sprintf("  Valid:            %s (%.2f%%)",
                  format(valid, big.mark = ","),
                  (valid / total) * 100))

  if (!is.null(undefined)) {
    message(sprintf("  Undefined/Empty:  %s (%.2f%%)",
                    format(undefined, big.mark = ","),
                    (undefined / total) * 100))
  }

  if (!is.null(invalid)) {
    message(sprintf("  Invalid:          %s (%.2f%%)",
                    format(invalid, big.mark = ","),
                    (invalid / total) * 100))

    # Warn if invalid rate is high
    invalid_rate <- (invalid / total) * 100
    if (invalid_rate > 5) {
      log_warn(sprintf("High invalid rate (%.2f%%) for %s", invalid_rate, transform_name))
    }
  }

  message("")
  invisible(NULL)
}

# ============================================================================
# Pipeline Logging
# ============================================================================

#' Log pipeline phase start
#'
#' @param phase_name character name of the phase
#' @export
log_phase_start <- function(phase_name) {
  message("")
  message(strrep("=", 60))
  log_info(sprintf("PHASE: %s", phase_name))
  message(strrep("=", 60))
  invisible(NULL)
}

#' Log a checkpoint save
#'
#' @param checkpoint_name character name of checkpoint
#' @param file_path character path where checkpoint was saved
#' @param row_count integer number of rows saved
#' @export
log_checkpoint <- function(checkpoint_name, file_path, row_count) {
  log_info(sprintf("Checkpoint '%s' saved: %s (%s rows)",
                   checkpoint_name,
                   file_path,
                   format(row_count, big.mark = ",")))
  invisible(NULL)
}
