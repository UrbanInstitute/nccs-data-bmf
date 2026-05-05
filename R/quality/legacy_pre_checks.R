# ============================================================================
# legacy_pre_checks.R
# Pre-transformation validation for harmonized legacy BMF data.
#
# Runs AFTER harmonize_legacy_bmf(). At this point columns have been renamed
# to current-schema names and BMF_REQUIRED_COLUMNS members are NA-filled if
# absent in the source. We only require the always-populated subset
# (BMF_LEGACY_MIN_COLUMNS) to be non-empty.
# ============================================================================

#' Validate harmonized legacy BMF structure
#'
#' @param dt data.table post-harmonization
#' @param min_cols character columns that must be present AND non-empty
#' @param strict logical; if TRUE stop on validation failure
#' @return validation results list (same shape as validate_raw_bmf_structure)
validate_legacy_bmf_structure <- function(dt,
                                          min_cols = BMF_LEGACY_MIN_COLUMNS,
                                          strict = TRUE) {

  results <- validate_raw_bmf_structure(dt, required_cols = min_cols, strict = FALSE)

  # Additional check: minimum columns must not be entirely empty after harmonization
  empty_cols <- character(0)
  for (col in intersect(min_cols, names(dt))) {
    n_populated <- sum(!is.na(dt[[col]]) & dt[[col]] != "")
    if (n_populated == 0) empty_cols <- c(empty_cols, col)
  }
  if (length(empty_cols) > 0) {
    msg <- sprintf("Minimum-required columns are entirely empty after harmonization: %s",
                   paste(empty_cols, collapse = ", "))
    results$messages <- c(results$messages, msg)
    results$passed <- FALSE
    results$empty_min_columns <- empty_cols
  }

  message(sprintf("LEGACY MODE: minimum required columns = %d (%s)",
                  length(min_cols), paste(min_cols, collapse = ", ")))

  if (!results$passed && strict) {
    stop(paste(
      "Legacy pre-transformation validation failed:",
      paste(results$messages, collapse = "; ")
    ))
  }

  results
}
