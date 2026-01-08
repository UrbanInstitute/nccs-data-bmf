# ============================================================================
# input_validation.R
# Shared validation functions for BMF transformation pipeline
# ============================================================================

#' Validate Input Data Table
#'
#' @description Validates that a data.table has required columns.
#'
#' @param dt data.table to validate
#' @param required_cols character vector of required column names
#' @param context character description for error messages (e.g., "BMF data")
#'
#' @return invisible(TRUE) if valid, stops with error if not
#' @export
validate_data_table <- function(dt, required_cols, context = "data.table") {

  if (!inherits(dt, "data.table")) {
    stop(sprintf("%s must be a data.table, got %s", context, class(dt)[1]))
  }

  missing_cols <- setdiff(required_cols, names(dt))
  if (length(missing_cols) > 0) {
    stop(sprintf("%s missing required columns: %s",
                 context, paste(missing_cols, collapse = ", ")))
  }

  invisible(TRUE)
}

#' Validate Lookup Table
#'
#' @description Validates that a lookup table has required columns.
#'
#' @param lookup data.table lookup table
#' @param required_cols character vector of required column names
#' @param lookup_name character name of lookup for error messages
#'
#' @return invisible(TRUE) if valid, stops with error if not
#' @export
validate_lookup <- function(lookup, required_cols, lookup_name = "lookup table") {
  if (!inherits(lookup, "data.table")) {
    stop(sprintf("%s must be a data.table, got %s", lookup_name, class(lookup)[1]))
  }

  missing_cols <- setdiff(required_cols, names(lookup))
  if (length(missing_cols) > 0) {
    stop(sprintf("%s missing required columns: %s",
                 lookup_name, paste(missing_cols, collapse = ", ")))
  }

  if (nrow(lookup) == 0) {
    stop(sprintf("%s is empty (0 rows)", lookup_name))
  }

  invisible(TRUE)
}

#' Validate Inputs (Legacy Compatibility)
#'
#' @description Original validation function - validates both data table and lookup.
#'
#' @param dt data.table to validate
#' @param input_col character name of input column
#' @param lookup data.table lookup table
#' @param lookup_cols character vector of required lookup columns
#'
#' @return invisible(TRUE) if valid, stops with error if not
#' @export
validate_inputs <- function(dt, input_col, lookup, lookup_cols) {
  if (!all(lookup_cols %in% names(lookup))) {
    stop("Lookup table missing required columns: ",
         paste(setdiff(lookup_cols, names(lookup)), collapse = ", "))
  }
  if (!(input_col %in% names(dt))) {
    stop("Input data.table missing column: ", input_col)
  }
  invisible(TRUE)
}

#' Validate Join Success
#'
#' @description Checks if a lookup join produced any unmatched values.
#'
#' @param dt data.table after join
#' @param key_col character name of the key column used for join
#' @param result_col character name of the result column from join
#' @param lookup_name character name of lookup for messages
#' @param warn_threshold numeric percentage threshold for warning (default 0.01 = 1%)
#' @param error_threshold numeric percentage threshold for error (default NULL = no error)
#'
#' @return invisible(list) with unmatched count and percentage
#' @export
validate_join_success <- function(dt,
                                  key_col,
                                  result_col,
                                  lookup_name = "lookup",
                                  warn_threshold = 0.01,
                                  error_threshold = NULL) {

  total_rows <- nrow(dt)

  # Count rows where key exists but result is NA (unmatched)
  unmatched_count <- dt[!is.na(get(key_col)) & is.na(get(result_col)), .N]
  unmatched_pct <- unmatched_count / total_rows

  result <- list(
    unmatched_count = unmatched_count,
    unmatched_pct = unmatched_pct,
    total_rows = total_rows
  )

  if (!is.null(error_threshold) && unmatched_pct > error_threshold) {
    stop(sprintf("Join to %s failed: %d rows (%.2f%%) unmatched, exceeds %.2f%% threshold",
                 lookup_name,
                 unmatched_count,
                 unmatched_pct * 100,
                 error_threshold * 100))
  }

  if (unmatched_pct > warn_threshold) {
    warning(sprintf("Join to %s: %d rows (%.2f%%) unmatched",
                    lookup_name,
                    unmatched_count,
                    unmatched_pct * 100))
  }

  invisible(result)
}

#' Validate Type Conversion
#'
#' @description Checks if type conversion produced unexpected NAs.
#'
#' @param original_values vector of original values before conversion
#' @param converted_values vector of converted values
#' @param col_name character name of column for messages
#' @param warn_on_conversion_loss logical whether to warn on conversion failures
#'
#' @return invisible(list) with conversion statistics
#' @export
validate_type_conversion <- function(original_values,
                                     converted_values,
                                     col_name,
                                     warn_on_conversion_loss = TRUE) {

  # Count values that were non-NA before but became NA after conversion
  original_valid <- !is.na(original_values)
  converted_na <- is.na(converted_values)
  conversion_failures <- sum(original_valid & converted_na)

  result <- list(
    original_count = sum(original_valid),
    converted_count = sum(!converted_na),
    conversion_failures = conversion_failures
  )

  if (conversion_failures > 0 && warn_on_conversion_loss) {
    warning(sprintf("Type conversion for '%s': %d values failed to convert",
                    col_name, conversion_failures))
  }

  invisible(result)
}

#' Check for Negative Values
#'
#' @description Checks for and reports negative values in numeric columns.
#'
#' @param dt data.table containing the column
#' @param col_name character name of column to check
#' @param allow_negative logical whether negative values are expected
#'
#' @return invisible(integer) count of negative values
#' @export
validate_numeric_range <- function(dt, col_name, allow_negative = FALSE) {
  values <- dt[[col_name]]

  if (!is.numeric(values)) {
    stop(sprintf("Column '%s' is not numeric", col_name))
  }

  negative_count <- sum(values < 0, na.rm = TRUE)

  if (negative_count > 0 && !allow_negative) {
    negative_examples <- head(unique(values[values < 0]), 5)
    warning(sprintf("Column '%s' contains %d negative values. Examples: %s",
                    col_name,
                    negative_count,
                    paste(negative_examples, collapse = ", ")))
  }

  invisible(negative_count)
}
