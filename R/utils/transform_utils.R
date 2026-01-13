# ============================================================================
# transform_utils.R
# Generic transformation utilities for BMF pipeline
# ============================================================================

#' Generic Simple Lookup Transformation
#'
#' @description
#' Performs a standard lookup transformation: converts an input column to
#' a typed key, joins with a lookup table, and adds the definition column.
#' This consolidates the pattern used across many simple code transformations.
#'
#' @param dt data.table containing the input data
#' @param input_col character name of the input column in dt
#' @param lookup data.table containing lookup values
#' @param lookup_key character name of the key column in lookup table
#' @param definition_col character name of the definition column to add
#' @param type_func function to convert input values (default: as.integer)
#' @param output_key character name for the output key column (default: same as lookup_key)
#' @param validate logical whether to run validation checks (default: TRUE)
#'
#' @return data.table with new columns: output_key and definition_col
#'
#' @examples
#' \dontrun{
#' result <- transform_simple_lookup(
#'   dt = bmf_data,
#'   input_col = "FOUNDATION",
#'   lookup = foundation_lookup,
#'   lookup_key = "foundation_code",
#'   definition_col = "foundation_code_definition",
#'   type_func = as.integer
#' )
#' }
#'
#' @export
transform_simple_lookup <- function(dt,
                                    input_col,
                                    lookup,
                                    lookup_key,
                                    definition_col,
                                    type_func = as.integer,
                                    output_key = NULL,
                                    validate = TRUE) {

  # Default output_key to lookup_key if not specified
  if (is.null(output_key)) {
    output_key <- lookup_key
  }

  # Validation
  if (validate) {
    if (!(input_col %in% names(dt))) {
      stop(sprintf("Input column '%s' not found in data.table", input_col))
    }
    if (!(lookup_key %in% names(lookup))) {
      stop(sprintf("Lookup key '%s' not found in lookup table", lookup_key))
    }
    if (!(definition_col %in% names(lookup))) {
      stop(sprintf("Definition column '%s' not found in lookup table", definition_col))
    }
  }

  # Safe copy

dt_safe <- data.table::copy(dt)

  # Type conversion with suppressed warnings (check afterward)
  original_values <- dt_safe[[input_col]]
  dt_safe[, (output_key) := suppressWarnings(type_func(get(input_col)))]
  converted_values <- dt_safe[[output_key]]

  # Check for conversion failures
  conversion_failures <- sum(!is.na(original_values) & is.na(converted_values))
  if (conversion_failures > 0 && validate) {
    warning(sprintf("Type conversion for '%s': %d values failed to convert",
                    input_col, conversion_failures))
  }

  # Set keys for efficient join
  data.table::setkeyv(dt_safe, output_key)
  lookup_copy <- data.table::copy(lookup)
  data.table::setkeyv(lookup_copy, lookup_key)

  # Perform lookup join
  dt_safe[lookup_copy, (definition_col) := get(paste0("i.", definition_col))]

  # Check join success
  if (validate) {
    unmatched <- dt_safe[!is.na(get(output_key)) & is.na(get(definition_col)), .N]
    if (unmatched > 0) {
      warning(sprintf("Lookup join for '%s': %d rows with valid keys but no definition",
                      output_key, unmatched))
    }
  }

  return(dt_safe)
}

#' Standard Column Rename and Clean
#'
#' @description
#' Renames a column and applies basic cleaning (trim whitespace, uppercase).
#'
#' @param dt data.table containing the data
#' @param old_name character current column name
#' @param new_name character new column name
#' @param to_upper logical whether to convert to uppercase (default: TRUE)
#' @param trim logical whether to trim whitespace (default: TRUE)
#'
#' @return data.table with renamed and cleaned column
#' @export
clean_and_rename <- function(dt, old_name, new_name, to_upper = TRUE, trim = TRUE) {
  dt_safe <- data.table::copy(dt)

  if (!(old_name %in% names(dt_safe))) {
    stop(sprintf("Column '%s' not found", old_name))
  }

  # Get values
  values <- as.character(dt_safe[[old_name]])

  # Clean
  if (trim) {
    values <- trimws(values)
  }
  if (to_upper) {
    values <- toupper(values)
  }

  # Assign to new name
  dt_safe[, (new_name) := values]

  return(dt_safe)
}

#' Create Transformation Summary Statistics
#'
#' @description
#' Creates a summary of a transformation result showing counts of
#' valid, invalid, and missing values.
#'
#' @param dt data.table after transformation
#' @param key_col character name of the key column
#' @param definition_col character name of the definition column
#' @param invalid_values character vector of values considered invalid
#' @param undefined_values character vector of values considered undefined
#'
#' @return list with summary statistics
#' @export
summarize_transformation <- function(dt,
                                     key_col,
                                     definition_col,
                                     invalid_values = c("INVALID"),
                                     undefined_values = c("UNDEFINED", NA_character_, "")) {

  total <- nrow(dt)

  # Count by category
  key_values <- dt[[key_col]]
  def_values <- dt[[definition_col]]

  invalid_count <- sum(key_values %in% invalid_values, na.rm = TRUE)
  undefined_count <- sum(is.na(key_values) | key_values %in% undefined_values, na.rm = TRUE)
  valid_count <- total - invalid_count - undefined_count

  # Definition coverage
  has_definition <- sum(!is.na(def_values) & !(def_values %in% c(invalid_values, undefined_values)))

  list(
    total = total,
    valid = valid_count,
    invalid = invalid_count,
    undefined = undefined_count,
    valid_pct = (valid_count / total) * 100,
    invalid_pct = (invalid_count / total) * 100,
    undefined_pct = (undefined_count / total) * 100,
    definition_coverage = (has_definition / total) * 100
  )
}

#' Wrapper to time a transformation
#'
#' @description
#' Executes a transformation function and logs timing information.
#'
#' @param transform_func function the transformation function to call
#' @param ... arguments to pass to transform_func
#' @param transform_name character name for logging
#'
#' @return result of transform_func
#' @export
timed_transform <- function(transform_func, ..., transform_name = "transformation") {
  start_time <- Sys.time()

  result <- transform_func(...)

  duration <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))

  message(sprintf("[%s] Completed %s in %.2f seconds",
                  format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                  transform_name,
                  duration))

  return(result)
}
