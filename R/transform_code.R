# ============================================================================
# Generic Transformation Function
# ============================================================================

#' Generic Code Transformation with Lookup
#'
#' @description
#' A reusable function for transforming coded columns by joining with
#' lookup tables. Includes type conversion validation and post-join checks.
#'
#' This function is used internally by multiple specific transformations
#' (filing_requirement_code, pf_filing_requirement_code, etc.)
#'
#' @param dt data.table containing the input data
#' @param input_col character name of the input column
#' @param lookup_key character name of the key column in the lookup table
#' @param definition_col character name of the definition column to add
#' @param type_conversion_func function for type conversion (default: as.integer)
#' @param lookup data.table lookup table
#'
#' @return data.table with lookup_key and definition_col columns added
#'
#' @examples
#' \dontrun{
#' result <- transform_code(
#'   dt = bmf_data,
#'   input_col = "FILING_REQ_CD",
#'   lookup_key = "filing_requirement_code",
#'   definition_col = "filing_requirement_code_definition",
#'   type_conversion_func = as.integer,
#'   lookup = lookup_ls$filing_requirement_code
#' )
#' }
#'
#' @export
transform_code <- function(dt,
                           input_col,
                           lookup_key,
                           definition_col,
                           type_conversion_func = as.integer,
                           lookup) {
  
  # Input validation
  if (!(input_col %in% names(dt))) {
    stop(sprintf("Input column '%s' not found in data.table", input_col))
  }
  if (!(lookup_key %in% names(lookup))) {
    stop(sprintf("Lookup key '%s' not found in lookup table", lookup_key))
  }
  if (!(definition_col %in% names(lookup))) {
    stop(sprintf("Definition column '%s' not found in lookup table", definition_col))
  }
  
  # Safe copies
  dt_safe <- data.table::copy(dt)
  lookup_safe <- data.table::copy(lookup)
  
  # Type conversion with validation
  original_values <- dt_safe[[input_col]]
  dt_safe[, (lookup_key) := suppressWarnings(type_conversion_func(get(input_col)))]
  converted_values <- dt_safe[[lookup_key]]
  
  # Check for conversion failures
  conversion_failures <- sum(!is.na(original_values) & is.na(converted_values))
  if (conversion_failures > 0) {
    stop(sprintf("%s type conversion failed: %d values could not be converted",
                 input_col, conversion_failures))
  }
  
  # Set keys for efficient join
  data.table::setkeyv(dt_safe, lookup_key)
  data.table::setkeyv(lookup_safe, lookup_key)
  
  # Perform lookup join
  dt_safe[lookup_safe, (definition_col) := get(paste0("i.", definition_col))]
  
  # Post-join validation
  unmatched <- dt_safe[!is.na(get(lookup_key)) & is.na(get(definition_col)), .N]
  if (unmatched > 0) {
    warning(sprintf("Lookup join for '%s': %d rows with valid keys missing from lookup table",
                    lookup_key, unmatched))
  }
  
  return(dt_safe)
}