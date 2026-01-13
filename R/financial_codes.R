# ============================================================================
# financial_codes.R
# Transform financial code columns: Asset code (ASSET_CD) and Income code (INCOME_CD)
# ============================================================================

# ============================================================================
# Transformation Function
# ============================================================================

#' Transform Financial Code Column
#'
#' @description
#' Generic function to transform financial code columns (ASSET_CD, INCOME_CD)
#' by converting to integer and joining with lookup tables for definitions.
#'
#' Asset and income codes are derived from the organization's total assets
#' and total income, bucketed into ranges defined by the IRS.
#'
#' @param dt data.table containing BMF data with the financial code column
#' @param lookup data.table lookup table with code and definition columns
#' @param input_col character name of input column (e.g., "ASSET_CD", "INCOME_CD")
#' @param lookup_key character name of the key column in lookup table
#' @param definition_col character name of the definition column to add
#'
#' @return data.table with new columns:
#'   \itemize{
#'     \item {lookup_key} - Standardized code (integer)
#'     \item {definition_col} - Human-readable definition
#'   }
#'
#' @examples
#' \dontrun{
#' # Transform asset code
#' bmf <- transform_financial_code(
#'   dt = bmf_raw,
#'   lookup = lookup_ls$asset_code,
#'   input_col = "ASSET_CD",
#'   lookup_key = "asset_code",
#'   definition_col = "asset_code_definition"
#' )
#'
#' # Transform income code
#' bmf <- transform_financial_code(
#'   dt = bmf_raw,
#'   lookup = lookup_ls$income_code,
#'   input_col = "INCOME_CD",
#'   lookup_key = "income_code",
#'   definition_col = "income_code_definition"
#' )
#' }
#'
#' @export
transform_financial_code <- function(dt,
                                     lookup,
                                     input_col,
                                     lookup_key,
                                     definition_col) {

  # Input validation
  validate_data_table(dt, input_col, context = "BMF data")
  validate_lookup(lookup, c(lookup_key, definition_col),
                  lookup_name = sprintf("%s lookup", lookup_key))

  # Safe copies
  dt_safe <- data.table::copy(dt)
  lookup_safe <- data.table::copy(lookup)

  # Type conversion with error trapping
  # Convert to character first, then integer to handle mixed types
  original_values <- dt_safe[[input_col]]
  dt_safe[, (lookup_key) := suppressWarnings(
    as.integer(as.character(get(input_col)))
  )]

  # Validate type conversion
  conversion_failures <- sum(!is.na(original_values) & is.na(dt_safe[[lookup_key]]))
  if (conversion_failures > 0) {
    stop(sprintf(
      "%s type conversion failed: %d non-numeric values detected",
      input_col, conversion_failures
    ))
  }

  # Set keys for efficient join
  data.table::setkeyv(lookup_safe, lookup_key)
  data.table::setkeyv(dt_safe, lookup_key)

  # Perform lookup join
  dt_safe[lookup_safe, (definition_col) := get(paste0("i.", definition_col))]

  # Validate join success
  validate_join_success(
    dt_safe,
    key_col = lookup_key,
    result_col = definition_col,
    lookup_name = sprintf("%s lookup", lookup_key)
  )

  return(dt_safe)
}

# ============================================================================
# Convenience Wrappers
# ============================================================================

#' Transform Asset Code
#'
#' @description
#' Transforms the ASSET_CD column from raw BMF data into a standardized
#' asset_code with its corresponding definition.
#'
#' @param dt data.table containing BMF data with ASSET_CD column
#' @param lookup data.table asset code lookup table
#'
#' @return data.table with asset_code and asset_code_definition columns
#'
#' @export
transform_asset_code <- function(dt, lookup) {
  transform_financial_code(
    dt = dt,
    lookup = lookup,
    input_col = "ASSET_CD",
    lookup_key = "asset_code",
    definition_col = "asset_code_definition"
  )
}

#' Transform Income Code
#'
#' @description
#' Transforms the INCOME_CD column from raw BMF data into a standardized
#' income_code with its corresponding definition.
#'
#' @param dt data.table containing BMF data with INCOME_CD column
#' @param lookup data.table income code lookup table
#'
#' @return data.table with income_code and income_code_definition columns
#'
#' @export
transform_income_code <- function(dt, lookup) {
  transform_financial_code(
    dt = dt,
    lookup = lookup,
    input_col = "INCOME_CD",
    lookup_key = "income_code",
    definition_col = "income_code_definition"
  )
}
