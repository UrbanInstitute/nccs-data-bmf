# ============================================================================
# deductibility_code.R
# Transform DEDUCTIBILITY column to deductibility_code with definition
# ============================================================================

# ============================================================================
# Lookup Table Loading
# ============================================================================

deductibility_code_lookup <- data.table::fread(
  here::here("data/lookup/deductibility_code_lookup.csv"),
  colClasses = list(integer = "deductibility_code")
)

# ============================================================================
# Transformation Function
# ============================================================================

#' Transform Deductibility Code
#'
#' @description
#' Transforms the DEDUCTIBILITY column from raw BMF data into a standardized
#' deductibility_code with its corresponding definition from the lookup table.
#'
#' Deductibility codes indicate whether contributions to the organization
#' are tax-deductible and under what conditions.
#'
#' @param dt data.table containing BMF data with DEDUCTIBILITY column
#' @param input_col character name of input column (default: "DEDUCTIBILITY")
#' @param lookup data.table lookup table with deductibility_code and definition
#'
#' @return data.table with new columns:
#'   \itemize{
#'     \item deductibility_code - Standardized deductibility code (integer)
#'     \item deductibility_code_definition - Human-readable definition
#'   }
#'
#' @export
transform_deductibility_code <- function(dt,
                                         input_col = "DEDUCTIBILITY",
                                         lookup = deductibility_code_lookup) {
  transform_code(
    dt = dt,
    input_col = input_col,
    lookup_key = "deductibility_code",
    definition_col = "deductibility_code_definition",
    type_conversion_func = as.integer,
    lookup = lookup
  )
}
