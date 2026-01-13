# ============================================================================
# foundation_code.R
# Transform FOUNDATION column to foundation_code with definition
# ============================================================================

# ============================================================================
# Lookup Table Loading
# ============================================================================

foundation_code_lookup <- data.table::fread(
  here::here("data/lookup/foundation_code_lookup.csv"),
  colClasses = list(integer = "foundation_code")
)

# ============================================================================
# Transformation Function
# ============================================================================

#' Transform Foundation Code
#'
#' @description
#' Transforms the FOUNDATION column from raw BMF data into a standardized
#' foundation_code with its corresponding definition from the lookup table.
#'
#' Foundation codes indicate the private foundation status of the organization
#' and its classification under IRC Section 509(a).
#'
#' @param dt data.table containing BMF data with FOUNDATION column
#' @param input_col character name of input column (default: "FOUNDATION")
#' @param lookup data.table lookup table with foundation_code and definition
#'
#' @return data.table with new columns:
#'   \itemize{
#'     \item foundation_code - Standardized foundation code (integer)
#'     \item foundation_code_definition - Human-readable definition
#'   }
#'
#' @export
transform_foundation_code <- function(dt,
                                      input_col = "FOUNDATION",
                                      lookup = foundation_code_lookup) {
  transform_code(
    dt = dt,
    input_col = input_col,
    lookup_key = "foundation_code",
    definition_col = "foundation_code_definition",
    type_conversion_func = as.integer,
    lookup = lookup
  )
}
