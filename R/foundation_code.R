# ============================================================================
# foundation_code.R
# Transform FOUNDATION column to foundation_code with definition
# ============================================================================

# ============================================================================
# Module Constants
# ============================================================================

FOUNDATION_CODE_LOOKUP_COLS <- c("foundation_code", "foundation_code_definition")

# ============================================================================
# Lookup Table Loading
# ============================================================================

foundation_code_lookup <- data.table::fread(
  foundation_code_lookup_path,
  select = FOUNDATION_CODE_LOOKUP_COLS,
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
#' @examples
#' \dontrun{
#' bmf_transformed <- transform_foundation_code(bmf_raw)
#' }
#'
#' @export
transform_foundation_code <- function(dt,
                                      input_col = "FOUNDATION",
                                      lookup = foundation_code_lookup) {

  # Input validation
  validate_data_table(dt, input_col, context = "BMF data")
  validate_lookup(lookup, FOUNDATION_CODE_LOOKUP_COLS, "foundation_code_lookup")

  # Safe copy
  dt_safe <- data.table::copy(dt)

  # Type conversion
  dt_safe[, foundation_code := suppressWarnings(as.integer(get(input_col)))]

  # Lookup join
  dt_safe[lookup,
          foundation_code_definition := i.foundation_code_definition,
          on = .(foundation_code)]

  # Validate join success
  validate_join_success(
    dt_safe,
    key_col = "foundation_code",
    result_col = "foundation_code_definition",
    lookup_name = "foundation_code_lookup"
  )

  return(dt_safe)
}
