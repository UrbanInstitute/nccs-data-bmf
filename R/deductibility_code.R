# ============================================================================
# deductibility_code.R
# Transform DEDUCTIBILITY column to deductibility_code with definition
# ============================================================================

# ============================================================================
# Module Constants
# ============================================================================

DEDUCTIBILITY_CODE_LOOKUP_COLS <- c("deductibility_code", "deductibility_code_definition")

# ============================================================================
# Lookup Table Loading
# ============================================================================

deductibility_code_lookup <- data.table::fread(
  deductibility_code_lookup_path,
  select = DEDUCTIBILITY_CODE_LOOKUP_COLS,
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
#' @examples
#' \dontrun{
#' bmf_transformed <- transform_deductibility_code(bmf_raw)
#' }
#'
#' @export
transform_deductibility_code <- function(dt,
                                         input_col = "DEDUCTIBILITY",
                                         lookup = deductibility_code_lookup) {

  # Input validation
  validate_data_table(dt, input_col, context = "BMF data")
  validate_lookup(lookup, DEDUCTIBILITY_CODE_LOOKUP_COLS, "deductibility_code_lookup")

  # Safe copy
  dt_safe <- data.table::copy(dt)

  # Type conversion
  dt_safe[, deductibility_code := suppressWarnings(as.integer(get(input_col)))]

  # Lookup join
  dt_safe[lookup,
          deductibility_code_definition := i.deductibility_code_definition,
          on = .(deductibility_code)]

  # Validate join success
  validate_join_success(
    dt_safe,
    key_col = "deductibility_code",
    result_col = "deductibility_code_definition",
    lookup_name = "deductibility_code_lookup"
  )

  return(dt_safe)
}
