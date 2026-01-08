# ============================================================================
# affiliation_code.R
# Transform AFFILIATION column to affiliation_code with definition
# ============================================================================

# ============================================================================
# Module Constants
# ============================================================================

AFFILIATION_CODE_LOOKUP_COLS <- c("affiliation_code", "affiliation_code_definition")

# ============================================================================
# Lookup Table Loading
# ============================================================================

affiliation_code_lookup <- data.table::fread(
  affiliation_code_lookup_path,
  select = AFFILIATION_CODE_LOOKUP_COLS,
  colClasses = list(character = "affiliation_code")
)

# ============================================================================
# Transformation Function
# ============================================================================

#' Transform Affiliation Code
#'
#' @description
#' Transforms the AFFILIATION column from raw BMF data into a standardized
#' affiliation_code with its corresponding definition from the lookup table.
#'
#' Affiliation codes indicate the relationship between an organization and
#' a group exemption holder (e.g., independent, central, subordinate).
#'
#' @param dt data.table containing BMF data with AFFILIATION column
#' @param input_col character name of input column (default: "AFFILIATION")
#' @param lookup data.table lookup table with affiliation_code and definition
#'
#' @return data.table with new columns:
#'   \itemize{
#'     \item affiliation_code - Standardized affiliation code (character)
#'     \item affiliation_code_definition - Human-readable definition
#'   }
#'
#' @examples
#' \dontrun{
#' bmf_transformed <- transform_affiliation_code(bmf_raw)
#' }
#'
#' @export
transform_affiliation_code <- function(dt,
                                       input_col = "AFFILIATION",
                                       lookup = affiliation_code_lookup) {

 # Input validation
  validate_data_table(dt, input_col, context = "BMF data")
  validate_lookup(lookup, AFFILIATION_CODE_LOOKUP_COLS, "affiliation_code_lookup")

  # Safe copy
  dt_safe <- data.table::copy(dt)

  # Type conversion (affiliation codes are single-digit characters)
  dt_safe[, affiliation_code := as.character(get(input_col))]

  # Lookup join
  dt_safe[lookup,
          affiliation_code_definition := i.affiliation_code_definition,
          on = .(affiliation_code)]

  # Validate join success
  validate_join_success(
    dt_safe,
    key_col = "affiliation_code",
    result_col = "affiliation_code_definition",
    lookup_name = "affiliation_code_lookup"
  )

  return(dt_safe)
}
