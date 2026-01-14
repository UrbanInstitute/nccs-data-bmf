# ============================================================================
# affiliation_code.R
# Transform AFFILIATION column to affiliation_code with definition
# ============================================================================

# ============================================================================
# Lookup Table Loading
# ============================================================================

affiliation_code_lookup <- lookup_ls$affiliation_code[
  ,
  .(affiliation_code = as.character(affiliation_code),
    affiliation_code_definition)
]

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
#' @export
transform_affiliation_code <- function(dt,
                                       input_col = "AFFILIATION",
                                       lookup = affiliation_code_lookup) {
  transform_code(
    dt = dt,
    input_col = input_col,
    lookup_key = "affiliation_code",
    definition_col = "affiliation_code_definition",
    type_conversion_func = as.character,
    lookup = lookup
  )
}
