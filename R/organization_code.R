# ============================================================================
# organization_code.R
# Transform ORGANIZATION column to organization_code with definition
# ============================================================================

# ============================================================================
# Lookup Table Loading
# ============================================================================

organization_code_lookup <- lookup_ls$organization_code

# ============================================================================
# Transformation Function
# ============================================================================

#' Transform Organization Code
#'
#' @description
#' Transforms the ORGANIZATION column from raw BMF data into a standardized
#' organization_code with its corresponding definition from the lookup table.
#'
#' Organization codes indicate the legal form of the organization
#' (e.g., corporation, trust, association).
#'
#' @param dt data.table containing BMF data with ORGANIZATION column
#' @param input_col character name of input column (default: "ORGANIZATION")
#' @param lookup data.table lookup table with organization_code and definition
#'
#' @note
#' BMF pipeline function. Modifies input in place for efficiency. Caller should pass a copy if original must be preserved.
#' 
#' @return data.table with new columns:
#'   \itemize{
#'     \item organization_code - Standardized organization code (integer)
#'     \item organization_code_definition - Human-readable definition
#'   }
#'
#' @export
transform_bmf_organization_code <- function(dt,
                                        input_col = "ORGANIZATION",
                                        lookup = organization_code_lookup) {
  transform_code(
    dt = dt,
    input_col = input_col,
    lookup_key = "organization_code",
    definition_col = "organization_code_definition",
    type_conversion_func = as.integer,
    lookup = lookup
  )
}
