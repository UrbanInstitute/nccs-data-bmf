# ============================================================================
# filing_requirement_code.R
# Filing Requirement Code Transformation
# ============================================================================

# ============================================================================
# Lookup Table Loading
# ============================================================================

filing_requirement_code_lookup <- lookup_ls$filing_requirement_code
pf_filing_requirement_code_lookup <- lookup_ls$pf_filing_requirement_code

# ============================================================================
# Transformation functions
# ============================================================================
#' Transform Filing Requirement Code
#'
#' @description
#' Transforms the FILING_REQ_CD column from raw BMF data into a standardized
#' filing_requirement_code with its corresponding definition.
#'
#' Filing requirement codes indicate the annual filing requirements
#' for the organization (e.g., Form 990, 990-EZ, 990-N).
#'
#' @param dt data.table containing BMF data with FILING_REQ_CD column
#' @param input_col character name of input column (default: "FILING_REQ_CD")
#' @param lookup data.table lookup table with filing requirement codes
#'
#' @return data.table with new columns:
#'   \itemize{
#'     \item filing_requirement_code - Standardized code (integer)
#'     \item filing_requirement_code_definition - Human-readable definition
#'   }
#'
#' @export
transform_bmf_filing_requirement_code <- function(
    dt,
    input_col = "FILING_REQ_CD",
    lookup = filing_requirement_code_lookup
  ) {
  transform_code(
    dt = dt,
    input_col = input_col,
    lookup_key = "filing_requirement_code",
    definition_col = "filing_requirement_code_definition",
    type_conversion_func = as.integer,
    lookup = lookup
  )
}

#' Transform Private Foundation Filing Requirement Code
#'
#' @description
#' Transforms the PF_FILING_REQ_CD column from raw BMF data into a standardized
#' pf_filing_requirement_code with its corresponding definition.
#'
#' @param dt data.table containing BMF data with PF_FILING_REQ_CD column
#' @param input_col character name of input column (default: "PF_FILING_REQ_CD")
#' @param lookup data.table lookup table with PF filing requirement codes
#'
#' @return data.table with new columns:
#'   \itemize{
#'     \item pf_filing_requirement_code - Standardized code (integer)
#'     \item pf_filing_requirement_code_definition - Human-readable definition
#'   }
#'
#' @export
transform_bmf_pf_filing_requirement_code <- function(
    dt,
    input_col = "PF_FILING_REQ_CD",
    lookup = pf_filing_requirement_code_lookup
  ) {
  transform_code(
    dt = dt,
    input_col = input_col,
    lookup_key = "pf_filing_requirement_code",
    definition_col = "pf_filing_requirement_code_definition",
    type_conversion_func = as.integer,
    lookup = lookup
  )
}
