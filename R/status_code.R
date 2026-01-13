# ============================================================================
# status_code.R
# Transform STATUS column to status_code with definition
# ============================================================================

#' Transform Status Code
#'
#' @description
#' Transforms the STATUS column from raw BMF data into a standardized
#' status_code with its corresponding definition from the lookup table.
#'
#' Status codes indicate the current exempt status of the organization
#' (e.g., unconditional exemption, conditional exemption, revoked).
#'
#' @param dt data.table containing BMF data with STATUS column
#' @param input_col character name of input column (default: "STATUS")
#' @param lookup data.table lookup table with status_code and definition
#'   (typically lookup_ls$status_code from config.R)
#'
#' @return data.table with new columns:
#'   \itemize{
#'     \item status_code - Standardized status code (integer)
#'     \item status_code_definition - Human-readable definition
#'   }
#'
#' @export
transform_status_code <- function(dt,
                                  input_col = "STATUS",
                                  lookup) {
  transform_code(
    dt = dt,
    input_col = input_col,
    lookup_key = "status_code",
    definition_col = "status_code_definition",
    type_conversion_func = as.integer,
    lookup = lookup
  )
}
