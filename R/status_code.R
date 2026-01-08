# ============================================================================
# status_code.R
# Transform STATUS column to status_code with definition
# ============================================================================

# ============================================================================
# Module Constants
# ============================================================================

STATUS_CODE_LOOKUP_COLS <- c("status_code", "status_code_definition")

# ============================================================================
# Transformation Function
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
#'
#' @return data.table with new columns:
#'   \itemize{
#'     \item status_code - Standardized status code (integer)
#'     \item status_code_definition - Human-readable definition
#'   }
#'
#' @examples
#' \dontrun{
#' bmf_transformed <- transform_status_code(bmf_raw, lookup = lookup_ls$status_code)
#' }
#'
#' @export
transform_status_code <- function(dt,
                                  input_col = "STATUS",
                                  lookup) {

  # Input validation
  validate_data_table(dt, input_col, context = "BMF data")
  validate_lookup(lookup, STATUS_CODE_LOOKUP_COLS, "status_code_lookup")

  # Safe copy
  dt_safe <- data.table::copy(dt)

  # Type conversion
  dt_safe[, status_code := suppressWarnings(as.integer(get(input_col)))]

  # Lookup join
  dt_safe[lookup,
          status_code_definition := i.status_code_definition,
          on = .(status_code)]

  # Validate join success
  validate_join_success(
    dt_safe,
    key_col = "status_code",
    result_col = "status_code_definition",
    lookup_name = "status_code_lookup"
  )

  return(dt_safe)
}
