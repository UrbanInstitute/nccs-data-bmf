# ============================================================================
# organization_code.R
# Transform ORGANIZATION column to organization_code with definition
# ============================================================================

# ============================================================================
# Module Constants
# ============================================================================

ORGANIZATION_CODE_LOOKUP_COLS <- c("organization_code", "organization_code_definition")

# ============================================================================
# Lookup Table Loading
# ============================================================================

organization_code_lookup <- data.table::fread(
  organization_code_lookup_path,
  select = ORGANIZATION_CODE_LOOKUP_COLS,
  colClasses = list(integer = "organization_code")
)

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
#' @return data.table with new columns:
#'   \itemize{
#'     \item organization_code - Standardized organization code (integer)
#'     \item organization_code_definition - Human-readable definition
#'   }
#'
#' @examples
#' \dontrun{
#' bmf_transformed <- transform_organization_code(bmf_raw)
#' }
#'
#' @export
transform_organization_code <- function(dt,
                                        input_col = "ORGANIZATION",
                                        lookup = organization_code_lookup) {

  # Input validation
  validate_data_table(dt, input_col, context = "BMF data")
  validate_lookup(lookup, ORGANIZATION_CODE_LOOKUP_COLS, "organization_code_lookup")

  # Safe copy
  dt_safe <- data.table::copy(dt)

  # Type conversion
  dt_safe[, organization_code := suppressWarnings(as.integer(get(input_col)))]

  # Lookup join
  dt_safe[lookup,
          organization_code_definition := i.organization_code_definition,
          on = .(organization_code)]

  # Validate join success
  validate_join_success(
    dt_safe,
    key_col = "organization_code",
    result_col = "organization_code_definition",
    lookup_name = "organization_code_lookup"
  )

  return(dt_safe)
}
