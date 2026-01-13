# ============================================================================
# ico_name.R
# Transform ICO (In-Care-Of) name column
# ============================================================================

# ============================================================================
# Module Constants
# ============================================================================
#
# The ICO field contains names prefixed with "%" followed by a space
# and sometimes additional characters before the actual name.
ICO_PREFIX_PATTERN <- "^%\\s.*"

# Output column names
ICO_OUTPUT_COLS <- c(
  "in_care_of_name_raw",
  "in_care_of_name_clean",
  "in_care_of_name_provided"
)

# ============================================================================
# Transformation Function
# ============================================================================

#' Transform In-Care-Of Name
#'
#' @description
#' Transforms the ICO (In-Care-Of) column from raw BMF data. The ICO field
#' contains the name of a person or organization that handles mail for the
#' nonprofit, typically formatted with a "%" prefix.
#'
#' @param dt data.table containing BMF data with ICO column
#' @param input_col character name of input column (default: "ICO")
#'
#' @return data.table with new columns:
#'   \itemize{
#'     \item in_care_of_name_raw - Original ICO value
#'     \item in_care_of_name_clean - Cleaned and title-cased name
#'     \item in_care_of_name_provided - Logical flag indicating if ICO was provided
#'   }
#'
#' @examples
#' \dontrun{
#' bmf_transformed <- transform_ico_name(bmf_raw)
#' }
#'
#' @export
transform_ico_name <- function(dt, input_col = "ICO") {

  # Input validation
  validate_data_table(dt, input_col, context = "BMF data")

  # Safe copy
  dt_safe <- data.table::copy(dt)

  # Preserve raw value (renamed for clarity)
  dt_safe[, in_care_of_name_raw := as.character(get(input_col))]

  # Clean: remove % prefix pattern
  dt_safe[, in_care_of_name_clean := stringr::str_remove(
    in_care_of_name_raw,
    ICO_PREFIX_PATTERN
  )]

  # Standardize: title case and squish whitespace
  dt_safe[, in_care_of_name_clean := stringr::str_to_title(in_care_of_name_clean)]
  dt_safe[, in_care_of_name_clean := stringr::str_squish(in_care_of_name_clean)]

  # Flag for quality assurance and metadata
  dt_safe[, in_care_of_name_provided := data.table::fifelse(
    is.na(in_care_of_name_raw) | in_care_of_name_raw == "",
    FALSE,
    TRUE
  )]

  # Quality report
  provided_count <- dt_safe[in_care_of_name_provided == TRUE, .N]
  total_rows <- nrow(dt_safe)
  message(sprintf(
    "In-Care-Of name: %s of %s (%0.1f%%) have ICO names provided",
    format(provided_count, big.mark = ","),
    format(total_rows, big.mark = ","),
    100 * provided_count / total_rows
  ))

  return(dt_safe)
}
