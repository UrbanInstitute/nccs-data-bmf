# ============================================================================
# ein.R
# Transform EIN (Employer Identification Number) column
# ============================================================================

# ============================================================================
# Module Constants
# ============================================================================

# EIN format: 9 digits, displayed as XX-XXXXXXX
EIN_LENGTH <- 9
EIN_FORMAT_PATTERN <- "^(..)(.*)$"
EIN_FORMAT_REPLACEMENT <- "\\1-\\2"

# ============================================================================
# Helper Functions
# ============================================================================

#' Clean and Format EIN
#'
#' @description
#' Cleans an EIN by removing non-numeric characters, padding to 9 digits,
#' and formatting with the standard XX-XXXXXXX pattern.
#'
#' @param ein_vector character or numeric vector of EIN values
#'
#' @return character vector of formatted EINs (XX-XXXXXXX format)
#'
#' @examples
#' \dontrun{
#' clean_and_format_ein(c("123456789", "12-3456789", "1234567"))
#' # Returns: c("12-3456789", "12-3456789", "00-1234567")
#' }
#'
#' @noRd
.clean_and_format_ein <- function(ein_vector) {
  # Remove all non-numeric characters
  clean_ein <- stringr::str_remove_all(ein_vector, "[^0-9]")

  # Pad to 9 digits
  padded_ein <- stringr::str_pad(
    clean_ein,
    width = EIN_LENGTH,
    side = "left",
    pad = "0"
  )

  # Format as XX-XXXXXXX
  formatted_ein <- stringr::str_replace(
    padded_ein,
    EIN_FORMAT_PATTERN,
    EIN_FORMAT_REPLACEMENT
  )

  return(formatted_ein)
}

# ============================================================================
# Transformation Function
# ============================================================================

#' Transform EIN Column
#'
#' @description
#' Transforms the EIN column from raw BMF data into a standardized format.
#' The raw EIN may contain hyphens or other characters; this function
#' normalizes all values to the XX-XXXXXXX format.
#'
#' @param dt data.table containing BMF data with EIN column
#' @param input_col character name of input column (default: "EIN")
#'
#' @return data.table with new columns:
#'   \itemize{
#'     \item ein_raw - Original EIN value
#'     \item ein - Formatted EIN (XX-XXXXXXX)
#'   }
#'
#' @examples
#' \dontrun{
#' bmf_transformed <- transform_ein(bmf_raw)
#' }
#'
#' @export
transform_ein <- function(dt, input_col = "EIN") {

  # Input validation
  validate_data_table(dt, input_col, context = "BMF data")

  # Safe copy
  dt_safe <- data.table::copy(dt)

  # Preserve raw value
  dt_safe[, ein_raw := as.character(get(input_col))]

  # Clean and format
  dt_safe[, ein := .clean_and_format_ein(ein_raw)]

  # Validate: check for any that are still malformed
  invalid_count <- dt_safe[!grepl("^\\d{2}-\\d{7}$", ein), .N]
  if (invalid_count > 0) {
    warning(sprintf(
      "EIN transformation: %d values do not match expected format XX-XXXXXXX",
      invalid_count
    ))
  }

  # Quality report
  message(sprintf(
    "EIN: %s records processed",
    format(nrow(dt_safe), big.mark = ",")
  ))

  return(dt_safe)
}

# ============================================================================
# Backward Compatibility
# ============================================================================

#' @rdname transform_ein
#' @description Legacy alias for .clean_and_format_ein()
#' @export
clean_and_format_ein <- function(ein_vector) {
  .clean_and_format_ein(ein_vector)
}
