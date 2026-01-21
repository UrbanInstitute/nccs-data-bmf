# ============================================================================
# transform_tax_period.R
# Transform TAX_PERIOD column to date format
# ============================================================================

# ============================================================================
# Module Constants
# ============================================================================

# Expected format length for tax period (YYYYMM = 6 characters)
TAX_PERIOD_LENGTH <- 6

# Sentinel date for missing tax periods
TAX_PERIOD_MISSING_DATE <- "1900-01-01"

# ============================================================================
# Transformation Function
# ============================================================================

#' Transform Tax Period
#'
#' @description
#' Transforms the TAX_PERIOD column from raw BMF data (YYYYMM format) into
#' a proper date column. Missing values are flagged and replaced with a
#' sentinel date (1900-01-01) for consistent downstream processing.
#'
#' The tax period represents the end date of the organization's tax year.
#'
#' @param dt data.table containing BMF data with TAX_PERIOD column
#' @param input_col character name of input column (default: "TAX_PERIOD")
#'
#' @return data.table with new columns:
#'   \itemize{
#'     \item tax_period_ymd - Tax period as Date (YYYY-MM-01)
#'     \item tax_period_is_missing - Logical flag for missing values
#'   }
#'
#' @examples
#' \dontrun{
#' bmf_transformed <- transform_tax_period(bmf_raw)
#' }
#'
#' @export
transform_tax_period <- function(dt, input_col = "TAX_PERIOD") {

  # Input validation
  validate_data_table(dt, input_col, context = "BMF data")

  # Safe copy
  dt_safe <- data.table::copy(dt)

  # Convert to string and preserve original
  dt_safe[, tax_period_ym_str := as.character(get(input_col))]

  # Validate format: must be 6 characters (YYYYMM)
  invalid_length <- dt_safe[
    !is.na(tax_period_ym_str) & nchar(tax_period_ym_str) != TAX_PERIOD_LENGTH,
    .N
  ]

  if (invalid_length > 0) {
    stop(sprintf(
      "Column '%s' contains %d values with invalid length. Expected %d characters (YYYYMM format).",
      input_col, invalid_length, TAX_PERIOD_LENGTH
    ))
  }


  # Parse tax period: YYYYMM -> YYYY-MM-01
  dt_safe[, tax_period_ymd := lubridate::ymd(
    paste0(tax_period_ym_str, "01")
  )]

  # Flag missing values
  dt_safe[, tax_period_is_missing := is.na(tax_period_ymd)]

  # Count missing for quality report
  missing_count <- dt_safe[tax_period_is_missing == TRUE, .N]
  if (missing_count > 0) {
    message(sprintf(
      "Tax period: %s missing values replaced with sentinel date %s",
      format(missing_count, big.mark = ","),
      TAX_PERIOD_MISSING_DATE
    ))
  }

  # Replace NA with sentinel date
  dt_safe[is.na(tax_period_ymd),
          tax_period_ymd := lubridate::ymd(TAX_PERIOD_MISSING_DATE)]

  return(dt_safe)
}
