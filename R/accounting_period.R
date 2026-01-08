# ============================================================================
# accounting_period.R
# Transform ACCT_PD column to accounting_period
# ============================================================================

# ============================================================================
# Module Constants
# ============================================================================

# Valid accounting period range (month of fiscal year end, 0 = undefined)
ACCOUNTING_PERIOD_MIN <- 0
ACCOUNTING_PERIOD_MAX <- 12

# ============================================================================
# Transformation Function
# ============================================================================

#' Transform Accounting Period
#'
#' @description
#' Transforms the ACCT_PD column from raw BMF data into a standardized
#' accounting_period integer representing the fiscal year end month.
#'
#' Valid values are 1-12 (representing January-December) or 0 for undefined.
#' The function validates that all values fall within this range.
#'
#' @param dt data.table containing BMF data with ACCT_PD column
#' @param input_col character name of input column (default: "ACCT_PD")
#'
#' @return data.table with new column:
#'   \itemize{
#'     \item accounting_period - Fiscal year end month (integer 0-12)
#'   }
#'
#' @examples
#' \dontrun{
#' bmf_transformed <- transform_accounting_period(bmf_raw)
#' }
#'
#' @export
transform_accounting_period <- function(dt, input_col = "ACCT_PD") {

  # Input validation
  validate_data_table(dt, input_col, context = "BMF data")

  # Safe copy
  dt_safe <- data.table::copy(dt)

  # Type conversion
  dt_safe[, accounting_period := suppressWarnings(as.integer(get(input_col)))]

  # Range validation
  invalid_codes <- dt_safe[
    !is.na(accounting_period) &
      !(accounting_period >= ACCOUNTING_PERIOD_MIN & accounting_period <= ACCOUNTING_PERIOD_MAX),
    unique(accounting_period)
  ]

  if (length(invalid_codes) > 0) {
    stop(sprintf(
      "Invalid accounting periods detected: %s. Valid range is %d-%d.",
      paste(invalid_codes, collapse = ", "),
      ACCOUNTING_PERIOD_MIN,
      ACCOUNTING_PERIOD_MAX
    ))
  }

  return(dt_safe)
}
