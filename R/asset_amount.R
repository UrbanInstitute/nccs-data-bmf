# ============================================================================
# asset_amount.R
# Transform financial amount columns (ASSET_AMT, INCOME_AMT, REVENUE_AMT)
# ============================================================================

# ============================================================================
# Transformation Function
# ============================================================================

#' Transform Financial Amount Column
#'
#' @description
#' Transforms financial amount columns (ASSET_AMT, INCOME_AMT, REVENUE_AMT)
#' by converting to numeric format with validation. Checks for negative values
#' and failed type conversions.
#'
#' @param dt data.table containing BMF data with the financial amount column
#' @param input_col character name of input column (e.g., "ASSET_AMT")
#' @param output_col character name of output column (e.g., "asset_amount")
#' @param allow_negative logical whether to allow negative values without warning
#'   (default: FALSE). Some fields like INCOME_AMT can legitimately be negative.
#'
#' @return data.table with new column containing numeric financial amount
#'
#' @examples
#' \dontrun{
#' # Transform asset amount (negative values unexpected)
#' bmf <- transform_financials(bmf_raw, "ASSET_AMT", "asset_amount")
#'
#' # Transform income amount (negative values allowed)
#' bmf <- transform_financials(bmf_raw, "INCOME_AMT", "income_amount",
#'                             allow_negative = TRUE)
#' }
#'
#' @export
transform_financials <- function(dt,
                                 input_col,
                                 output_col,
                                 allow_negative = FALSE) {

  # Input validation
  validate_data_table(dt, input_col, context = "BMF data")

  # Safe copy
  dt_safe <- data.table::copy(dt)

  # Check for negative values before conversion
  if (!allow_negative) {
    negative_values <- dt_safe[get(input_col) < 0, unique(get(input_col))]
    if (length(negative_values) > 0) {
      warning(sprintf(
        "Column '%s' contains %d unique negative values: %s",
        input_col,
        length(negative_values),
        paste(head(negative_values, 5), collapse = ", ")
      ))
    }
  }

  # Type conversion
  dt_safe[, (output_col) := suppressWarnings(as.numeric(get(input_col)))]

  # Check for conversion failures (non-NA input became NA output)
  failed_conversions <- dt_safe[
    is.na(get(output_col)) & !is.na(get(input_col)) & get(input_col) != "",
    unique(get(input_col))
  ]

  if (length(failed_conversions) > 0) {
    stop(sprintf(
      "Column '%s' conversion failed for %d non-numeric values: %s. Check source data.",
      input_col,
      length(failed_conversions),
      paste(head(failed_conversions, 5), collapse = ", ")
    ))
  }

  return(dt_safe)
}

# ============================================================================
# Convenience Wrappers
# ============================================================================

#' Transform Asset Amount
#'
#' @description
#' Transforms the ASSET_AMT column to numeric asset_amount.
#' Negative values trigger a warning (unexpected for assets).
#'
#' @param dt data.table containing BMF data with ASSET_AMT column
#'
#' @return data.table with asset_amount column
#'
#' @export
transform_asset_amount <- function(dt) {
  transform_financials(dt, "ASSET_AMT", "asset_amount", allow_negative = FALSE)
}

#' Transform Income Amount
#'
#' @description
#' Transforms the INCOME_AMT column to numeric income_amount.
#' Negative values are allowed (can represent losses).
#'
#' @param dt data.table containing BMF data with INCOME_AMT column
#'
#' @return data.table with income_amount column
#'
#' @export
transform_income_amount <- function(dt) {
  transform_financials(dt, "INCOME_AMT", "income_amount", allow_negative = TRUE)
}

#' Transform Revenue Amount
#'
#' @description
#' Transforms the REVENUE_AMT column to numeric revenue_amount.
#' Negative values are allowed (can represent adjustments).
#'
#' @param dt data.table containing BMF data with REVENUE_AMT column
#'
#' @return data.table with revenue_amount column
#'
#' @export
transform_revenue_amount <- function(dt) {
  transform_financials(dt, "REVENUE_AMT", "revenue_amount", allow_negative = TRUE)
}
