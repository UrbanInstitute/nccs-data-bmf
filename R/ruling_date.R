# ============================================================================
# ruling_date.R
# Transform RULING column (IRS ruling/determination date)
# ============================================================================

# ============================================================================
# Module Constants
# ============================================================================

# Expected format: YYYYMM (6 characters)
RULING_DATE_LENGTH <- 6

# Sentinel date for missing/invalid ruling dates
RULING_DATE_MISSING <- as.Date("1900-01-01")

# ============================================================================
# Transformation Function
# ============================================================================

#' Transform Ruling Date
#'
#' @description
#' Transforms the RULING column from raw BMF data (YYYYMM format) into a
#' proper Date column. The ruling date represents when the IRS issued the
#' organization's tax-exempt determination letter.
#'
#' Missing or invalid values are flagged and replaced with a sentinel date
#' (1900-01-01) for consistent downstream processing.
#'
#' @param dt data.table containing BMF data with RULING column
#' @param input_col character name of input column (default: "RULING")
#'
#' @return data.table with new columns:
#'   \itemize{
#'     \item ruling_date_ym_str - Original YYYYMM value as string
#'     \item ruling_date - Parsed Date (YYYY-MM-01)
#'     \item ruling_date_is_missing - Logical flag for missing/invalid values
#'   }
#'
#' @examples
#' \dontrun{
#' bmf_transformed <- transform_ruling_date(bmf_raw)
#' }
#'
#' @export
transform_ruling_date <- function(dt, input_col = "RULING") {

  # Input validation
  validate_data_table(dt, input_col, context = "BMF data")

  # Safe copy
  dt_safe <- data.table::copy(dt)

  # Convert to string and preserve original
  dt_safe[, ruling_date_ym_str := as.character(get(input_col))]

  # Validate format before parsing
  invalid_format <- dt_safe[
    !is.na(ruling_date_ym_str) &
      ruling_date_ym_str != "" &
      ruling_date_ym_str != "0" &
      nchar(ruling_date_ym_str) != RULING_DATE_LENGTH,
    .N
  ]

  if (invalid_format > 0) {
    warning(sprintf(
      "RULING column contains %d values with non-standard length (expected %d characters)",
      invalid_format, RULING_DATE_LENGTH
    ))
  }

  # Parse to date: YYYYMM -> YYYY-MM-01
  dt_safe[, ruling_date := lubridate::ymd(
    paste0(ruling_date_ym_str, "01"),
    quiet = TRUE
  )]

  # Flag missing values
  dt_safe[, ruling_date_is_missing := is.na(ruling_date) |
            ruling_date_ym_str == "" |
            ruling_date_ym_str == "0"]

  # Count missing for quality report
  missing_count <- dt_safe[ruling_date_is_missing == TRUE, .N]
  if (missing_count > 0) {
    message(sprintf(
      "Ruling date: %s missing/invalid values flagged",
      format(missing_count, big.mark = ",")
    ))
  }

  # Replace NA with sentinel date
  dt_safe[is.na(ruling_date), ruling_date := RULING_DATE_MISSING]

  # Quality report
  total_rows <- nrow(dt_safe)
  valid_count <- total_rows - missing_count
  message(sprintf(
    "Ruling date: %s of %s (%0.1f%%) successfully parsed",
    format(valid_count, big.mark = ","),
    format(total_rows, big.mark = ","),
    100 * valid_count / total_rows
  ))

  return(dt_safe)
}
