# ============================================================================
# pre_checks.R
# Pre-transformation quality validation for raw BMF data
# ============================================================================

# ============================================================================
# Module Constants
# ============================================================================

# Expected columns in raw BMF file
BMF_REQUIRED_COLUMNS <- c(
  "EIN", "NAME", "ICO", "STREET", "CITY", "STATE", "ZIP",
  "GROUP", "SUBSECTION", "AFFILIATION", "CLASSIFICATION",
  "RULING", "DEDUCTIBILITY", "FOUNDATION", "ACTIVITY",
  "ORGANIZATION", "STATUS", "TAX_PERIOD", "ASSET_CD",
  "INCOME_CD", "FILING_REQ_CD", "PF_FILING_REQ_CD",
  "ASSET_AMT", "INCOME_AMT", "REVENUE_AMT", "NTEE_CD",
  "SORT_NAME", "ACCT_PD", "REGION", "RYEAR", "ID"
)

# Minimum columns required after legacy BMF harmonization. These are the only
# current-schema columns that appear (under their legacy aliases) in every
# 501CX-NONPROFIT-PX dictionary from 1989-2016. See data/crosswalks/XWALK-BMF-V2.0.csv.
BMF_LEGACY_MIN_COLUMNS <- c(
  "EIN", "NAME", "CITY", "STATE", "ZIP", "SUBSECTION", "NTEE_CD"
)

# ============================================================================
# Pre-Check Functions
# ============================================================================

#' Validate Raw BMF Structure
#'
#' @description
#' Validates that the raw BMF data has the expected structure before
#' transformation. Checks for required columns, and basic data quality 
#' indicators.
#' 
#' @details
#' Failure only occurs if the required columns aren't present
#' 
#'
#' @param dt data.table raw BMF data
#' @param required_cols character vector of required column names
#' @param strict logical if TRUE, stop on validation failures; if FALSE,
#'   return validation results without stopping
#'
#' @return list with validation results:
#'   \itemize{
#'     \item passed - logical overall pass/fail
#'     \item row_count - numeric actual row count
#'     \item missing_columns - character vector of missing required columns
#'     \item extra_columns - character vector of unexpected columns
#'     \item duplicate_eins - numeric count of duplicate EINs
#'     \item null_counts - named numeric vector of NULL counts per required column
#'   }
#'
#' @examples
#' \dontrun{
#' results <- validate_raw_bmf_structure(bmf_raw)
#' if (!results$passed) {
#'   stop("BMF validation failed")
#' }
#' }
#'
#' @export
validate_raw_bmf_structure <- function(dt,
                                       required_cols = BMF_REQUIRED_COLUMNS,
                                       strict = TRUE) {

  results <- list(
    passed = TRUE,
    timestamp = Sys.time(),
    row_count = nrow(dt),
    column_count = ncol(dt),
    missing_columns = character(0),
    extra_columns = character(0),
    duplicate_eins = 0L,
    null_counts = integer(0),
    messages = character(0)
  )

  # Check 1: Required columns exist
  results$missing_columns <- setdiff(required_cols, names(dt))
  if (length(results$missing_columns) > 0) {
    msg <- sprintf(
      "Missing required columns: %s",
      paste(results$missing_columns, collapse = ", ")
    )
    results$messages <- c(results$messages, msg)
    results$passed <- FALSE
  }

  # Check 2: Extra columns (informational, not a failure)
  results$extra_columns <- setdiff(names(dt), required_cols)
  if (length(results$extra_columns) > 0) {
    msg <- sprintf(
      "Found %d extra columns: %s",
      length(results$extra_columns),
      paste(head(results$extra_columns, 5), collapse = ", ")
    )
    results$messages <- c(results$messages, msg)
    # Not a failure, just informational
  }

  # Check 3: Duplicate EINs
  if ("EIN" %in% names(dt)) {
    results$duplicate_eins <- nrow(dt) - data.table::uniqueN(dt$EIN)
    if (results$duplicate_eins > 0) {
      msg <- sprintf(
        "Found %s duplicate EIN values",
        format(results$duplicate_eins, big.mark = ",")
      )
      results$messages <- c(results$messages, msg)
      # Duplicates may be expected (e.g., multiple regional files with overlap)
    }
  }

  # Check 4: NULL counts for required columns
  present_required <- intersect(required_cols, names(dt))
  results$null_counts <- sapply(present_required, function(col) {
    sum(is.na(dt[[col]]) | dt[[col]] == "")
  })

  # Log results
  message("========================================")
  message("PRE-TRANSFORMATION VALIDATION RESULTS")
  message("========================================")
  message(sprintf("Timestamp: %s", results$timestamp))
  message(sprintf("Row count: %s", format(results$row_count, big.mark = ",")))
  message(sprintf("Column count: %d", results$column_count))
  message(sprintf("Missing columns: %d", length(results$missing_columns)))
  message(sprintf("Duplicate EINs: %s", format(results$duplicate_eins, big.mark = ",")))

  # Log high NULL counts (>10%)
  high_nulls <- results$null_counts[results$null_counts > (nrow(dt) * 0.1)]
  if (length(high_nulls) > 0) {
    message("Columns with >10% missing values:")
    for (col in names(high_nulls)) {
      pct <- 100 * high_nulls[col] / nrow(dt)
      message(sprintf("  - %s: %s (%.1f%%)",
                      col,
                      format(high_nulls[col], big.mark = ","),
                      pct))
    }
  }

  message(sprintf("OVERALL: %s", ifelse(results$passed, "PASSED", "FAILED")))
  message("========================================")

  if (!results$passed && strict) {
    stop(paste(
      "Pre-transformation validation failed:",
      paste(results$messages, collapse = "; ")
    ))
  }

  return(results)
}