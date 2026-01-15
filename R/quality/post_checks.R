# ============================================================================
# post_checks.R
# Post-transformation quality validation for processed BMF data
# ============================================================================

# ============================================================================
# Module Constants
# ============================================================================

# Column categories with data types for quality reporting
# Types: "character", "numeric", "date", "boolean", "code"
COLUMN_CATEGORIES <- list(
  identity = list(
    name = "Identity",
    columns = list(
      ein = list(type = "character"),
      ein_raw = list(type = "character")
    )
  ),
  org_name = list(
    name = "Organization Name",
    columns = list(
      org_name_raw = list(type = "character"),
      org_name_join = list(type = "character"),
      org_name_display = list(type = "character"),
      org_legal_suffix = list(type = "character"),
      org_parent_name = list(type = "character")
    )
  ),
  dba_name = list(
    name = "DBA Name",
    columns = list(
      dba_name = list(type = "character"),
      dba_name_raw = list(type = "character")
    )
  ),
  ico = list(
    name = "In Care Of",
    columns = list(
      in_care_of_name_raw = list(type = "character"),
      in_care_of_name_clean = list(type = "character"),
      in_care_of_name_provided = list(type = "boolean")
    )
  ),
  group_exemption = list(
    name = "Group Exemption",
    columns = list(
      group_exemption_number_raw = list(type = "character"),
      group_exemption_number = list(type = "character"),
      group_exemption_is_member = list(type = "boolean")
    )
  ),
  address_raw = list(
    name = "Address (Raw)",
    columns = list(
      org_addr_street_raw = list(type = "character"),
      org_addr_city_raw = list(type = "character"),
      org_addr_state_raw = list(type = "character"),
      org_addr_zip_raw = list(type = "character")
    )
  ),
  address_clean = list(
    name = "Address (Cleaned)",
    columns = list(
      org_addr_street = list(type = "character"),
      org_addr_city = list(type = "character"),
      org_addr_state = list(type = "code"),
      org_addr_zip5 = list(type = "character"),
      org_addr_zip4 = list(type = "character"),
      org_addr_zip = list(type = "character"),
      org_addr_full = list(type = "character")
    )
  ),
  address_flags = list(
    name = "Address Quality Flags",
    columns = list(
      org_addr_is_missing = list(type = "boolean"),
      org_addr_is_po_box = list(type = "boolean"),
      org_addr_is_rural_route = list(type = "boolean"),
      org_addr_has_special_chars = list(type = "boolean"),
      org_addr_missing_number = list(type = "boolean"),
      org_addr_state_invalid = list(type = "boolean")
    )
  ),
  classification = list(
    name = "Classification",
    columns = list(
      subsection_code = list(type = "code"),
      classification_code = list(type = "code"),
      exempt_organization_type = list(type = "character"),
      all_classifications_string = list(type = "character")
    )
  ),
  codes = list(
    name = "Organization Codes",
    columns = list(
      affiliation_code = list(type = "code"),
      affiliation_code_definition = list(type = "character"),
      deductibility_code = list(type = "code"),
      deductibility_code_definition = list(type = "character"),
      foundation_code = list(type = "code"),
      foundation_code_definition = list(type = "character"),
      organization_code = list(type = "code"),
      organization_code_definition = list(type = "character"),
      status_code = list(type = "code"),
      status_code_definition = list(type = "character")
    )
  ),
  dates = list(
    name = "Dates",
    columns = list(
      ruling_date_ym_str = list(type = "character"),
      ruling_date = list(type = "date"),
      ruling_date_is_missing = list(type = "boolean"),
      tax_period_ymd = list(type = "date"),
      tax_period_is_missing = list(type = "boolean")
    )
  ),
  financial_codes = list(
    name = "Financial Codes",
    columns = list(
      asset_code = list(type = "code"),
      asset_code_definition = list(type = "character"),
      income_code = list(type = "code"),
      income_code_definition = list(type = "character")
    )
  ),
  financial_amounts = list(
    name = "Financial Amounts",
    columns = list(
      asset_amount = list(type = "numeric"),
      income_amount = list(type = "numeric"),
      revenue_amount = list(type = "numeric")
    )
  ),
  activity = list(
    name = "Activity",
    columns = list(
      activity_code = list(type = "character"),
      activity_code_definitions = list(type = "character"),
      activity_code_categories = list(type = "character")
    )
  ),
  filing = list(
    name = "Filing Requirements",
    columns = list(
      filing_requirement_code = list(type = "code"),
      filing_requirement_code_definition = list(type = "character"),
      pf_filing_requirement_code = list(type = "code"),
      pf_filing_requirement_code_definition = list(type = "character")
    )
  ),
  ntee = list(
    name = "NTEE Codes",
    columns = list(
      ntee_code_raw = list(type = "character"),
      ntee_code_clean = list(type = "code"),
      ntee_code_definition = list(type = "character"),
      ntee_code_major_group = list(type = "code"),
      ntee_activity_code = list(type = "code"),
      ntee_activity_code_definition = list(type = "character"),
      ntee_common_code = list(type = "code"),
      ntee_common_code_definition = list(type = "character")
    )
  ),
  nteev2 = list(
    name = "NTEE V2 Codes",
    columns = list(
      nteev2 = list(type = "character"),
      nteev2_code = list(type = "code"),
      nteev2_subsector = list(type = "code"),
      nteev2_org_type = list(type = "code")
    )
  )
)

# Expected output columns after full transformation
BMF_OUTPUT_COLUMNS <- c(
  # Identity fields
  "ein", "ein_raw",
  # Organization name fields
  "org_name_raw", "org_name_join", "org_name_display", "org_legal_suffix", "org_parent_name",
  # Secondary (Doing business as) name fields
  "dba_name", "dba_name_raw",
  # ICO fields
  "in_care_of_name_raw", "in_care_of_name_clean", "in_care_of_name_provided",
  # Group exemption fields
  "group_exemption_number_raw", "group_exemption_number", "group_exemption_is_member",
  # Address fields (raw)
  "org_addr_street_raw", "org_addr_city_raw", "org_addr_state_raw", "org_addr_zip_raw",
  # Address fields (cleaned)
  "org_addr_street", "org_addr_city", "org_addr_state",
  "org_addr_zip5", "org_addr_zip4", "org_addr_zip", "org_addr_full",
  # Address quality flags
  "org_addr_is_missing", "org_addr_is_po_box", "org_addr_is_rural_route",
  "org_addr_has_special_chars", "org_addr_missing_number", "org_addr_state_invalid",
  # Classification fields
  "subsection_code", "classification_code", "exempt_organization_type",
  "all_classifications_string",
  # Code fields
  "affiliation_code", "affiliation_code_definition",
  "deductibility_code", "deductibility_code_definition",
  "foundation_code", "foundation_code_definition",
  "organization_code", "organization_code_definition",
  "status_code", "status_code_definition",
  # Date fields
  "ruling_date_ym_str", "ruling_date", "ruling_date_is_missing",
  "tax_period_ymd", "tax_period_is_missing",
  # Financial code fields
  "asset_code", "asset_code_definition",
  "income_code", "income_code_definition",
  # Financial amount fields
  "asset_amount", "income_amount", "revenue_amount",
  # Activity fields
  "activity_code", "activity_code_definitions", "activity_code_categories",
  # Filing requirement fields
  "filing_requirement_code", "filing_requirement_code_definition",
  "pf_filing_requirement_code", "pf_filing_requirement_code_definition",
  # NTEE fields
  "ntee_code_raw", "ntee_code_clean", "ntee_code_definition", "ntee_code_major_group", "ntee_activity_code", "ntee_activity_code_definition",
  "ntee_common_code", "ntee_common_code_definition",
  # NTEEV2 fields
  "nteev2", "nteev2_code", "nteev2_subsector", "nteev2_org_type"
). 

# Critical fields that must have no NULLs in valid records
CRITICAL_FIELDS <- c(
  "ein"
)

# ============================================================================
# Type-Specific Statistics Helper Functions
# ============================================================================

#' Calculate Completeness Statistics
#'
#' @param col_data Vector of column data
#' @param n_rows Total row count for percentage calculations
#' @return List with completeness metrics
calc_completeness_stats <- function(col_data, n_rows) {
  is_na <- is.na(col_data)
  is_empty <- !is_na & col_data == ""
  non_null <- sum(!is_na & !is_empty)

  list(
    total = n_rows,
    non_null = non_null,
    null_count = sum(is_na),
    empty_count = sum(is_empty),
    completeness_pct = round(100 * non_null / n_rows, 2),
    missing_pct = round(100 * (sum(is_na) + sum(is_empty)) / n_rows, 2)
  )
}

#' Calculate Numeric Column Statistics
#'
#' @param col_data Numeric vector
#' @return List with numeric distribution statistics
calc_numeric_stats <- function(col_data) {
  valid_data <- col_data[!is.na(col_data)]

  if (length(valid_data) == 0) {
    return(list(
      min = NA, max = NA, mean = NA, median = NA, sd = NA,
      q1 = NA, q3 = NA, iqr = NA,
      zero_count = 0, negative_count = 0,
      valid_count = 0
    ))
  }

  q <- quantile(valid_data, probs = c(0.25, 0.75), na.rm = TRUE)
  iqr_val <- q[2] - q[1]

  list(
    min = min(valid_data),
    max = max(valid_data),
    mean = round(mean(valid_data), 2),
    median = median(valid_data),
    sd = round(sd(valid_data), 2),
    q1 = q[1],
    q3 = q[2],
    iqr = iqr_val,
    zero_count = sum(valid_data == 0),
    negative_count = sum(valid_data < 0),
    valid_count = length(valid_data)
  )
}

#' Calculate Character Column Statistics
#'
#' @param col_data Character vector
#' @return List with character field statistics
calc_character_stats <- function(col_data) {
  valid_data <- col_data[!is.na(col_data) & col_data != ""]

  if (length(valid_data) == 0) {
    return(list(
      unique_count = 0,
      min_length = NA,
      max_length = NA,
      avg_length = NA,
      valid_count = 0
    ))
  }

  lengths <- nchar(valid_data)

  list(
    unique_count = data.table::uniqueN(valid_data),
    min_length = min(lengths),
    max_length = max(lengths),
    avg_length = round(mean(lengths), 1),
    valid_count = length(valid_data)
  )
}

#' Calculate Code Column Statistics
#'
#' @param col_data Vector of code values
#' @param top_n Number of top values to return (default: 10)
#' @return List with code distribution statistics
calc_code_stats <- function(col_data, top_n = 10) {
  valid_data <- col_data[!is.na(col_data) & col_data != ""]

  if (length(valid_data) == 0) {
    return(list(
      unique_count = 0,
      top_values = list(),
      valid_count = 0
    ))
  }

  # Calculate frequency distribution
  freq_table <- data.table::data.table(value = valid_data)[
    , .N, by = value
  ][order(-N)]

  # Get top N values
  top_values <- head(freq_table, top_n)
  top_list <- lapply(seq_len(nrow(top_values)), function(i) {
    list(
      value = as.character(top_values$value[i]),
      count = top_values$N[i],
      pct = round(100 * top_values$N[i] / length(valid_data), 2)
    )
  })

  list(
    unique_count = data.table::uniqueN(valid_data),
    top_values = top_list,
    valid_count = length(valid_data)
  )
}

#' Calculate Boolean Column Statistics
#'
#' @param col_data Logical vector
#' @param n_rows Total row count for percentage calculations
#' @return List with boolean distribution statistics
calc_boolean_stats <- function(col_data, n_rows) {
  true_count <- sum(col_data == TRUE, na.rm = TRUE)
  false_count <- sum(col_data == FALSE, na.rm = TRUE)
  na_count <- sum(is.na(col_data))

  list(
    true_count = true_count,
    false_count = false_count,
    na_count = na_count,
    true_pct = round(100 * true_count / n_rows, 2),
    false_pct = round(100 * false_count / n_rows, 2),
    na_pct = round(100 * na_count / n_rows, 2)
  )
}

#' Calculate Date Column Statistics
#'
#' @param col_data Date vector
#' @return List with date range statistics
calc_date_stats <- function(col_data) {
  valid_dates <- col_data[!is.na(col_data)]

  if (length(valid_dates) == 0) {
    return(list(
      min_date = NA,
      max_date = NA,
      range_days = NA,
      valid_count = 0
    ))
  }

  min_date <- min(valid_dates)
  max_date <- max(valid_dates)

  list(
    min_date = as.character(min_date),
    max_date = as.character(max_date),
    range_days = as.integer(max_date - min_date),
    valid_count = length(valid_dates)
  )
}

# ============================================================================
# Column and Category Report Functions
# ============================================================================

#' Generate Quality Report for a Single Column
#'
#' @param dt data.table containing the data
#' @param col_name Name of the column to analyze
#' @param col_type Data type of the column
#' @return List with column quality metrics
generate_column_report <- function(dt, col_name, col_type) {
  if (!col_name %in% names(dt)) {
    return(list(
      column_name = col_name,
      column_type = col_type,
      present = FALSE,
      completeness = NULL,
      type_stats = NULL
    ))
  }

  col_data <- dt[[col_name]]
  n_rows <- nrow(dt)

  # Calculate completeness for all column types

completeness <- calc_completeness_stats(col_data, n_rows)

  # Calculate type-specific statistics
  type_stats <- switch(col_type,
    "numeric" = calc_numeric_stats(col_data),
    "character" = calc_character_stats(col_data),
    "code" = calc_code_stats(col_data),
    "boolean" = calc_boolean_stats(col_data, n_rows),
    "date" = calc_date_stats(col_data),
    list()  # default empty
  )

  list(
    column_name = col_name,
    column_type = col_type,
    present = TRUE,
    completeness = completeness,
    type_stats = type_stats
  )
}

#' Generate Quality Report for a Category of Columns
#'
#' @param dt data.table containing the data
#' @param category_key Key from COLUMN_CATEGORIES
#' @param category_config Category configuration from COLUMN_CATEGORIES
#' @return List with category quality metrics
generate_category_report <- function(dt, category_key, category_config) {
  columns_config <- category_config$columns
  col_names <- names(columns_config)

  # Generate report for each column in category
  column_reports <- lapply(col_names, function(col_name) {
    col_type <- columns_config[[col_name]]$type
    generate_column_report(dt, col_name, col_type)
  })
  names(column_reports) <- col_names

  # Calculate category summary
  present_count <- sum(sapply(column_reports, function(x) x$present))
  completeness_values <- sapply(column_reports, function(x) {
    if (!is.null(x$completeness)) x$completeness$completeness_pct else NA
  })
  avg_completeness <- mean(completeness_values, na.rm = TRUE)

  list(
    category_name = category_config$name,
    category_key = category_key,
    column_count = length(col_names),
    columns_present = present_count,
    avg_completeness = round(avg_completeness, 2),
    columns = column_reports
  )
}

# ============================================================================
# Quality Report Functions
# ============================================================================

#' Generate Post-Transformation Quality Report
#'
#' @description
#' Generates a comprehensive quality report after all transformations are
#' complete. Checks for expected columns, validates data integrity, and
#' provides detailed quality metrics for each column organized by category.
#'
#' @param dt data.table transformed BMF data
#' @param pre_check_results list results from validate_raw_bmf_structure()
#' @param expected_cols character vector of expected output columns
#'
#' @return list with quality report:
#'   \itemize{
#'     \item passed - logical overall pass/fail
#'     \item row_preservation - logical TRUE if row count matches pre-check
#'     \item missing_columns - character vector of missing expected columns
#'     \item overall_completeness - numeric average completeness across all columns
#'     \item category_reports - list of detailed reports by category
#'     \item summary_stats - list of high-level summary statistics
#'   }
#'
#' @export
generate_quality_report <- function(dt,
                                    pre_check_results = NULL,
                                    expected_cols = BMF_OUTPUT_COLUMNS) {

  report <- list(
    passed = TRUE,
    timestamp = Sys.time(),
    row_count = nrow(dt),
    column_count = ncol(dt),
    row_preservation = TRUE,
    missing_columns = character(0),
    extra_columns = character(0),
    overall_completeness = 0,
    critical_field_issues = list(),
    category_reports = list(),
    summary_stats = list()
  )

  # ---------------------------------------------------------------------------
  # Check 1: Row preservation
  # ---------------------------------------------------------------------------
  if (!is.null(pre_check_results)) {
    if (nrow(dt) != pre_check_results$row_count) {
      report$row_preservation <- FALSE
      report$passed <- FALSE
      warning(sprintf(
        "Row count changed during transformation: %s -> %s",
        format(pre_check_results$row_count, big.mark = ","),
        format(nrow(dt), big.mark = ",")
      ))
    }
  }

  # ---------------------------------------------------------------------------
  # Check 2: Expected columns exist
  # ---------------------------------------------------------------------------
  report$missing_columns <- setdiff(expected_cols, names(dt))
  report$extra_columns <- setdiff(names(dt), expected_cols)

  # ---------------------------------------------------------------------------
  # Check 3: Critical fields validation
  # ---------------------------------------------------------------------------
  for (field in CRITICAL_FIELDS) {
    if (field %in% names(dt)) {
      null_count <- sum(is.na(dt[[field]]) | dt[[field]] == "")
      if (null_count > 0) {
        report$critical_field_issues[[field]] <- null_count
        report$passed <- FALSE
      }
    }
  }

  # ---------------------------------------------------------------------------
  # Check 4: Generate comprehensive category reports
  # ---------------------------------------------------------------------------
  category_keys <- names(COLUMN_CATEGORIES)
  report$category_reports <- lapply(category_keys, function(cat_key) {
    generate_category_report(dt, cat_key, COLUMN_CATEGORIES[[cat_key]])
  })
  names(report$category_reports) <- category_keys

  # ---------------------------------------------------------------------------
  # Check 5: Calculate overall completeness
  # ---------------------------------------------------------------------------
  all_completeness <- unlist(lapply(report$category_reports, function(cat) {
    sapply(cat$columns, function(col) {
      if (!is.null(col$completeness)) col$completeness$completeness_pct else NA
    })
  }))
  report$overall_completeness <- round(mean(all_completeness, na.rm = TRUE), 2)

  # ---------------------------------------------------------------------------
  # Check 6: High-level summary statistics
  # ---------------------------------------------------------------------------

  # Identity summary
  if ("ein" %in% names(dt)) {
    report$summary_stats$unique_eins <- data.table::uniqueN(dt$ein)
    report$summary_stats$duplicate_eins <- nrow(dt) - report$summary_stats$unique_eins
  }

  # Organization name summary
  if ("org_name_raw" %in% names(dt)) {
    report$summary_stats$unique_org_names <- data.table::uniqueN(dt$org_name_raw)
  }

  # Financial summary
  if ("asset_amount" %in% names(dt)) {
    valid_assets <- dt$asset_amount[!is.na(dt$asset_amount)]
    report$summary_stats$financial <- list(
      total_assets = sum(valid_assets),
      median_assets = median(valid_assets),
      orgs_with_assets = length(valid_assets),
      orgs_zero_assets = sum(valid_assets == 0)
    )
  }

  if ("income_amount" %in% names(dt)) {
    valid_income <- dt$income_amount[!is.na(dt$income_amount)]
    if (is.null(report$summary_stats$financial)) {
      report$summary_stats$financial <- list()
    }
    report$summary_stats$financial$total_income <- sum(valid_income)
    report$summary_stats$financial$median_income <- median(valid_income)
  }

  if ("revenue_amount" %in% names(dt)) {
    valid_revenue <- dt$revenue_amount[!is.na(dt$revenue_amount)]
    if (is.null(report$summary_stats$financial)) {
      report$summary_stats$financial <- list()
    }
    report$summary_stats$financial$total_revenue <- sum(valid_revenue)
    report$summary_stats$financial$median_revenue <- median(valid_revenue)
  }

  # Address quality summary
  if ("org_addr_is_missing" %in% names(dt)) {
    report$summary_stats$address_quality <- list(
      missing_count = sum(dt$org_addr_is_missing == TRUE, na.rm = TRUE),
      missing_pct = round(100 * sum(dt$org_addr_is_missing == TRUE, na.rm = TRUE) / nrow(dt), 2),
      po_box_count = sum(dt$org_addr_is_po_box == TRUE, na.rm = TRUE),
      po_box_pct = round(100 * sum(dt$org_addr_is_po_box == TRUE, na.rm = TRUE) / nrow(dt), 2),
      rural_route_count = sum(dt$org_addr_is_rural_route == TRUE, na.rm = TRUE),
      invalid_state_count = sum(dt$org_addr_state_invalid == TRUE, na.rm = TRUE)
    )
  }

  # Classification summary - subsection distribution
  if ("subsection_code" %in% names(dt)) {
    subsection_dist <- dt[, .N, by = subsection_code][order(-N)]
    report$summary_stats$subsection_distribution <- as.data.frame(subsection_dist)
  }

  # Status code summary
  if ("status_code" %in% names(dt)) {
    status_dist <- dt[, .N, by = status_code][order(-N)]
    report$summary_stats$status_distribution <- as.data.frame(status_dist)
  }

  # NTEE major group summary
  if ("ntee_code_major_group" %in% names(dt)) {
    ntee_dist <- dt[!is.na(ntee_code_major_group) & ntee_code_major_group != "",
                    .N, by = ntee_code_major_group][order(-N)]
    report$summary_stats$ntee_major_group_distribution <- as.data.frame(ntee_dist)
  }

  # Date range summary
  if ("ruling_date" %in% names(dt)) {
    valid_dates <- dt$ruling_date[!is.na(dt$ruling_date)]
    if (length(valid_dates) > 0) {
      report$summary_stats$ruling_date_range <- list(
        earliest = as.character(min(valid_dates)),
        latest = as.character(max(valid_dates)),
        orgs_with_ruling_date = length(valid_dates)
      )
    }
  }

  return(report)
}

#' Print Quality Report
#'
#' @description
#' Prints a comprehensive formatted quality report to the console, organized
#' by column category with detailed completeness and type-specific statistics.
#'
#' @param report list quality report from generate_quality_report()
#' @param verbose logical if TRUE, print detailed column-level stats (default: FALSE)
#'
#' @export
print_quality_report <- function(report, verbose = FALSE) {

  message("")
  message("================================================================================")
  message("                    POST-TRANSFORMATION QUALITY REPORT")
  message("================================================================================")
  message(sprintf("Timestamp: %s", report$timestamp))
  message(sprintf("Final row count: %s", format(report$row_count, big.mark = ",")))
  message(sprintf("Final column count: %d", report$column_count))
  message(sprintf("Overall completeness: %.1f%%", report$overall_completeness))
  message(sprintf("Row preservation: %s",
                  ifelse(report$row_preservation, "PASSED", "FAILED")))
  message("")

  # ---------------------------------------------------------------------------
  # Missing columns warning
  # ---------------------------------------------------------------------------
  if (length(report$missing_columns) > 0) {
    message("WARNING - Missing expected columns:")
    for (col in report$missing_columns) {
      message(sprintf("  - %s", col))
    }
    message("")
  }

  # ---------------------------------------------------------------------------
  # Critical field issues
  # ---------------------------------------------------------------------------
  if (length(report$critical_field_issues) > 0) {
    message("CRITICAL FIELD ISSUES:")
    for (field in names(report$critical_field_issues)) {
      message(sprintf("  - %s: %s NULL values",
                      field,
                      format(report$critical_field_issues[[field]], big.mark = ",")))
    }
    message("")
  }

  # ---------------------------------------------------------------------------
  # Category-by-category completeness summary
  # ---------------------------------------------------------------------------
  message("--------------------------------------------------------------------------------")
  message("COMPLETENESS BY CATEGORY")
  message("--------------------------------------------------------------------------------")

  for (cat_key in names(report$category_reports)) {
    cat_report <- report$category_reports[[cat_key]]
    message(sprintf("\n[%s] (%d columns, avg completeness: %.1f%%)",
                    cat_report$category_name,
                    cat_report$column_count,
                    cat_report$avg_completeness))

    # Find columns with less than 100% completeness
    low_completeness_cols <- list()
    for (col_name in names(cat_report$columns)) {
      col <- cat_report$columns[[col_name]]
      if (col$present && !is.null(col$completeness)) {
        if (col$completeness$completeness_pct < 100) {
          low_completeness_cols[[col_name]] <- col$completeness$completeness_pct
        }
      }
    }

    if (length(low_completeness_cols) > 0) {
      # Sort by completeness ascending
      sorted_cols <- low_completeness_cols[order(unlist(low_completeness_cols))]
      for (col_name in names(sorted_cols)) {
        message(sprintf("    - %s: %.1f%%", col_name, sorted_cols[[col_name]]))
      }
    } else {
      message("    All columns 100% complete")
    }

    # Print verbose type-specific stats if requested
    if (verbose) {
      for (col_name in names(cat_report$columns)) {
        col <- cat_report$columns[[col_name]]
        if (col$present && !is.null(col$type_stats)) {
          .print_column_type_stats(col_name, col$column_type, col$type_stats)
        }
      }
    }
  }
  message("")

  # ---------------------------------------------------------------------------
  # High-level summary statistics
  # ---------------------------------------------------------------------------
  message("--------------------------------------------------------------------------------")
  message("SUMMARY STATISTICS")
  message("--------------------------------------------------------------------------------")

  # Identity stats
  if (!is.null(report$summary_stats$unique_eins)) {
    message("\nIdentity:")
    message(sprintf("  Unique EINs: %s",
                    format(report$summary_stats$unique_eins, big.mark = ",")))
    if (!is.null(report$summary_stats$duplicate_eins) && report$summary_stats$duplicate_eins > 0) {
      message(sprintf("  Duplicate EINs: %s",
                      format(report$summary_stats$duplicate_eins, big.mark = ",")))
    }
  }

  # Financial stats
  if (!is.null(report$summary_stats$financial)) {
    fin <- report$summary_stats$financial
    message(sprintf("\nFinancial:"))
    if (!is.null(fin$total_assets)) {
      message(sprintf("  Total Assets: $%s", format(fin$total_assets, big.mark = ",")))
      message(sprintf("  Median Assets: $%s", format(fin$median_assets, big.mark = ",")))
      message(sprintf("  Orgs with Assets: %s", format(fin$orgs_with_assets, big.mark = ",")))
      message(sprintf("  Orgs with Zero Assets: %s", format(fin$orgs_zero_assets, big.mark = ",")))
    }
    if (!is.null(fin$total_income)) {
      message(sprintf("  Total Income: $%s", format(fin$total_income, big.mark = ",")))
      message(sprintf("  Median Income: $%s", format(fin$median_income, big.mark = ",")))
    }
    if (!is.null(fin$total_revenue)) {
      message(sprintf("  Total Revenue: $%s", format(fin$total_revenue, big.mark = ",")))
      message(sprintf("  Median Revenue: $%s", format(fin$median_revenue, big.mark = ",")))
    }
  }

  # Address quality stats
  if (!is.null(report$summary_stats$address_quality)) {
    aq <- report$summary_stats$address_quality
    message(sprintf("\nAddress Quality:"))
    message(sprintf("  Missing Address: %s (%.1f%%)",
                    format(aq$missing_count, big.mark = ","), aq$missing_pct))
    message(sprintf("  P.O. Box: %s (%.1f%%)",
                    format(aq$po_box_count, big.mark = ","), aq$po_box_pct))
    message(sprintf("  Rural Route: %s",
                    format(aq$rural_route_count, big.mark = ",")))
    message(sprintf("  Invalid State: %s",
                    format(aq$invalid_state_count, big.mark = ",")))
  }

  # Ruling date range
  if (!is.null(report$summary_stats$ruling_date_range)) {
    rdr <- report$summary_stats$ruling_date_range
    message(sprintf("\nRuling Date Range:"))
    message(sprintf("  Earliest: %s", rdr$earliest))
    message(sprintf("  Latest: %s", rdr$latest))
    message(sprintf("  Orgs with Ruling Date: %s",
                    format(rdr$orgs_with_ruling_date, big.mark = ",")))
  }

  # Subsection distribution (top 5)
  if (!is.null(report$summary_stats$subsection_distribution)) {
    message(sprintf("\nSubsection Distribution (Top 5):"))
    dist <- head(report$summary_stats$subsection_distribution, 5)
    for (i in seq_len(nrow(dist))) {
      message(sprintf("  %s: %s",
                      dist$subsection_code[i],
                      format(dist$N[i], big.mark = ",")))
    }
  }

  # NTEE major group distribution (top 5)
  if (!is.null(report$summary_stats$ntee_major_group_distribution)) {
    message(sprintf("\nNTEE Major Group Distribution (Top 5):"))
    dist <- head(report$summary_stats$ntee_major_group_distribution, 5)
    for (i in seq_len(nrow(dist))) {
      message(sprintf("  %s: %s",
                      dist$ntee_code_major_group[i],
                      format(dist$N[i], big.mark = ",")))
    }
  }

  message("")
  message("================================================================================")
  message(sprintf("OVERALL RESULT: %s", ifelse(report$passed, "PASSED", "FAILED")))
  message("================================================================================")
  message("")
}

#' Print Type-Specific Column Statistics (Internal Helper)
#'
#' @param col_name Column name
#' @param col_type Column type
#' @param type_stats Type-specific statistics
.print_column_type_stats <- function(col_name, col_type, type_stats) {
  if (col_type == "numeric" && !is.null(type_stats$valid_count) && type_stats$valid_count > 0) {
    message(sprintf("      [%s] min=%.0f, max=%.0f, mean=%.0f, median=%.0f",
                    col_name, type_stats$min, type_stats$max, type_stats$mean, type_stats$median))
  } else if (col_type == "code" && !is.null(type_stats$unique_count)) {
    message(sprintf("      [%s] %d unique values",
                    col_name, type_stats$unique_count))
  } else if (col_type == "date" && !is.null(type_stats$min_date)) {
    message(sprintf("      [%s] range: %s to %s",
                    col_name, type_stats$min_date, type_stats$max_date))
  }
}

#' Save Quality Report to File
#'
#' @description
#' Saves the comprehensive quality report to a JSON file for auditing purposes
#' and downstream processing (e.g., HTML rendering in a separate repo).
#'
#' @param report list quality report from generate_quality_report()
#' @param output_path character path to save JSON report
#'
#' @export
save_quality_report <- function(report, output_path) {

  # Create output directory if needed
  output_dir <- dirname(output_path)
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  # Deep copy the report for JSON serialization
  report_json <- report

  # Convert timestamp to ISO 8601 string
  report_json$timestamp <- format(report_json$timestamp, "%Y-%m-%dT%H:%M:%S")

  # Convert data.tables/data.frames in summary_stats to lists for clean JSON
  if (!is.null(report_json$summary_stats$subsection_distribution)) {
    report_json$summary_stats$subsection_distribution <-
      as.data.frame(report_json$summary_stats$subsection_distribution)
  }
  if (!is.null(report_json$summary_stats$status_distribution)) {
    report_json$summary_stats$status_distribution <-
      as.data.frame(report_json$summary_stats$status_distribution)
  }
  if (!is.null(report_json$summary_stats$ntee_major_group_distribution)) {
    report_json$summary_stats$ntee_major_group_distribution <-
      as.data.frame(report_json$summary_stats$ntee_major_group_distribution)
  }

  # Round large numeric values for readability
  .round_large_numbers <- function(x, digits = 2) {
    if (is.numeric(x) && length(x) == 1 && !is.na(x)) {
      if (abs(x) >= 1e6) {
        return(round(x, digits))
      }
    }
    return(x)
  }

  # Apply rounding to financial stats
  if (!is.null(report_json$summary_stats$financial)) {
    report_json$summary_stats$financial <- lapply(
      report_json$summary_stats$financial,
      .round_large_numbers
    )
  }

  jsonlite::write_json(
    report_json,
    output_path,
    pretty = TRUE,
    auto_unbox = TRUE,
    digits = 4,
    na = "null"
  )

  message(sprintf("Quality report saved to: %s", output_path))
}

#' Validate Transformation Step
#'
#' @description
#' Validates a single transformation step by comparing row counts and
#' checking for expected new columns.
#'
#' @param dt_before data.table before transformation
#' @param dt_after data.table after transformation
#' @param expected_new_cols character vector of columns that should be added
#' @param step_name character name of the transformation step
#'
#' @return logical TRUE if validation passes
#'
#' @export
validate_step <- function(dt_before,
                          dt_after,
                          expected_new_cols,
                          step_name) {

  passed <- TRUE

  # Check row count preservation
  if (nrow(dt_before) != nrow(dt_after)) {
    warning(sprintf(
      "[%s] Row count changed: %s -> %s",
      step_name,
      format(nrow(dt_before), big.mark = ","),
      format(nrow(dt_after), big.mark = ",")
    ))
    passed <- FALSE
  }

  # Check expected new columns exist
  missing_cols <- setdiff(expected_new_cols, names(dt_after))
  if (length(missing_cols) > 0) {
    warning(sprintf(
      "[%s] Missing expected columns: %s",
      step_name,
      paste(missing_cols, collapse = ", ")
    ))
    passed <- FALSE
  }

  if (passed) {
    message(sprintf("[%s] Validation passed", step_name))
  }

  return(passed)
}

# ============================================================================
# HTML/PDF Report Rendering
# ============================================================================

#' Render Quality Report to HTML or PDF
#'
#' @description
#' Renders the quality report to an HTML or PDF document using a Quarto
#' template. The report includes interactive visualizations of field
#' completeness, subsection distribution, and organization type distribution.
#'
#' @param report list quality report from generate_quality_report()
#' @param output_path character path for the output file (e.g.,
#'   "data/quality/bmf_2025_quality_report.html")
#' @param format character output format: "html" (default) or "pdf"
#' @param open logical if TRUE, open the report in browser/viewer after
#'   rendering (default: FALSE)
#'
#' @return character path to the rendered report file (invisibly)
#'
#' @details
#' This function requires:
#' - The `quarto` R package (for rendering)
#' - A working Quarto installation
#' - For PDF output: a LaTeX distribution (e.g., TinyTeX)
#'
#' The report template is located at `R/quality/quality_report_template.qmd`.
#'
#' @examples
#' \dontrun{
#' report <- generate_quality_report(bmf_processed)
#' render_quality_report(report, "data/quality/report.html")
#' render_quality_report(report, "data/quality/report.pdf", format = "pdf")
#' }
#'
#' @export
render_quality_report <- function(report,
                                  output_path,
                                  format = c("html", "pdf"),
                                  open = FALSE) {

  format <- match.arg(format)

  # Validate report structure
  required_fields <- c("passed", "timestamp", "row_count", "completeness")
  missing_fields <- setdiff(required_fields, names(report))
  if (length(missing_fields) > 0) {
    stop(sprintf(
      "Report is missing required fields: %s",
      paste(missing_fields, collapse = ", ")
    ))
  }

  # Check for quarto package
  if (!requireNamespace("quarto", quietly = TRUE)) {
    stop(
      "The 'quarto' package is required for rendering reports.\n",
      "Install with: install.packages('quarto')"
    )
  }

  # Locate template
  template_path <- here::here("R", "quality", "quality_report_template.qmd")
  if (!file.exists(template_path)) {
    stop(sprintf("Quality report template not found at: %s", template_path))
  }

  # Create output directory if needed
  output_dir <- dirname(output_path)
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  # Save report data to temporary RDS file for the template to read
  temp_rds <- tempfile(fileext = ".rds")
  saveRDS(report, temp_rds)
  on.exit(unlink(temp_rds), add = TRUE)

  # Determine output filename
  output_file <- basename(output_path)
  output_dir_abs <- normalizePath(output_dir, mustWork = FALSE)

  message(sprintf("Rendering quality report to %s...", format))

  # Render the report
  tryCatch({
    quarto::quarto_render(
      input = template_path,
      output_format = format,
      output_file = output_file,
      execute_params = list(report_data_path = temp_rds),
      quiet = TRUE
    )

    # Move rendered file to output path
    rendered_file <- file.path(
      dirname(template_path),
      sub("\\.qmd$", paste0(".", format), basename(template_path))
    )

    # Handle case where quarto uses the output_file name
    if (!file.exists(rendered_file)) {
      rendered_file <- file.path(dirname(template_path), output_file)
    }

    if (file.exists(rendered_file) && normalizePath(rendered_file) != normalizePath(output_path, mustWork = FALSE)) {
      file.copy(rendered_file, output_path, overwrite = TRUE)
      unlink(rendered_file)
    }

    message(sprintf("Quality report saved to: %s", output_path))

    # Open in browser/viewer if requested
    if (open && file.exists(output_path)) {
      if (format == "html") {
        utils::browseURL(output_path)
      } else {
        system2("open", output_path, wait = FALSE)
      }
    }

    invisible(output_path)

  }, error = function(e) {
    stop(sprintf(
      "Failed to render quality report: %s\n\nEnsure Quarto is installed: https://quarto.org/docs/get-started/",
      e$message
    ))
  })
}
