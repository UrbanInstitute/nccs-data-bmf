# ============================================================================
# post_checks.R
# Post-transformation quality validation for processed BMF data
# ============================================================================

# ============================================================================
# Module Constants
# ============================================================================

# Expected output columns after full transformation
BMF_OUTPUT_COLUMNS <- c(
  # Identity fields
  "ein", "ein_raw",
  # Organization name fields
  "org_name_raw", "org_name_join", "org_name_display", "org_legal_suffix",
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
  "activity_code_definitions", "activity_code_categories",
  # Filing requirement fields
  "filing_requirement_code", "filing_requirement_code_definition",
  # NTEE fields
  "ntee_code", "ntee_code_definition"
)

# Critical fields that must have no NULLs in valid records
CRITICAL_FIELDS <- c(
  "ein",
  "subsection_code"
)

# ============================================================================
# Quality Report Functions
# ============================================================================

#' Generate Post-Transformation Quality Report
#'
#' @description
#' Generates a comprehensive quality report after all transformations are
#' complete. Checks for expected columns, validates data integrity, and
#' summarizes key metrics.
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
#'     \item completeness - named numeric vector of completeness rates
#'     \item summary_stats - list of summary statistics
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
    completeness = numeric(0),
    critical_field_issues = list(),
    summary_stats = list()
  )

  # Check 1: Row preservation
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

  # Check 2: Expected columns exist
  report$missing_columns <- setdiff(expected_cols, names(dt))
  report$extra_columns <- setdiff(names(dt), expected_cols)

  # Check 3: Completeness rates
  present_cols <- intersect(expected_cols, names(dt))
  report$completeness <- sapply(present_cols, function(col) {
    non_null <- sum(!is.na(dt[[col]]) & dt[[col]] != "")
    100 * non_null / nrow(dt)
  })

  # Check 4: Critical fields
  for (field in CRITICAL_FIELDS) {
    if (field %in% names(dt)) {
      null_count <- sum(is.na(dt[[field]]) | dt[[field]] == "")
      if (null_count > 0) {
        report$critical_field_issues[[field]] <- null_count
        report$passed <- FALSE
      }
    }
  }

  # Check 5: Summary statistics
  if ("ein" %in% names(dt)) {
    report$summary_stats$unique_eins <- data.table::uniqueN(dt$ein)
  }

  if ("subsection_code" %in% names(dt)) {
    report$summary_stats$subsection_distribution <- dt[, .N, by = subsection_code][order(-N)]
  }

  if ("exempt_organization_type" %in% names(dt)) {
    report$summary_stats$orgtype_distribution <- dt[, .N, by = exempt_organization_type][order(-N)]
  }

  if ("asset_amount" %in% names(dt)) {
    report$summary_stats$total_assets <- sum(dt$asset_amount, na.rm = TRUE)
    report$summary_stats$median_assets <- median(dt$asset_amount, na.rm = TRUE)
  }

  # Address quality statistics
  if ("org_addr_is_missing" %in% names(dt)) {
    report$summary_stats$address_quality <- list(
      missing_count = sum(dt$org_addr_is_missing == TRUE, na.rm = TRUE),
      po_box_count = sum(dt$org_addr_is_po_box == TRUE, na.rm = TRUE),
      rural_route_count = sum(dt$org_addr_is_rural_route == TRUE, na.rm = TRUE),
      invalid_state_count = sum(dt$org_addr_state_invalid == TRUE, na.rm = TRUE)
    )
  }

  return(report)
}

#' Print Quality Report
#'
#' @description
#' Prints a formatted quality report to the console.
#'
#' @param report list quality report from generate_quality_report()
#'
#' @export
print_quality_report <- function(report) {

  message("")
  message("========================================")
  message("POST-TRANSFORMATION QUALITY REPORT")
  message("========================================")
  message(sprintf("Timestamp: %s", report$timestamp))
  message(sprintf("Final row count: %s", format(report$row_count, big.mark = ",")))
  message(sprintf("Final column count: %d", report$column_count))
  message(sprintf("Row preservation: %s",
                  ifelse(report$row_preservation, "PASSED", "FAILED")))
  message("")

  # Missing columns
  if (length(report$missing_columns) > 0) {
    message("Missing expected columns:")
    for (col in report$missing_columns) {
      message(sprintf("  - %s", col))
    }
    message("")
  }

  # Completeness summary
  message("Field Completeness (selected fields):")
  low_completeness <- report$completeness[report$completeness < 100]
  if (length(low_completeness) > 0) {
    low_completeness <- sort(low_completeness)
    for (col in names(head(low_completeness, 10))) {
      message(sprintf("  - %s: %.1f%%", col, low_completeness[col]))
    }
  } else {
    message("  All fields 100% complete")
  }
  message("")

  # Critical field issues
  if (length(report$critical_field_issues) > 0) {
    message("CRITICAL FIELD ISSUES:")
    for (field in names(report$critical_field_issues)) {
      message(sprintf("  - %s: %s NULL values",
                      field,
                      format(report$critical_field_issues[[field]], big.mark = ",")))
    }
    message("")
  }

  # Summary statistics
  if (length(report$summary_stats) > 0) {
    message("Summary Statistics:")
    if (!is.null(report$summary_stats$unique_eins)) {
      message(sprintf("  - Unique EINs: %s",
                      format(report$summary_stats$unique_eins, big.mark = ",")))
    }
    if (!is.null(report$summary_stats$total_assets)) {
      message(sprintf("  - Total Assets: $%s",
                      format(report$summary_stats$total_assets, big.mark = ",")))
    }
    if (!is.null(report$summary_stats$address_quality)) {
      aq <- report$summary_stats$address_quality
      message("  - Address Quality:")
      message(sprintf("      Missing: %s", format(aq$missing_count, big.mark = ",")))
      message(sprintf("      P.O. Box: %s", format(aq$po_box_count, big.mark = ",")))
      message(sprintf("      Rural Route: %s", format(aq$rural_route_count, big.mark = ",")))
      message(sprintf("      Invalid State: %s", format(aq$invalid_state_count, big.mark = ",")))
    }
    message("")
  }

  message(sprintf("OVERALL: %s", ifelse(report$passed, "PASSED", "FAILED")))
  message("========================================")
  message("")
}

#' Save Quality Report to File
#'
#' @description
#' Saves the quality report to a JSON file for auditing purposes.
#'
#' @param report list quality report from generate_quality_report()
#' @param output_path character path to save JSON report
#'
#' @export
save_quality_report <- function(report, output_path) {

  # Convert data.tables to data.frames for JSON serialization
  report_json <- report
  if (!is.null(report_json$summary_stats$subsection_distribution)) {
    report_json$summary_stats$subsection_distribution <-
      as.data.frame(report_json$summary_stats$subsection_distribution)
  }
  if (!is.null(report_json$summary_stats$orgtype_distribution)) {
    report_json$summary_stats$orgtype_distribution <-
      as.data.frame(report_json$summary_stats$orgtype_distribution)
  }

  jsonlite::write_json(
    report_json,
    output_path,
    pretty = TRUE,
    auto_unbox = TRUE
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
