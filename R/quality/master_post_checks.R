# ============================================================================
# master_post_checks.R
#
# Quality checks specific to the Master BMF: row-count is unique-EIN,
# source coverage, vintage histogram, year-range sanity, completeness
# of master-only fields.
#
# Returns a JSON-serializable list compatible with save_quality_report().
# ============================================================================

#' Generate the Master BMF quality report
#'
#' @param con DuckDB connection (must contain bmf_master)
#' @param inputs Discovery output from discover_master_inputs()
#' @return list with quality metrics; printable / serializable
generate_master_quality_report <- function(con, inputs) {
  q <- function(sql) DBI::dbGetQuery(con, sql)

  total_rows <- q("SELECT COUNT(*) AS n FROM bmf_master")$n
  distinct_eins <- q("SELECT COUNT(DISTINCT ein) AS n FROM bmf_master")$n

  by_source <- q("
    SELECT bmf_source, COUNT(*) AS n_rows
      FROM bmf_master
     GROUP BY bmf_source
     ORDER BY bmf_source
  ")

  vintage_hist <- q("
    SELECT bmf_vintage_ym, bmf_source, COUNT(*) AS n_rows
      FROM bmf_master
     GROUP BY bmf_vintage_ym, bmf_source
     ORDER BY bmf_vintage_ym
  ")

  year_range <- q("
    SELECT MIN(first_year_in_bmf) AS min_first_year,
           MAX(first_year_in_bmf) AS max_first_year,
           MIN(last_year_in_bmf)  AS min_last_year,
           MAX(last_year_in_bmf)  AS max_last_year
      FROM bmf_master
  ")

  vintages_observed <- q("
    SELECT MIN(bmf_vintages_observed) AS min_vintages,
           AVG(bmf_vintages_observed) AS avg_vintages,
           MAX(bmf_vintages_observed) AS max_vintages,
           SUM(CASE WHEN bmf_vintages_observed = 1 THEN 1 ELSE 0 END) AS n_only_once
      FROM bmf_master
  ")

  # Per-column completeness on the master table.
  col_names <- DBI::dbListFields(con, "bmf_master")
  completeness <- vapply(col_names, function(col) {
    sql <- sprintf(
      "SELECT 100.0 * SUM(CASE WHEN %s IS NULL THEN 0 ELSE 1 END) / COUNT(*) AS pct FROM bmf_master",
      DBI::dbQuoteIdentifier(con, col)
    )
    q(sql)$pct
  }, numeric(1))
  completeness_dt <- data.table::data.table(
    column = col_names,
    pct_non_null = round(as.numeric(completeness), 2)
  )
  data.table::setorder(completeness_dt, -pct_non_null)

  # Pre-condition checks.
  ein_unique <- (total_rows == distinct_eins)
  inputs_n   <- nrow(inputs)
  vintage_n  <- nrow(vintage_hist)

  passed <- ein_unique &&
            inputs_n > 0 &&
            total_rows > 0

  list(
    timestamp        = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
    passed           = passed,
    total_rows       = total_rows,
    distinct_eins    = distinct_eins,
    ein_unique       = ein_unique,
    n_input_files    = inputs_n,
    n_vintages       = vintage_n,
    rows_by_source   = as.list(by_source),
    year_range       = as.list(year_range),
    vintages_observed = as.list(vintages_observed),
    vintage_histogram = vintage_hist,
    completeness     = completeness_dt
  )
}

#' Print a human-readable summary of the master quality report
#'
#' @param report list returned by generate_master_quality_report()
print_master_quality_report <- function(report) {
  message("================ Master BMF Quality Report ================")
  message(sprintf("Timestamp:        %s", report$timestamp))
  message(sprintf("Passed:           %s", report$passed))
  message(sprintf("Total rows:       %s", format(report$total_rows, big.mark = ",")))
  message(sprintf("Distinct EINs:    %s", format(report$distinct_eins, big.mark = ",")))
  message(sprintf("EIN-unique:       %s", report$ein_unique))
  message(sprintf("Input files:      %d", report$n_input_files))
  message(sprintf("Vintages stacked: %d", report$n_vintages))
  message("Rows by source:")
  print(report$rows_by_source)
  message("Year range (first_year_in_bmf .. last_year_in_bmf):")
  print(report$year_range)
  message("Vintages observed per EIN:")
  print(report$vintages_observed)
  message("Top 20 most-complete columns:")
  print(head(report$completeness, 20))
  message("===========================================================")
}

#' Save the master quality report as JSON
save_master_quality_report <- function(report, path) {
  if (!dir.exists(dirname(path))) dir.create(dirname(path), recursive = TRUE)
  jsonlite::write_json(report, path, auto_unbox = TRUE, pretty = TRUE,
                       dataframe = "rows", null = "null")
  log_info(sprintf("Master quality report saved: %s", path))
}

#' Render the master quality report to HTML
#'
#' Uses Quarto + R/quality/master_quality_report_template.qmd. The
#' report list is staged to a temp RDS so the template can readRDS()
#' it via the report_data_path parameter.
#'
#' @param report list returned by generate_master_quality_report()
#' @param output_path target HTML path
#' @return Invisibly: the output path on success, NULL on failure
render_master_quality_report <- function(report, output_path) {
  if (!requireNamespace("quarto", quietly = TRUE)) {
    log_warn("'quarto' R package not installed — skipping HTML report")
    return(invisible(NULL))
  }

  template_path <- here::here("R", "quality", "master_quality_report_template.qmd")
  if (!file.exists(template_path)) {
    log_warn(sprintf("Master template not found at %s — skipping HTML report",
                     template_path))
    return(invisible(NULL))
  }

  output_dir <- dirname(output_path)
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

  temp_rds <- tempfile(fileext = ".rds")
  saveRDS(report, temp_rds)
  on.exit(unlink(temp_rds), add = TRUE)

  output_file <- basename(output_path)

  log_info("Rendering master quality report to HTML...")
  tryCatch({
    quarto::quarto_render(
      input          = template_path,
      output_format  = "html",
      output_file    = output_file,
      execute_params = list(report_data_path = temp_rds),
      quiet          = TRUE
    )
    rendered_file <- file.path(dirname(template_path), output_file)
    if (!file.exists(rendered_file)) {
      rendered_file <- file.path(
        dirname(template_path),
        sub("\\.qmd$", ".html", basename(template_path))
      )
    }
    if (file.exists(rendered_file) &&
        normalizePath(rendered_file) != normalizePath(output_path, mustWork = FALSE)) {
      file.copy(rendered_file, output_path, overwrite = TRUE)
      unlink(rendered_file)
    }
    log_info(sprintf("Master quality report HTML rendered: %s", output_path))
    invisible(output_path)
  }, error = function(e) {
    log_warn(sprintf("HTML render failed (non-fatal): %s", conditionMessage(e)))
    invisible(NULL)
  })
}
