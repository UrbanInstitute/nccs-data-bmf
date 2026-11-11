# The Unified BMF validation report as a whole: it carries the counts for
# the derived organization columns and fails when one of them holds a value
# it must not hold (ADR 0059 Decision 10). Each test builds a small Unified
# BMF table in an in-memory DuckDB database and makes the report the way
# the build does.
# Run: Rscript -e 'testthat::test_dir("tests/testthat")'

library(testthat)
library(data.table)

repo_root <- normalizePath(file.path(testthat::test_path(), "..", ".."))
source(file.path(repo_root, "R", "nccs_org_role.R"))
source(file.path(repo_root, "R", "nccs_org_legal_type.R"))
source(file.path(repo_root, "R", "nccs_org_field.R"))
source(file.path(repo_root, "R", "unified_bmf_org_columns.R"))
source(file.path(repo_root, "R", "quality", "unified_bmf_org_column_checks.R"))
source(file.path(repo_root, "R", "quality", "master_post_checks.R"))

# ============================================================================
# 1. A small Unified BMF table and the list of files it was built from
# ============================================================================

small_unified_bmf <- data.frame(
  ein = c("01-0000001", "01-0000002", "01-0000003"),
  ntee_code_raw = c("P20", "B11", NA),
  subsection_code = c("3", "3", "8"),
  foundation_code = c("15", "4", "0"),
  bmf_source = c("current", "current", "legacy"),
  bmf_vintage_ym = c("2026-11", "2026-11", "2013-07"),
  first_year_in_bmf = c(2001L, 2010L, 1999L),
  last_year_in_bmf = c(2026L, 2026L, 2013L),
  bmf_vintages_observed = c(40L, 12L, 3L),
  stringsAsFactors = FALSE
)

input_files <- data.frame(
  bmf_source = c("current", "legacy"),
  stringsAsFactors = FALSE
)

open_small_unified_bmf <- function() {
  connection <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")

  DBI::dbWriteTable(connection, "bmf_master", small_unified_bmf)

  add_nccs_org_columns_to_unified_bmf(connection)

  return(connection)
}

# ============================================================================
# 2. What the report carries and when it fails
# ============================================================================

test_that("the report passes and counts every value of the three derived columns", {
  connection <- open_small_unified_bmf()
  on.exit(DBI::dbDisconnect(connection, shutdown = TRUE))

  report <- generate_master_quality_report(connection, input_files)

  expect_true(report$passed)

  expect_true(report$derived_org_columns$passed)

  counts <- report$derived_org_columns$counts

  expect_identical(
    unique(counts$column),
    c("nccs_org_role", "nccs_org_legal_type", "nccs_org_field")
  )

  expect_identical(
    counts$organizations[counts$column == "nccs_org_legal_type"],
    c(1L, 1L, 1L)
  )

  expect_identical(
    counts$value[counts$column == "nccs_org_field"],
    c("HS", "ZA", NA)
  )
})

test_that("the report fails when a derived column holds a value outside its allowed list", {
  connection <- open_small_unified_bmf()
  on.exit(DBI::dbDisconnect(connection, shutdown = TRUE))

  DBI::dbExecute(connection, "UPDATE bmf_master SET nccs_org_field = 'XX' WHERE ein = '01-0000001'")

  report <- generate_master_quality_report(connection, input_files)

  expect_false(report$passed)

  expect_identical(report$derived_org_columns$problems$value, "XX")
})

test_that("the report fails when a value and its words disagree", {
  connection <- open_small_unified_bmf()
  on.exit(DBI::dbDisconnect(connection, shutdown = TRUE))

  DBI::dbExecute(
    connection,
    "UPDATE bmf_master SET nccs_org_role_definition = 'Supporting' WHERE ein = '01-0000001'"
  )

  report <- generate_master_quality_report(connection, input_files)

  expect_false(report$passed)

  expect_identical(report$derived_org_columns$problems$column, "nccs_org_role")
})

test_that("the report shows the change from the previous build's counts", {
  connection <- open_small_unified_bmf()
  on.exit(DBI::dbDisconnect(connection, shutdown = TRUE))

  previous_counts <- data.frame(
    column = c("nccs_org_legal_type", "nccs_org_legal_type"),
    value = c("PC", "O"),
    definition = c("Public charity", "Other tax-exempt organization"),
    organizations = c(3L, 1L),
    stringsAsFactors = FALSE
  )

  report <- generate_master_quality_report(connection, input_files, previous_counts)

  counts <- report$derived_org_columns$counts

  legal_type_counts <- counts[counts$column == "nccs_org_legal_type", ]

  expect_identical(legal_type_counts$value, c("O", "PC", "PF"))

  # The previous build had no private foundation, so its count there is 0.
  expect_identical(legal_type_counts$change, c(0L, -2L, 1L))

  expect_true(report$derived_org_columns$has_previous_counts)
})

test_that("the summary printed for the build log names a problem in a derived column", {
  connection <- open_small_unified_bmf()
  on.exit(DBI::dbDisconnect(connection, shutdown = TRUE))

  DBI::dbExecute(connection, "UPDATE bmf_master SET nccs_org_field = 'XX' WHERE ein = '01-0000001'")

  report <- generate_master_quality_report(connection, input_files)

  printed <- capture.output(
    printed_messages <- testthat::capture_messages(print_master_quality_report(report))
  )

  all_printed <- paste(c(printed, printed_messages), collapse = "\n")

  expect_match(all_printed, "FAILED: 1 problem\\(s\\) in the derived organization columns")

  expect_match(all_printed, "value is not in the allowed list")
})
