# Adding the organization role columns to the finished Unified BMF table
# (ADR 0058). Each test builds a small one-row-per-EIN table in an
# in-memory DuckDB database, runs the step the build runs, and checks the
# table a reader would get.
# Run: Rscript -e 'testthat::test_dir("tests/testthat")'

library(testthat)

repo_root <- normalizePath(file.path(testthat::test_path(), "..", ".."))
source(file.path(repo_root, "R", "nccs_org_role.R"))
source(file.path(repo_root, "R", "unified_bmf_org_role.R"))

# ============================================================================
# 1. A small Unified BMF table to work on
# ============================================================================

small_unified_bmf <- data.frame(
  ein = c("01-0000001", "01-0000002", "01-0000003", "01-0000004", "01-0000005", "01-0000006"),
  org_name_display = c("Operating Org", "Support Org", "Members Org", "Private Fund", "No Code Org", "Comma Org"),
  ntee_code_raw = c("P20", "B11", "Y42", "P20", NA, "B12,"),
  subsection_code = c("3", "3", "8", "3", "3", "3"),
  foundation_code = c("15", "15", "0", "4", "15", "16"),
  EIN2 = c("EIN-01-0000001", "EIN-01-0000002", "EIN-01-0000003", "EIN-01-0000004", "EIN-01-0000005", "EIN-01-0000006"),
  stringsAsFactors = FALSE
)

open_small_unified_bmf <- function(table_rows = small_unified_bmf) {
  connection <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")

  DBI::dbWriteTable(connection, "bmf_master", table_rows)

  return(connection)
}

read_unified_bmf <- function(connection) {
  unified_bmf <- DBI::dbGetQuery(connection, "SELECT * FROM bmf_master ORDER BY ein")

  return(unified_bmf)
}

# ============================================================================
# 2. What the step adds
# ============================================================================

test_that("the two role columns are added at the end, after every existing column", {
  connection <- open_small_unified_bmf()
  on.exit(DBI::dbDisconnect(connection, shutdown = TRUE))

  add_nccs_org_role_to_unified_bmf(connection)

  expect_identical(
    DBI::dbListFields(connection, "bmf_master"),
    c(names(small_unified_bmf), "nccs_org_role", "nccs_org_role_definition")
  )
})

test_that("each organization gets the role the rule gives it", {
  connection <- open_small_unified_bmf()
  on.exit(DBI::dbDisconnect(connection, shutdown = TRUE))

  add_nccs_org_role_to_unified_bmf(connection)

  unified_bmf <- read_unified_bmf(connection)

  expect_identical(unified_bmf$nccs_org_role, c("O", "S", "M", "S", NA, "S"))

  expect_identical(
    unified_bmf$nccs_org_role_definition,
    c("Operating", "Supporting", "Mutual benefit", "Supporting", NA, "Supporting")
  )
})

test_that("existing columns and rows are left exactly as they were", {
  connection <- open_small_unified_bmf()
  on.exit(DBI::dbDisconnect(connection, shutdown = TRUE))

  add_nccs_org_role_to_unified_bmf(connection)

  unified_bmf <- read_unified_bmf(connection)

  expect_identical(unified_bmf[names(small_unified_bmf)], small_unified_bmf)
})

test_that("the step reports how many organizations it worked on", {
  connection <- open_small_unified_bmf()
  on.exit(DBI::dbDisconnect(connection, shutdown = TRUE))

  expect_identical(add_nccs_org_role_to_unified_bmf(connection), 6L)
})

# ============================================================================
# 3. Running it again, and refusing bad tables
# ============================================================================

test_that("running the step twice gives the same table as running it once", {
  connection <- open_small_unified_bmf()
  on.exit(DBI::dbDisconnect(connection, shutdown = TRUE))

  add_nccs_org_role_to_unified_bmf(connection)

  after_first_run <- read_unified_bmf(connection)

  add_nccs_org_role_to_unified_bmf(connection)

  expect_identical(read_unified_bmf(connection), after_first_run)

  expect_identical(
    DBI::dbListFields(connection, "bmf_master"),
    c(names(small_unified_bmf), "nccs_org_role", "nccs_org_role_definition")
  )
})

test_that("a table without one of the three input columns is refused and left alone", {
  rows_without_foundation_code <- small_unified_bmf[names(small_unified_bmf) != "foundation_code"]

  connection <- open_small_unified_bmf(rows_without_foundation_code)
  on.exit(DBI::dbDisconnect(connection, shutdown = TRUE))

  expect_error(
    add_nccs_org_role_to_unified_bmf(connection),
    "missing the column\\(s\\) the role is derived from: foundation_code"
  )

  expect_identical(DBI::dbListFields(connection, "bmf_master"), names(rows_without_foundation_code))
})

test_that("a table with a repeated EIN is refused and left alone", {
  rows_with_repeated_ein <- rbind(small_unified_bmf, small_unified_bmf[1, ])

  connection <- open_small_unified_bmf(rows_with_repeated_ein)
  on.exit(DBI::dbDisconnect(connection, shutdown = TRUE))

  expect_error(add_nccs_org_role_to_unified_bmf(connection), "one row per EIN")

  expect_identical(DBI::dbListFields(connection, "bmf_master"), names(small_unified_bmf))
})

# ============================================================================
# 4. Counts to read before publishing
# ============================================================================

test_that("organizations are counted by role, with unknown in its own row", {
  connection <- open_small_unified_bmf()
  on.exit(DBI::dbDisconnect(connection, shutdown = TRUE))

  add_nccs_org_role_to_unified_bmf(connection)

  expect_identical(
    count_unified_bmf_by_org_role(connection),
    data.frame(
      nccs_org_role = c("M", "O", "S", NA),
      nccs_org_role_definition = c("Mutual benefit", "Operating", "Supporting", NA),
      organizations = c(1L, 1L, 3L, 1L),
      stringsAsFactors = FALSE
    )
  )
})
