# Adding the derived organization columns (role, legal type and field, each
# as a value and in words) to the finished Unified BMF table (ADR 0058 and
# ADR 0059). Each test builds a small one-row-per-EIN table in an
# in-memory DuckDB database, runs the step the build runs, and checks the
# table a reader would get.
# Run: Rscript -e 'testthat::test_dir("tests/testthat")'

library(testthat)

repo_root <- normalizePath(file.path(testthat::test_path(), "..", ".."))
source(file.path(repo_root, "R", "nccs_org_role.R"))
source(file.path(repo_root, "R", "nccs_org_legal_type.R"))
source(file.path(repo_root, "R", "nccs_org_field.R"))
source(file.path(repo_root, "R", "unified_bmf_org_columns.R"))

derived_org_columns <- c(
  "nccs_org_role",
  "nccs_org_role_definition",
  "nccs_org_legal_type",
  "nccs_org_legal_type_definition",
  "nccs_org_field",
  "nccs_org_field_definition"
)

# ============================================================================
# 1. A small Unified BMF table to work on
# ============================================================================

small_unified_bmf <- data.frame(
  ein = c(
    "01-0000001", "01-0000002", "01-0000003", "01-0000004",
    "01-0000005", "01-0000006", "01-0000007"
  ),
  org_name_display = c(
    "Operating Org", "Support Org", "Members Org", "Private Fund",
    "No Code Org", "Comma Org", "No Foundation Code Org"
  ),
  ntee_code_raw = c("P20", "B11", "Y42", "P20", NA, "B12,", "A20"),
  subsection_code = c("3", "3", "8", "3", "3", "3", "3"),
  foundation_code = c("15", "15", "0", "4", "15", "16", "0"),
  EIN2 = c(
    "EIN-01-0000001", "EIN-01-0000002", "EIN-01-0000003", "EIN-01-0000004",
    "EIN-01-0000005", "EIN-01-0000006", "EIN-01-0000007"
  ),
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

test_that("the six derived columns are added at the end, after every existing column", {
  connection <- open_small_unified_bmf()
  on.exit(DBI::dbDisconnect(connection, shutdown = TRUE))

  add_nccs_org_columns_to_unified_bmf(connection)

  expect_identical(
    DBI::dbListFields(connection, "bmf_master"),
    c(names(small_unified_bmf), derived_org_columns)
  )
})

test_that("each organization gets the role the rule gives it", {
  connection <- open_small_unified_bmf()
  on.exit(DBI::dbDisconnect(connection, shutdown = TRUE))

  add_nccs_org_columns_to_unified_bmf(connection)

  unified_bmf <- read_unified_bmf(connection)

  expect_identical(unified_bmf$nccs_org_role, c("O", "S", "M", "S", NA, "S", "O"))

  expect_identical(
    unified_bmf$nccs_org_role_definition,
    c("Operating", "Supporting", "Mutual benefit", "Supporting", NA, "Supporting", "Operating")
  )
})

test_that("each organization gets the legal type the rule gives it", {
  connection <- open_small_unified_bmf()
  on.exit(DBI::dbDisconnect(connection, shutdown = TRUE))

  add_nccs_org_columns_to_unified_bmf(connection)

  unified_bmf <- read_unified_bmf(connection)

  # The last organization is a 501(c)(3) with no foundation code: unknown.
  expect_identical(unified_bmf$nccs_org_legal_type, c("PC", "PC", "O", "PF", "PC", "PC", NA))

  expect_identical(
    unified_bmf$nccs_org_legal_type_definition,
    c(
      "Public charity", "Public charity", "Other tax-exempt organization",
      "Private foundation", "Public charity", "Public charity", NA
    )
  )
})

test_that("each organization gets the field the rule gives it", {
  connection <- open_small_unified_bmf()
  on.exit(DBI::dbDisconnect(connection, shutdown = TRUE))

  add_nccs_org_columns_to_unified_bmf(connection)

  unified_bmf <- read_unified_bmf(connection)

  expect_identical(unified_bmf$nccs_org_field, c("HS", "ZA", "MO", "HS", NA, "ZB", "AR"))

  expect_identical(
    unified_bmf$nccs_org_field_definition,
    c(
      "Human services", "Single organization support", "Other mutual benefit",
      "Human services", NA, "Fundraising within one field",
      "Arts, culture and humanities"
    )
  )
})

test_that("existing columns and rows are left exactly as they were", {
  connection <- open_small_unified_bmf()
  on.exit(DBI::dbDisconnect(connection, shutdown = TRUE))

  add_nccs_org_columns_to_unified_bmf(connection)

  unified_bmf <- read_unified_bmf(connection)

  expect_identical(unified_bmf[names(small_unified_bmf)], small_unified_bmf)
})

test_that("the step reports how many organizations it worked on", {
  connection <- open_small_unified_bmf()
  on.exit(DBI::dbDisconnect(connection, shutdown = TRUE))

  expect_identical(add_nccs_org_columns_to_unified_bmf(connection), 7L)
})

# ============================================================================
# 3. Running it again, and refusing bad tables
# ============================================================================

test_that("running the step twice gives the same table as running it once", {
  connection <- open_small_unified_bmf()
  on.exit(DBI::dbDisconnect(connection, shutdown = TRUE))

  add_nccs_org_columns_to_unified_bmf(connection)

  after_first_run <- read_unified_bmf(connection)

  add_nccs_org_columns_to_unified_bmf(connection)

  expect_identical(read_unified_bmf(connection), after_first_run)

  expect_identical(
    DBI::dbListFields(connection, "bmf_master"),
    c(names(small_unified_bmf), derived_org_columns)
  )
})

test_that("a table without one of the three input columns is refused and left alone", {
  rows_without_foundation_code <- small_unified_bmf[names(small_unified_bmf) != "foundation_code"]

  connection <- open_small_unified_bmf(rows_without_foundation_code)
  on.exit(DBI::dbDisconnect(connection, shutdown = TRUE))

  expect_error(
    add_nccs_org_columns_to_unified_bmf(connection),
    "missing the column\\(s\\) the derived columns are worked out from: foundation_code"
  )

  expect_identical(DBI::dbListFields(connection, "bmf_master"), names(rows_without_foundation_code))
})

test_that("a table with a repeated EIN is refused and left alone", {
  rows_with_repeated_ein <- rbind(small_unified_bmf, small_unified_bmf[1, ])

  connection <- open_small_unified_bmf(rows_with_repeated_ein)
  on.exit(DBI::dbDisconnect(connection, shutdown = TRUE))

  expect_error(add_nccs_org_columns_to_unified_bmf(connection), "one row per EIN")

  expect_identical(DBI::dbListFields(connection, "bmf_master"), names(small_unified_bmf))
})

test_that("a table with a missing EIN is refused and left alone", {
  rows_with_missing_ein <- small_unified_bmf
  rows_with_missing_ein$ein[2] <- NA

  connection <- open_small_unified_bmf(rows_with_missing_ein)
  on.exit(DBI::dbDisconnect(connection, shutdown = TRUE))

  expect_error(add_nccs_org_columns_to_unified_bmf(connection), "one row per EIN")

  expect_identical(DBI::dbListFields(connection, "bmf_master"), names(small_unified_bmf))
})

test_that("a failure part-way through leaves the table exactly as it was", {
  connection <- open_small_unified_bmf()
  on.exit(DBI::dbDisconnect(connection, shutdown = TRUE))

  # A table already holding the step's working name makes the swap fail
  # after the columns have been derived.
  DBI::dbExecute(connection, "CREATE TABLE bmf_master_with_org_columns AS SELECT 1 AS placeholder")

  expect_error(add_nccs_org_columns_to_unified_bmf(connection))

  expect_identical(read_unified_bmf(connection), small_unified_bmf)

  expect_identical(
    DBI::dbGetQuery(connection, "SELECT * FROM bmf_master_with_org_columns"),
    data.frame(placeholder = 1L)
  )
})

test_that("a failure after the copy is made leaves no half-finished copy behind", {
  connection <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  on.exit(DBI::dbDisconnect(connection, shutdown = TRUE))

  # Another table that points at the Unified BMF table stops the database
  # from dropping it, so the swap fails after the copy has been created.
  DBI::dbExecute(connection, "
    CREATE TABLE bmf_master (
      ein VARCHAR PRIMARY KEY,
      ntee_code_raw VARCHAR,
      subsection_code VARCHAR,
      foundation_code VARCHAR
    )
  ")
  DBI::dbExecute(connection, "INSERT INTO bmf_master VALUES ('01-0000001', 'P20', '3', '15')")
  DBI::dbExecute(connection, "CREATE TABLE points_at_bmf (ein VARCHAR REFERENCES bmf_master (ein))")

  expect_error(add_nccs_org_columns_to_unified_bmf(connection))

  expect_identical(
    DBI::dbListFields(connection, "bmf_master"),
    c("ein", "ntee_code_raw", "subsection_code", "foundation_code")
  )

  expect_false(DBI::dbExistsTable(connection, "bmf_master_with_org_columns"))
})

test_that("the step can run again after a failed attempt", {
  connection <- open_small_unified_bmf()
  on.exit(DBI::dbDisconnect(connection, shutdown = TRUE))

  DBI::dbExecute(connection, "CREATE TABLE bmf_master_with_org_columns AS SELECT 1 AS placeholder")

  expect_error(add_nccs_org_columns_to_unified_bmf(connection))

  DBI::dbExecute(connection, "DROP TABLE bmf_master_with_org_columns")

  expect_identical(add_nccs_org_columns_to_unified_bmf(connection), 7L)

  expect_identical(read_unified_bmf(connection)$nccs_org_role, c("O", "S", "M", "S", NA, "S", "O"))
})

test_that("derived columns left by an earlier build are replaced, whatever they held", {
  rows_with_stale_columns <- small_unified_bmf
  rows_with_stale_columns$nccs_org_field <- "XX"
  rows_with_stale_columns$nccs_org_role <- "X"

  connection <- open_small_unified_bmf(rows_with_stale_columns)
  on.exit(DBI::dbDisconnect(connection, shutdown = TRUE))

  add_nccs_org_columns_to_unified_bmf(connection)

  unified_bmf <- read_unified_bmf(connection)

  expect_identical(names(unified_bmf), c(names(small_unified_bmf), derived_org_columns))

  expect_identical(unified_bmf$nccs_org_field, c("HS", "ZA", "MO", "HS", NA, "ZB", "AR"))
})

# ============================================================================
# 4. Counts to read before publishing
# ============================================================================

test_that("organizations are counted by every value of every derived column, with unknown in its own row", {
  connection <- open_small_unified_bmf()
  on.exit(DBI::dbDisconnect(connection, shutdown = TRUE))

  add_nccs_org_columns_to_unified_bmf(connection)

  expect_identical(
    count_unified_bmf_by_org_columns(connection),
    data.frame(
      column = c(
        rep("nccs_org_role", 4),
        rep("nccs_org_legal_type", 4),
        rep("nccs_org_field", 6)
      ),
      value = c(
        "M", "O", "S", NA,
        "O", "PC", "PF", NA,
        "AR", "HS", "MO", "ZA", "ZB", NA
      ),
      definition = c(
        "Mutual benefit", "Operating", "Supporting", NA,
        "Other tax-exempt organization", "Public charity", "Private foundation", NA,
        "Arts, culture and humanities", "Human services", "Other mutual benefit",
        "Single organization support", "Fundraising within one field", NA
      ),
      organizations = c(
        1L, 2L, 3L, 1L,
        1L, 4L, 1L, 1L,
        1L, 2L, 1L, 1L, 1L, 1L
      ),
      stringsAsFactors = FALSE
    )
  )
})

test_that("counting is refused for a table that does not have the derived columns", {
  connection <- open_small_unified_bmf()
  on.exit(DBI::dbDisconnect(connection, shutdown = TRUE))

  expect_error(
    count_unified_bmf_by_org_columns(connection),
    "does not have the derived column\\(s\\): nccs_org_role"
  )
})

test_that("counting is refused when only one derived column is absent", {
  connection <- open_small_unified_bmf()
  on.exit(DBI::dbDisconnect(connection, shutdown = TRUE))

  add_nccs_org_columns_to_unified_bmf(connection)

  DBI::dbExecute(connection, "ALTER TABLE bmf_master DROP COLUMN nccs_org_field_definition")

  expect_error(
    count_unified_bmf_by_org_columns(connection),
    "does not have the derived column\\(s\\): nccs_org_field_definition"
  )
})

# ============================================================================
# 5. The data dictionary knows the new columns
# ============================================================================

test_that("every column the step adds has a description in the data dictionary", {
  column_descriptions <- utils::read.csv(
    file.path(repo_root, "data", "dictionaries", "column_descriptions.csv"),
    stringsAsFactors = FALSE
  )

  connection <- open_small_unified_bmf()
  on.exit(DBI::dbDisconnect(connection, shutdown = TRUE))

  add_nccs_org_columns_to_unified_bmf(connection)

  added_columns <- setdiff(DBI::dbListFields(connection, "bmf_master"), names(small_unified_bmf))

  described <- column_descriptions$description[match(added_columns, column_descriptions$column_name)]

  expect_identical(added_columns, derived_org_columns)

  expect_true(all(!is.na(described) & nchar(described) > 0))
})
