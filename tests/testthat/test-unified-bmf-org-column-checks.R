# The part of the Unified BMF validation report that covers the derived
# organization columns: role, legal type and field (ADR 0059 Decision 10).
# Each test hands the check a small table of counts, the way the build does,
# and reads the result a person reading the report would see.
# Run: Rscript -e 'testthat::test_dir("tests/testthat")'

library(testthat)

repo_root <- normalizePath(file.path(testthat::test_path(), "..", ".."))
source(file.path(repo_root, "R", "nccs_org_role.R"))
source(file.path(repo_root, "R", "nccs_org_legal_type.R"))
source(file.path(repo_root, "R", "nccs_org_field.R"))
source(file.path(repo_root, "R", "unified_bmf_org_columns.R"))
source(file.path(repo_root, "R", "quality", "unified_bmf_org_column_checks.R"))

# ============================================================================
# 1. Small tables of counts to work on
# ============================================================================

counts_from_this_build <- data.frame(
  column = c(
    "nccs_org_role", "nccs_org_role", "nccs_org_role",
    "nccs_org_legal_type", "nccs_org_legal_type",
    "nccs_org_field", "nccs_org_field"
  ),
  value = c("O", "S", NA, "PC", NA, "HS", NA),
  definition = c("Operating", "Supporting", NA, "Public charity", NA, "Human services", NA),
  organizations = c(70L, 20L, 10L, 90L, 10L, 85L, 15L),
  stringsAsFactors = FALSE
)

counts_from_previous_build <- data.frame(
  column = c("nccs_org_role", "nccs_org_role", "nccs_org_role", "nccs_org_role"),
  value = c("O", "S", "M", NA),
  definition = c("Operating", "Supporting", "Mutual benefit", NA),
  organizations = c(60L, 20L, 5L, 12L),
  stringsAsFactors = FALSE
)

counts_for_column <- function(counts, column_name) {
  rows <- counts[counts$column == column_name, ]

  rownames(rows) <- NULL

  return(rows)
}

# ============================================================================
# 2. Counts and the change from the previous build
# ============================================================================

test_that("healthy counts pass and keep the count for every value and for blanks", {
  result <- check_unified_bmf_org_columns(counts_from_this_build)

  expect_true(result$passed)

  expect_identical(nrow(result$problems), 0L)

  expect_identical(result$counts$organizations, counts_from_this_build$organizations)

  expect_identical(result$counts$value, counts_from_this_build$value)
})

test_that("with no previous build the change is blank, not zero", {
  result <- check_unified_bmf_org_columns(counts_from_this_build)

  expect_true(all(is.na(result$counts$previous_organizations)))

  expect_true(all(is.na(result$counts$change)))
})

test_that("the change is this build's count minus the previous build's count", {
  result <- check_unified_bmf_org_columns(counts_from_this_build, counts_from_previous_build)

  role_counts <- counts_for_column(result$counts, "nccs_org_role")

  expect_identical(role_counts$value, c("M", "O", "S", NA))

  expect_identical(role_counts$organizations, c(0L, 70L, 20L, 10L))

  expect_identical(role_counts$previous_organizations, c(5L, 60L, 20L, 12L))

  expect_identical(role_counts$change, c(-5L, 10L, 0L, -2L))
})

test_that("a value that was in the previous build and is gone now is shown with a count of zero", {
  result <- check_unified_bmf_org_columns(counts_from_this_build, counts_from_previous_build)

  role_counts <- counts_for_column(result$counts, "nccs_org_role")

  expect_identical(role_counts$definition[role_counts$value %in% "M"], "Mutual benefit")

  expect_true(result$passed)
})

test_that("a value that is new in this build is compared with a previous count of zero", {
  previous_counts_without_supporting <- counts_from_previous_build[
    !counts_from_previous_build$value %in% "S",
  ]

  result <- check_unified_bmf_org_columns(counts_from_this_build, previous_counts_without_supporting)

  role_counts <- counts_for_column(result$counts, "nccs_org_role")

  expect_identical(role_counts$previous_organizations[role_counts$value %in% "S"], 0L)

  expect_identical(role_counts$change[role_counts$value %in% "S"], 20L)
})

test_that("a column the previous build did not have shows a blank change", {
  result <- check_unified_bmf_org_columns(counts_from_this_build, counts_from_previous_build)

  field_counts <- counts_for_column(result$counts, "nccs_org_field")

  expect_identical(field_counts$organizations, c(85L, 15L))

  expect_true(all(is.na(field_counts$previous_organizations)))

  expect_true(all(is.na(field_counts$change)))
})

# ============================================================================
# 3. What stops the publish
# ============================================================================

test_that("a value outside the allowed list fails the check and is named", {
  counts_with_bad_value <- counts_from_this_build
  counts_with_bad_value$value[1] <- "X"

  result <- check_unified_bmf_org_columns(counts_with_bad_value)

  expect_false(result$passed)

  expect_identical(result$problems$column, "nccs_org_role")

  expect_identical(result$problems$value, "X")

  expect_identical(result$problems$problem, "value is not in the allowed list")
})

test_that("a value in words that disagrees with its value fails the check", {
  counts_with_wrong_words <- counts_from_this_build
  counts_with_wrong_words$definition[4] <- "Private foundation"

  result <- check_unified_bmf_org_columns(counts_with_wrong_words)

  expect_false(result$passed)

  expect_identical(result$problems$column, "nccs_org_legal_type")

  expect_identical(result$problems$problem, "the value in words does not match the value")
})

test_that("a blank with words beside it fails the check", {
  counts_with_words_on_blank <- counts_from_this_build
  counts_with_words_on_blank$definition[7] <- "Human services"

  result <- check_unified_bmf_org_columns(counts_with_words_on_blank)

  expect_false(result$passed)

  expect_identical(result$problems$column, "nccs_org_field")
})

test_that("a value with no words beside it fails the check", {
  counts_with_no_words <- counts_from_this_build
  counts_with_no_words$definition[6] <- NA

  result <- check_unified_bmf_org_columns(counts_with_no_words)

  expect_false(result$passed)

  expect_identical(result$problems$value, "HS")
})

test_that("a derived column with no counts at all fails the check", {
  counts_without_field <- counts_from_this_build[counts_from_this_build$column != "nccs_org_field", ]

  result <- check_unified_bmf_org_columns(counts_without_field)

  expect_false(result$passed)

  expect_identical(result$problems$column, "nccs_org_field")

  expect_identical(result$problems$problem, "the column has no counts")

  expect_identical(result$problems$organizations, 0L)
})

test_that("a value from an allowed list of another column fails the check", {
  counts_with_swapped_value <- counts_from_this_build
  counts_with_swapped_value$value[4] <- "HS"
  counts_with_swapped_value$definition[4] <- "Human services"

  result <- check_unified_bmf_org_columns(counts_with_swapped_value)

  expect_false(result$passed)

  expect_identical(result$problems$problem, "value is not in the allowed list")
})

# ============================================================================
# 4. Reading the counts of the previous build
# ============================================================================

test_that("the counts written into one report are read back for the next build", {
  result <- check_unified_bmf_org_columns(counts_from_this_build)

  report_path <- tempfile(fileext = ".json")
  on.exit(unlink(report_path))

  jsonlite::write_json(
    list(timestamp = "2026-11-12T10:00:00", derived_org_columns = result),
    report_path,
    auto_unbox = TRUE,
    pretty = TRUE,
    dataframe = "rows",
    null = "null"
  )

  previous_counts <- read_previous_unified_bmf_org_column_counts(report_path)

  expect_identical(previous_counts$column, counts_from_this_build$column)

  expect_identical(previous_counts$value, counts_from_this_build$value)

  expect_identical(previous_counts$organizations, counts_from_this_build$organizations)

  next_result <- check_unified_bmf_org_columns(counts_from_this_build, previous_counts)

  expect_true(all(next_result$counts$change == 0L))
})

test_that("a previous report from before the derived columns gives no counts", {
  report_path <- tempfile(fileext = ".json")
  on.exit(unlink(report_path))

  jsonlite::write_json(list(timestamp = "2026-10-12T10:00:00", passed = TRUE), report_path, auto_unbox = TRUE)

  expect_null(read_previous_unified_bmf_org_column_counts(report_path))
})

test_that("a previous report that cannot be read gives no counts and says so", {
  expect_warning(
    previous_counts <- read_previous_unified_bmf_org_column_counts(
      file.path(tempdir(), "no-such-report.json")
    ),
    "could not be read"
  )

  expect_null(previous_counts)
})
