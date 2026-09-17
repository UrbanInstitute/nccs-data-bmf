# Tests for scripts/check_ntee_gap_vintage_diff.R, the per-file before/after
# gate used for the Z18 reprocess.
#
# Each test builds a small BEFORE file (what the pipeline wrote before the fix:
# the 12 restored codes come out INVALID / Z99) and an AFTER file (what the
# reprocess writes), using the real transform and the real lookup workbook.
# Then one thing is broken on purpose and the script must notice.

library(testthat)
library(data.table)   # R/transform_ntee_code.R uses %chin% unqualified

repo_root   <- normalizePath(file.path(testthat::test_path(), "..", ".."))
script_path <- file.path(repo_root, "scripts", "check_ntee_gap_vintage_diff.R")
source(file.path(repo_root, "R", "transform_ntee_code.R"))

# ---------------------------------------------------------------------------
# Lookups
# ---------------------------------------------------------------------------

lookup_path <- file.path(repo_root, "data", "lookup", "bmf_code_lookup.xlsx")
read_lookup_sheet <- function(sheet_name) {
  data.table::setDT(openxlsx::read.xlsx(lookup_path, sheet = sheet_name))
}

ntee_code_lookup   <- read_lookup_sheet("ntee_code")
major_group_lookup <- read_lookup_sheet("ntee_code_major_group")
common_code_lookup <- read_lookup_sheet("ntee_common_code")
subsector_lookup   <- read_lookup_sheet("nteev2_subsector")

legacy_crosswalk <- data.table::fread(
  file.path(repo_root, "data", "lookup", "ntee_legacy_5char_lookup.csv")
)
legacy_crosswalk[, NTEE := toupper(trimws(NTEE))]
data.table::setkey(legacy_crosswalk, NTEE)

restored_codes <- c("B29", "E6A", "F31", "K2A", "K2B", "K2C",
                    "L4A", "L4B", "M99", "P76", "P7A", "P83")

# The lookup as it was before PR #54: without the 12 restored codes.
pre_fix_ntee_code_lookup <- ntee_code_lookup[!ntee_code %in% restored_codes]

# ---------------------------------------------------------------------------
# Fixture builders
# ---------------------------------------------------------------------------

# Run the real transform over a vector of raw codes with a given lookup and
# return a small processed-file-like table.
build_processed_rows <- function(raw_codes, code_lookup) {
  input <- data.table::data.table(
    ein      = sprintf("10-%07d", seq_along(raw_codes)),
    org_name = paste0("ORG ", seq_along(raw_codes)),
    NTEE_CD  = raw_codes
  )

  output <- NULL
  invisible(capture.output(
    output <- suppressMessages(suppressWarnings(transform_ntee_code(
      input,
      ntee_code_lookup         = code_lookup,
      ntee_major_group_lookup  = major_group_lookup,
      ntee_common_code_lookup  = common_code_lookup,
      nteev2_subsector_lookup  = subsector_lookup,
      ntee_legacy_5char_lookup = legacy_crosswalk,
      year = "0000", path = NULL, write_scd = FALSE
    )))
  ))

  output[, NTEE_CD := NULL]
  output
}

fixture_codes <- c(restored_codes, "A20", "B21", "P20", "X20", "Z99", "B11")

before_rows <- build_processed_rows(fixture_codes, pre_fix_ntee_code_lookup)
after_rows  <- build_processed_rows(fixture_codes, ntee_code_lookup)

# Sanity: the fixture really reproduces the pre-fix behaviour.
stopifnot(all(before_rows[ntee_code_raw %in% restored_codes, nteev2_code] == "Z99"))

# Write both tables to temporary CSVs, run the script, and return its exit
# status and its last output line.
run_checker <- function(before_table, after_table, label = "case") {
  before_path <- tempfile(fileext = ".csv")
  after_path  <- tempfile(fileext = ".csv")
  data.table::fwrite(before_table, before_path)
  data.table::fwrite(after_table,  after_path)

  output <- suppressWarnings(system2(
    "Rscript", c("--vanilla", script_path, before_path, after_path, label),
    stdout = TRUE, stderr = FALSE
  ))
  status <- attr(output, "status")
  if (is.null(status)) status <- 0L

  list(status = status, line = tail(output, 1))
}

# Pick one pipe-delimited field out of the result line.
result_field <- function(result, position) {
  strsplit(result$line, "|", fixed = TRUE)[[1]][position]
}

# Field positions in the result line, for readability below.
FIELD_GAP_ROWS          <- 6
FIELD_ROWS_CHANGED      <- 7
FIELD_CHANGED_OUTSIDE   <- 8
FIELD_NOT_Z99_BEFORE    <- 9
FIELD_WRONG_AFTER       <- 10
FIELD_COLS_OUTSIDE      <- 11

# ---------------------------------------------------------------------------
# Tests
# ---------------------------------------------------------------------------

test_that("a correct reprocess passes, with major-group changes allowed", {
  result <- run_checker(before_rows, after_rows)

  expect_equal(result$status, 0L, info = result$line)
  expect_equal(result_field(result, FIELD_GAP_ROWS), "12")
  expect_equal(result_field(result, FIELD_ROWS_CHANGED), "12")
  expect_true(any(before_rows$ntee_code_major_group != after_rows$ntee_code_major_group))
})

test_that("row order does not matter, including with duplicate EINs", {
  duplicate_before <- before_rows[ntee_code_raw == "B29"][, org_name := "ORG DUP"]
  duplicate_after  <- after_rows[ntee_code_raw == "B29"][, org_name := "ORG DUP"]

  before_with_duplicate <- rbind(before_rows, duplicate_before)
  after_with_duplicate  <- rbind(after_rows,  duplicate_after)
  after_shuffled        <- after_with_duplicate[sample(.N)]

  expect_equal(run_checker(before_with_duplicate, after_shuffled)$status, 0L)
})

test_that("a missing derived value after the fix fails (NA-aware)", {
  after_with_missing <- data.table::copy(after_rows)
  after_with_missing[ntee_code_raw == "B29", nteev2_code := NA_character_]

  result <- run_checker(before_rows, after_with_missing)

  expect_equal(result$status, 1L)
  expect_equal(result_field(result, FIELD_WRONG_AFTER), "1")
})

test_that("passing a post-fix file as BEFORE fails (precondition)", {
  result <- run_checker(after_rows, after_rows)

  expect_equal(result$status, 1L)
  expect_equal(result_field(result, FIELD_NOT_Z99_BEFORE), "12")
})

test_that("wrong derived values on a gap row fail", {
  after_with_wrong_values <- data.table::copy(after_rows)
  after_with_wrong_values[ntee_code_raw == "B29",
                          `:=`(ntee_code_clean = "WRONG", nteev2_code = "A00")]

  result <- run_checker(before_rows, after_with_wrong_values)

  expect_equal(result$status, 1L)
  expect_equal(result_field(result, FIELD_WRONG_AFTER), "1")
})

test_that("a change outside the gap rows fails", {
  after_with_stray_change <- data.table::copy(after_rows)
  after_with_stray_change[ntee_code_raw == "A20", nteev2_code := "A99"]

  result <- run_checker(before_rows, after_with_stray_change)

  expect_equal(result$status, 1L)
  expect_equal(result_field(result, FIELD_CHANGED_OUTSIDE), "1")
})

test_that("a change outside the allowed columns fails", {
  after_with_renamed_org <- data.table::copy(after_rows)
  after_with_renamed_org[1, org_name := "RENAMED"]

  result <- run_checker(before_rows, after_with_renamed_org)

  expect_equal(result$status, 1L)
  expect_equal(result_field(result, FIELD_COLS_OUTSIDE), "org_name")
})

test_that("missing required columns fail loudly", {
  result <- run_checker(before_rows[, .(ein, org_name)], after_rows[, .(ein, org_name)])

  expect_equal(result$status, 1L)
  expect_match(result$line, "MISSING-REQUIRED-COLUMNS")
})

test_that("a row-count mismatch fails", {
  expect_equal(run_checker(before_rows, after_rows[-1])$status, 1L)
})

test_that("rows that are identical on every invariant column pair fine", {
  before_with_identical <- rbind(before_rows, before_rows[ntee_code_raw == "B29"])
  after_with_identical  <- rbind(after_rows,  after_rows[ntee_code_raw == "B29"])

  result <- run_checker(before_with_identical, after_with_identical[sample(.N)])

  expect_equal(result$status, 0L)
  expect_equal(result_field(result, FIELD_GAP_ROWS), "13")
})
