# Backlog Z9: the quality report must fail when a transformation empties a
# column whose source had values, and must not fail for columns whose source
# was already empty (old legacy months lack many IRS fields).

library(data.table)
# pre_checks.R first and alone: it must work without post_checks.R (review of PR #53)
source(here::here("R", "quality", "pre_checks.R"))
stopifnot(exists("count_nonempty_values"))
source(here::here("R", "quality", "post_checks.R"))

raw <- data.table(
  EIN    = c("010000001", "010000002", "010000003"),
  NAME   = c("A", "B", "C"),
  STREET = c("1 MAIN ST", "2 OAK AVE", "PO BOX 3"),
  ZIP    = c("02138", "2139", "02140"),
  ICO    = c("", "", ""),           # legitimately empty source
  ACTIVITY = c(NA_character_, NA_character_, NA_character_)
)

test_that("count_nonempty_values treats NA and empty strings as missing", {
  n <- count_nonempty_values(raw)
  expect_equal(unname(n[c("EIN", "ICO", "ACTIVITY")]), c(3L, 0L, 0L))
  expect_equal(unname(count_nonempty_values(data.table(flag = c(TRUE, NA, FALSE)))["flag"]), 2L)
})

test_that("an output column emptied by a transform is flagged", {
  out <- data.table(
    ein = c("01-0000001", "01-0000002", "01-0000003"),
    org_addr_zip5 = c(NA_character_, NA_character_, NA_character_),  # the ZIP defect shape
    org_addr_street = c("1 MAIN ST", "2 OAK AVE", "PO BOX 3"),
    in_care_of_name_clean = c(NA_character_, NA_character_, NA_character_),  # source ICO was empty
    activity_code = c("", "", "")   # source ACTIVITY was empty
  )
  emptied <- find_emptied_columns(out, count_nonempty_values(raw))
  expect_equal(emptied, "org_addr_zip5")
})

test_that("nothing is flagged when every populated source keeps values", {
  out <- data.table(ein = c("01-0000001", "01-0000002", "01-0000003"),
                    org_addr_zip5 = c("02138", "02139", "02140"))
  expect_equal(find_emptied_columns(out, count_nonempty_values(raw)), character(0))
})

test_that("optional derivations are never flagged even when empty", {
  # Five-digit ZIPs have no ZIP+4; a suffix, parent name, or definition column
  # can be empty for a whole file without anything being wrong.
  out <- data.table(
    ein = c("01-0000001", "01-0000002", "01-0000003"),
    org_addr_zip5 = c("02138", "90210", "02140"),
    org_addr_zip4 = c(NA_character_, NA_character_, NA_character_),
    org_parent_name = c(NA_character_, NA_character_, NA_character_),
    org_legal_suffix = c(NA_character_, NA_character_, NA_character_),
    org_addr_is_po_box = c(FALSE, FALSE, TRUE)
  )
  expect_equal(find_emptied_columns(out, count_nonempty_values(raw)), character(0))
})

test_that("only listed columns are checked", {
  out <- data.table(org_addr_zip5 = c(NA_character_, NA_character_, NA_character_))
  expect_equal(find_emptied_columns(out, count_nonempty_values(raw), preserved = "ein"), character(0))
})

test_that("the check is skipped without source counts", {
  out <- data.table(org_addr_zip5 = c(NA_character_, NA_character_))
  expect_equal(find_emptied_columns(out, NULL), character(0))
})

test_that("generate_quality_report fails on an emptied column and reports it", {
  pre <- list(row_count = 3L, nonempty_counts = count_nonempty_values(raw))
  out <- data.table(
    ein = c("01-0000001", "01-0000002", "01-0000003"),
    org_addr_zip5 = c(NA_character_, NA_character_, NA_character_)
  )
  expect_warning(rep <- generate_quality_report(out, pre_check_results = pre, expected_cols = names(out)),
                 "emptied column")
  expect_false(rep$passed)
  expect_equal(rep$emptied_columns, "org_addr_zip5")
})


# Backlog Z27: a critical field fails the report only above a small share of
# empty rows; the count is always recorded.

test_that("a few empty EINs are counted but do not fail the report", {
  n <- 100000L
  ein <- sprintf("01-%07d", seq_len(n))
  ein[1:5] <- ""                      # 5 in 100,000 = 1 in 20,000, under the 1 in 10,000 limit
  out <- data.table(ein = ein)
  rep <- generate_quality_report(out, pre_check_results = list(row_count = n), expected_cols = "ein")
  expect_true(rep$passed)
  expect_equal(rep$critical_field_missing$ein, 5L)
  expect_equal(length(rep$critical_field_issues), 0)
})

test_that("too many empty EINs fail the report and are reported", {
  out <- data.table(ein = c("01-0000001", "", "01-0000003"))   # 1 in 3
  expect_warning(
    rep <- generate_quality_report(out, pre_check_results = list(row_count = 3L), expected_cols = "ein"),
    "above the"
  )
  expect_false(rep$passed)
  expect_equal(rep$critical_field_missing$ein, 1L)
  expect_equal(rep$critical_field_issues$ein, 1L)
})

test_that("an EIN column with no empties records a zero count", {
  out <- data.table(ein = c("01-0000001", "01-0000002"))
  rep <- generate_quality_report(out, pre_check_results = list(row_count = 2L), expected_cols = "ein")
  expect_true(rep$passed)
  expect_equal(rep$critical_field_missing$ein, 0L)
})
