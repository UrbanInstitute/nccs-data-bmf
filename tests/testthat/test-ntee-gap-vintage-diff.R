# scripts/check_ntee_gap_vintage_diff.R: the per-file before/after gate for the
# Z18 reprocess. Each test builds a small BEFORE (pre-fix: gap codes INVALID /
# Z99) and AFTER (post-fix) file with the real transform and the real lookups,
# then breaks one thing.
library(testthat)
library(data.table)

repo_root <- normalizePath(file.path(testthat::test_path(), "..", ".."))
script    <- file.path(repo_root, "scripts", "check_ntee_gap_vintage_diff.R")
source(file.path(repo_root, "R", "transform_ntee_code.R"))
lookup_path <- file.path(repo_root, "data", "lookup", "bmf_code_lookup.xlsx")
read_sheet <- function(s) setDT(openxlsx::read.xlsx(lookup_path, sheet = s))
lk <- list(ntee_code = read_sheet("ntee_code"), ntee_code_major_group = read_sheet("ntee_code_major_group"),
           ntee_common_code = read_sheet("ntee_common_code"), nteev2_subsector = read_sheet("nteev2_subsector"))
legacy_xw <- fread(file.path(repo_root, "data", "lookup", "ntee_legacy_5char_lookup.csv"))
legacy_xw[, NTEE := toupper(trimws(NTEE))]; setkey(legacy_xw, NTEE)

transform <- function(codes, eins, lookup = lk$ntee_code) {
  dt <- data.table(ein = eins, org_name = paste0("ORG ", seq_along(codes)), NTEE_CD = codes)
  out <- NULL
  invisible(capture.output(out <- suppressMessages(suppressWarnings(transform_ntee_code(
    dt, ntee_code_lookup = lookup, ntee_major_group_lookup = lk$ntee_code_major_group,
    ntee_common_code_lookup = lk$ntee_common_code, nteev2_subsector_lookup = lk$nteev2_subsector,
    ntee_legacy_5char_lookup = legacy_xw, year = "0000", path = NULL, write_scd = FALSE)))))
  out[, NTEE_CD := NULL]; out
}
gaps  <- c("B29", "E6A", "F31", "K2A", "K2B", "K2C", "L4A", "L4B", "M99", "P76", "P7A", "P83")
codes <- c(gaps, "A20", "B21", "P20", "X20", "Z99", "B11")
eins  <- sprintf("%02d-%07d", 10, seq_along(codes))
pre_fix_lookup <- lk$ntee_code[!ntee_code %in% gaps]
before <- transform(codes, eins, lookup = pre_fix_lookup)   # what the old files hold
after  <- transform(codes, eins)                            # what the reprocess writes
stopifnot(all(before[ntee_code_raw %in% gaps, nteev2_code] == "Z99"))

run_check <- function(b, a, label = "case") {
  fb <- tempfile(fileext = ".csv"); fa <- tempfile(fileext = ".csv")
  fwrite(b, fb); fwrite(a, fa)
  out <- suppressWarnings(system2("Rscript", c("--vanilla", script, fb, fa, label), stdout = TRUE, stderr = FALSE))
  list(status = attr(out, "status") %||% 0L, line = tail(out, 1))
}
`%||%` <- function(x, y) if (is.null(x)) y else x
field <- function(res, i) strsplit(res$line, "|", fixed = TRUE)[[1]][i]

test_that("a correct reprocess passes, with major-group changes allowed", {
  res <- run_check(before, after)
  expect_equal(res$status, 0L, info = res$line)
  expect_equal(field(res, 6), "12")   # gap rows
  expect_equal(field(res, 7), "12")   # rows changed == gap rows
  expect_true(any(before$ntee_code_major_group != after$ntee_code_major_group))
})

test_that("row order does not matter, including with duplicate EINs", {
  b2 <- rbind(before, before[ntee_code_raw == "B29"][, org_name := "ORG DUP"])
  a2 <- rbind(after,  after[ntee_code_raw == "B29"][, org_name := "ORG DUP"])
  expect_equal(run_check(b2, a2[sample(.N)])$status, 0L)
})

test_that("a missing derived value after the fix fails (NA-aware)", {
  a2 <- copy(after); a2[ntee_code_raw == "B29", nteev2_code := NA_character_]
  res <- run_check(before, a2); expect_equal(res$status, 1L); expect_equal(field(res, 10), "1")
})

test_that("passing a post-fix file as BEFORE fails (precondition)", {
  res <- run_check(after, after); expect_equal(res$status, 1L); expect_equal(field(res, 9), "12")
})

test_that("wrong derived values on a gap row fail", {
  a2 <- copy(after); a2[ntee_code_raw == "B29", `:=`(ntee_code_clean = "WRONG", nteev2_code = "A00")]
  res <- run_check(before, a2); expect_equal(res$status, 1L); expect_equal(field(res, 10), "1")
})

test_that("a change outside the gap rows or outside the allowed columns fails", {
  a2 <- copy(after); a2[ntee_code_raw == "A20", nteev2_code := "A99"]
  res <- run_check(before, a2); expect_equal(res$status, 1L); expect_equal(field(res, 8), "1")
  a3 <- copy(after); a3[1, org_name := "RENAMED"]
  res <- run_check(before, a3); expect_equal(res$status, 1L); expect_equal(field(res, 11), "org_name")
})

test_that("missing required columns fail loudly", {
  res <- run_check(before[, .(ein, org_name)], after[, .(ein, org_name)])
  expect_equal(res$status, 1L); expect_match(res$line, "MISSING-REQUIRED-COLUMNS")
})

test_that("a row-count mismatch fails; fully identical duplicate rows pair fine", {
  expect_equal(run_check(before, after[-1])$status, 1L)
  b2 <- rbind(before, before[ntee_code_raw == "B29"]); a2 <- rbind(after, after[ntee_code_raw == "B29"])
  res <- run_check(b2, a2[sample(.N)]); expect_equal(res$status, 0L); expect_equal(field(res, 6), "13")
})
