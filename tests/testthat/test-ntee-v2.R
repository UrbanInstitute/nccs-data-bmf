# ADR 0048 — NTEE-V2 x00 rule. Acceptance criteria A, B, C.
# Run: Rscript -e 'testthat::test_dir("tests/testthat")'
# Sources R/transform_ntee_code.R directly; lookups come from the vendored
# workbook and the vendored legacy crosswalk, not from R/config.R (which
# also configures S3).

library(testthat)
library(data.table)

repo_root <- normalizePath(file.path(testthat::test_path(), "..", ".."))
source(file.path(repo_root, "R", "transform_ntee_code.R"))

lookup_path <- file.path(repo_root, "data", "lookup", "bmf_code_lookup.xlsx")
read_sheet <- function(s) data.table::setDT(openxlsx::read.xlsx(lookup_path, sheet = s))
lk <- list(
  ntee_code             = read_sheet("ntee_code"),
  ntee_code_major_group = read_sheet("ntee_code_major_group"),
  ntee_common_code      = read_sheet("ntee_common_code"),
  nteev2_subsector      = read_sheet("nteev2_subsector")
)
legacy_xw <- data.table::fread(file.path(repo_root, "data", "lookup", "ntee_legacy_5char_lookup.csv"))
legacy_xw[, NTEE := toupper(trimws(NTEE))]
data.table::setkey(legacy_xw, NTEE)

run_transform <- function(codes, legacy_mode = FALSE) {
  dt <- data.table::data.table(ein = sprintf("%02d-%07d", 10, seq_along(codes)), NTEE_CD = codes)
  out <- NULL
  invisible(capture.output(out <- suppressMessages(suppressWarnings(transform_ntee_code(
    dt,
    ntee_code_lookup         = lk$ntee_code,
    ntee_major_group_lookup  = lk$ntee_code_major_group,
    ntee_common_code_lookup  = lk$ntee_common_code,
    nteev2_subsector_lookup  = lk$nteev2_subsector,
    ntee_legacy_5char_lookup = legacy_xw,
    year = "2026", path = NULL, write_scd = FALSE, legacy_mode = legacy_mode
  )))))
  out
}

# ---------------------------------------------------------------------------
# A. Unit table (verbatim from ADR 0048)
# ---------------------------------------------------------------------------
test_that("A: nteev2_code_from_clean applies the x00 rule on the ADR table", {
  tbl <- data.table::fread(text = "
input,expected
B11,B00
B01,B00
B19,B00
B20,B20
B29,B29
B43,B43
A11,A00
INVALID,Z99
UNDEFINED,Z99
Z99,Z99
")
  expect_equal(nteev2_code_from_clean(tbl$input), tbl$expected)
  expect_equal(nteev2_code_from_clean(NA_character_), "Z99")
  expect_equal(nteev2_code_from_clean(character(0)), character(0))
})

test_that("A: the raw code A115 cleans to A11 and renders A00 / MS", {
  out <- run_transform("A115")
  expect_equal(out$ntee_code_clean, "A11")
  expect_equal(out$nteev2_code, "A00")
  expect_equal(out$nteev2_org_type, "MS")
  expect_equal(out$nteev2, "ART-A00-MS")
})

# ---------------------------------------------------------------------------
# B. Oracle: every 3-char code in the vendored legacy crosswalk
# ---------------------------------------------------------------------------
test_that("B: 3-char codes in ntee_legacy_5char_lookup.csv reproduce NTEE2, residuals explained", {
  three <- legacy_xw[nchar(NTEE) == 3]
  expect_gt(nrow(three), 500)
  out <- run_transform(three$NTEE)
  expect_equal(out$ntee_code_raw, three$NTEE)   # row order preserved
  three[, `:=`(got = out$nteev2, clean = out$ntee_code_clean,
               got_code = out$nteev2_code, got_sub = out$nteev2_subsector)]
  three[, exp_sub  := sub("-.*$", "", NTEE2)]
  three[, exp_code := sub("^[A-Z]{3}-([A-Z0-9]{3})-.*$", "\\1", NTEE2)]
  mism <- three[got != NTEE2]

  # Residual class 1 (EXPECTED, not a defect): the crosswalk predates the
  # UNI / HOS subsectors that the public NTEE-V2 spec carves out of EDU / HEL
  # (nccs/_resources/ntee.md). Middle slot and org_type must still agree.
  cls_uni_hos <- mism[got_sub %in% c("UNI", "HOS") & exp_sub %in% c("EDU", "HEL") & got_code == exp_code]
  expect_setequal(cls_uni_hos$NTEE, c(NTEEV2_SUBSECTOR_UNIVERSITY, NTEEV2_SUBSECTOR_HOSPITAL))

  # Residual class 2 (KNOWN GAP, out of scope for ADR 0048): codes present in
  # the NODC crosswalk but absent from data/lookup/bmf_code_lookup.xlsx
  # sheet ntee_code, so they clean to INVALID -> Z99. Listed by name so a
  # lookup update makes this test fail loudly and the list gets shortened.
  cls_missing <- mism[clean == NTEE_INVALID]
  expect_setequal(cls_missing$NTEE,
                  c("B29", "E6A", "F31", "K2A", "K2B", "K2C", "L4A", "L4B", "M99", "P76", "P7A", "P83"))
  expect_false(any(cls_missing$NTEE %in% lk$ntee_code$ntee_code))

  # Nothing else may differ. In particular no residual may involve the x00 rule.
  unexplained <- mism[!NTEE %in% c(cls_uni_hos$NTEE, cls_missing$NTEE)]
  expect_equal(nrow(unexplained), 0,
               info = paste(capture.output(print(unexplained[, .(NTEE, expected = NTEE2, got)])), collapse = "\n"))
  expect_false(any(grepl(NTEEV2_SPECIALTY_PATTERN, three$got_code)))
})

test_that("B: 5-char legacy codes reproduce NTEE2 via the crosswalk path", {
  five <- legacy_xw[nchar(NTEE) == 5]
  out <- run_transform(five$NTEE, legacy_mode = TRUE)
  expect_equal(sum(out$nteev2 != five$NTEE2), 0)
})

# ---------------------------------------------------------------------------
# C. Invariants
# ---------------------------------------------------------------------------
test_that("C: no specialty code ever reaches nteev2_code; composite is consistent", {
  codes <- c(lk$ntee_code$ntee_code, "B0129", "B8443", "S0241", "A115", "B430", "", "??", "Q", "Q1")
  out <- run_transform(codes, legacy_mode = TRUE)
  expect_false(any(grepl(NTEEV2_SPECIALTY_PATTERN, out$nteev2_code)))
  expect_true(all(out$nteev2 == paste(out$nteev2_subsector, out$nteev2_code, out$nteev2_org_type, sep = "-")))
  # org_type != RG iff raw digits 2-3 are a common code (pre-existing rule, pinned)
  common <- substr(out$ntee_code_raw, 2, 3) %in% c("01","02","03","05","11","12","19")
  expect_equal(out$nteev2_org_type != "RG", common)
})

test_that("C: subsector and org_type are unaffected by the x00 rule", {
  codes <- lk$ntee_code$ntee_code
  out <- run_transform(codes)
  # Recompute subsector/org_type the pre-ADR-0048 way (they never depended on nteev2_code)
  first <- substr(codes, 1, 1); d23 <- substr(codes, 2, 3)
  exp_type <- data.table::fcase(d23 == "01","AA", d23 == "02","MT", d23 == "03","PA", d23 == "05","RP",
                                d23 == "11","MS", d23 == "12","MM", d23 == "19","NS", default = "RG")
  expect_equal(out$nteev2_org_type, exp_type)
  expect_true(all(out[ntee_code_clean %in% NTEEV2_SUBSECTOR_UNIVERSITY, nteev2_subsector] == "UNI"))
  expect_true(all(out[ntee_code_clean %in% NTEEV2_SUBSECTOR_HOSPITAL,   nteev2_subsector] == "HOS"))
})

test_that("C: the invariant guard stops the pipeline on a violation", {
  bad <- data.table::data.table(nteev2_code = "B11", nteev2_subsector = "EDU", nteev2_org_type = "MS", nteev2 = "EDU-B11-MS")
  expect_error(.nteev2_invariants(bad), "specialty/common code")
})
