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
test_that("B: 3-char oracle, two populations exactly as amended criterion B defines", {
  three <- legacy_xw[nchar(NTEE) == 3]
  expect_gt(nrow(three), 500)
  out <- run_transform(three$NTEE)
  expect_equal(out$ntee_code_raw, three$NTEE)   # row order preserved
  three[, `:=`(got_code = out$nteev2_code, got_type = out$nteev2_org_type,
               got_sub = out$nteev2_subsector, clean = out$ntee_code_clean)]
  three[, exp_sub  := sub("-.*$", "", NTEE2)]
  three[, exp_code := sub("^[A-Z]{3}-([A-Z0-9]{3})-.*$", "\\1", NTEE2)]
  three[, exp_type := sub("^.*-", "", NTEE2)]

  # Population 2: the 12 named lookup gaps (BACKLOG Z18). This pinned list may
  # only SHRINK: a lookup update makes the expectation here fail loudly.
  gaps <- c("B29", "E6A", "F31", "K2A", "K2B", "K2C", "L4A", "L4B", "M99", "P76", "P7A", "P83")
  pop2 <- three[NTEE %in% gaps]
  expect_setequal(pop2$NTEE, gaps)
  expect_true(all(pop2$clean == NTEE_INVALID))
  expect_true(all(pop2$got_code == "Z99" & pop2$got_sub == "UNU" & pop2$got_type == "RG"))
  expect_false(any(gaps %in% lk$ntee_code$ntee_code))

  # Population 1: every remaining row is lookup-valid; middle slot and
  # org-type must match the crosswalk EXACTLY. Expected mismatches: 0.
  pop1 <- three[!NTEE %in% gaps]
  expect_equal(pop1[got_code != exp_code, .N], 0,
               info = paste(capture.output(print(pop1[got_code != exp_code])), collapse = "\n"))
  expect_equal(pop1[got_type != exp_type, .N], 0)

  # Subsector: exact except the UNI/HOS carve-out our spec mandates.
  carve <- c(NTEEV2_SUBSECTOR_UNIVERSITY, NTEEV2_SUBSECTOR_HOSPITAL)
  expect_true(all(pop1[NTEE %in% NTEEV2_SUBSECTOR_UNIVERSITY, got_sub] == "UNI"))
  expect_true(all(pop1[NTEE %in% NTEEV2_SUBSECTOR_HOSPITAL,   got_sub] == "HOS"))
  expect_equal(pop1[!NTEE %in% carve & got_sub != exp_sub, .N], 0)

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

test_that("C: subsector/org_type DERIVATIONS are unchanged (byte identity of artifacts is a criterion-D obligation, not proven here)", {
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

# ---------------------------------------------------------------------------
# Round-1 review additions (response matrix N-5, B-4)
# ---------------------------------------------------------------------------
test_that("N5: every value 01-19 collapses; 00 and 20 are boundaries; helper is idempotent", {
  digits <- sprintf("%02d", 1:19)
  expect_equal(nteev2_code_from_clean(paste0("B", digits)), rep("B00", 19))
  expect_equal(nteev2_code_from_clean(c("B00", "B20", "B99")), c("B00", "B20", "B99"))
  v <- c("B11", "B00", "B20", "Z99", "INVALID", NA)
  once <- nteev2_code_from_clean(v)
  expect_equal(nteev2_code_from_clean(once), once)     # idempotent
  expect_length(once, length(v))                       # length preserved
})

test_that("N5: NA, empty, malformed, lowercase, whitespace through the FULL transform", {
  out <- run_transform(c(NA, "", "??", "9X", "b11", " B11 ", "B1"))
  expect_equal(out$nteev2[1:4], rep("UNU-Z99-RG", 4))
  expect_equal(out$nteev2[5:6], rep("EDU-B00-MS", 2))  # raw prep upcases/trims
  # "B1" pads to "B10", which is NOT an NTEE-CC lookup code, so it cleans to
  # INVALID -> Z99 (lookup validation runs BEFORE the x00 rule; the helper
  # never sees it). Pinned so this ordering cannot silently change.
  expect_equal(out$nteev2_code[7], "Z99")
  expect_false(any(grepl(NTEEV2_SPECIALTY_PATTERN, out$nteev2_code)))
})

test_that("N5: unmatched 5-char legacy fallback routes positions 4-5 through the x00 rule", {
  # A0004 is not in the vendored crosswalk; positions 4-5 = "04" (in-range,
  # unused by NTEE-CC) must still collapse to A00 per the spec's x01-x19 rule.
  out <- run_transform(c("A0004", "S0241"), legacy_mode = TRUE)
  expect_equal(out$nteev2_code, c("A00", "S41"))
})

test_that("B4: the invariant guard is NA-safe and catches literal-NA composites", {
  na_row <- data.table::data.table(nteev2_code = NA_character_, nteev2_subsector = "EDU",
                                   nteev2_org_type = "RG", nteev2 = NA_character_)
  expect_error(.nteev2_invariants(na_row), "NA in nteev2 components")
  lit <- data.table::data.table(nteev2_code = "B20", nteev2_subsector = "EDU",
                                nteev2_org_type = "RG", nteev2 = "EDU-NA-RG")
  expect_error(.nteev2_invariants(lit))
})

test_that("B3: .ntee_output_validation re-checks the specialty pattern on the SCD", {
  scd <- data.table::data.table(ntee_code_clean = "B11", nteev2_code = "B11")
  expect_error(.ntee_output_validation(scd), "SCD output")
})
