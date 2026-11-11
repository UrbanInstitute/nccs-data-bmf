# Deriving the role of an organization, formerly NCCS_LEVEL_2 (operating, supporting, mutual
# benefit). Each test calls derive_nccs_org_role() the way a user would and
# checks the letter that comes back.
# Run: Rscript -e 'testthat::test_dir("tests/testthat")'
#
# The comparison with the original LEVEL2 column in the older BMF files is
# too large for a unit test; scripts/check_nccs_org_role.R does it.

library(testthat)

repo_root <- normalizePath(file.path(testthat::test_path(), "..", ".."))
source(file.path(repo_root, "R", "nccs_org_role.R"))

# ============================================================================
# 1. NTEE code rules (the definition used through 2016)
# ============================================================================

test_that("common codes 11 and 12 are supporting in any major group", {
  expect_identical(
    derive_nccs_org_role(c("B11", "E112", "T12", "X12"), definition = "through_2016"),
    c("S", "S", "S", "S")
  )
})

test_that("the common code rule wins over the mutual benefit rule", {
  expect_identical(
    derive_nccs_org_role(c("Y115", "Y12"), definition = "through_2016"),
    c("S", "S")
  )
})

test_that("codes that start with Y are mutual benefit", {
  expect_identical(
    derive_nccs_org_role(c("Y42", "Y20", "Y032"), definition = "through_2016"),
    c("M", "M", "M")
  )
})

test_that("grantmaking T codes are supporting", {
  expect_identical(
    derive_nccs_org_role(c("T20", "T22", "T30", "T31", "T70", "T90", "T99"), definition = "through_2016"),
    c("S", "S", "S", "S", "S", "S", "S")
  )
})

test_that("T0x, T19, T40 and T50 stay operating, with or without a fourth character", {
  expect_identical(
    derive_nccs_org_role(c("T01", "T033", "T05", "T19", "T193", "T40", "T50"), definition = "through_2016"),
    c("O", "O", "O", "O", "O", "O", "O")
  )
})

test_that("every other code is operating", {
  expect_identical(
    derive_nccs_org_role(c("P20", "B82", "X20", "A01", "Z99"), definition = "through_2016"),
    c("O", "O", "O", "O", "O")
  )
})

# ============================================================================
# 2. How codes are read
# ============================================================================

test_that("case and surrounding spaces do not change the answer", {
  expect_identical(
    derive_nccs_org_role(c(" b11 ", "y42", "t22\t"), definition = "through_2016"),
    c("S", "M", "S")
  )
})

test_that("five-character codes are read by their second and third characters", {
  expect_identical(
    derive_nccs_org_role(c("B1170", "S0241"), definition = "through_2016"),
    c("S", "O")
  )
})

test_that("a missing or blank code is unknown, never operating", {
  expect_identical(
    derive_nccs_org_role(c(NA, "", "   ", "\t"), definition = "through_2016"),
    c(NA_character_, NA_character_, NA_character_, NA_character_)
  )
})

test_that("a value that is not an NTEE code is unknown, never operating", {
  expect_identical(
    derive_nccs_org_role(c("123", "???", "UNDEFINED", "INVALID", "B 11"), definition = "through_2016"),
    c(NA_character_, NA_character_, NA_character_, NA_character_, NA_character_)
  )
})

test_that("short words and missing-value words are unknown, never a role", {
  expect_identical(
    derive_nccs_org_role(
      c("NA", "NULL", "YUCK", "TURT", "T", "Y", "BO2", "EJ21", "DISP", "000", "B11700"),
      definition = "through_2016"
    ),
    rep(NA_character_, times = 11)
  )
})

test_that("punctuation after an NTEE code is ignored (ADR 0058)", {
  expect_identical(
    derive_nccs_org_role(c("B12,", "P20.", "Y42;", "T22,,"), definition = "through_2016"),
    c("S", "O", "M", "S")
  )
})

test_that("punctuation inside or before an NTEE code still makes it unknown", {
  expect_identical(
    derive_nccs_org_role(c("B,12", ",B12", "B 12", ","), definition = "through_2016"),
    rep(NA_character_, times = 4)
  )
})

test_that("every shape of NTEE code found in the BMF files is read as a code", {
  expect_identical(
    derive_nccs_org_role(
      c("X2", "A69", "A6C", "D203", "A69Z", "A6E0", "A6CZ", "S0241", "G039B"),
      definition = "through_2016"
    ),
    rep("O", times = 9)
  )
})

test_that("an empty input returns an empty result", {
  expect_identical(
    derive_nccs_org_role(character(0), definition = "through_2016"),
    character(0)
  )
})

# ============================================================================
# 3. Foundation code rule (the definition used from 2017 on)
# ============================================================================

test_that("a 501(c)(3) with foundation code 4 or 17 is supporting whatever its NTEE code", {
  expect_identical(
    derive_nccs_org_role(
      ntee_code = c("P20", "P20", "Y42", NA),
      subsection_code = c(3, 3, 3, 3),
      foundation_code = c(4, 17, 4, 17)
    ),
    c("S", "S", "S", "S")
  )
})

test_that("other foundation codes leave the NTEE code rules in charge", {
  expect_identical(
    derive_nccs_org_role(
      ntee_code = c("P20", "B11", "Y42", NA, "P20", "P20"),
      subsection_code = c(3, 3, 3, 3, 3, 3),
      foundation_code = c(15, 15, 16, 15, 3, NA)
    ),
    c("O", "S", "M", NA, "O", "O")
  )
})

test_that("foundation codes 4 and 17 do not count outside 501(c)(3)", {
  expect_identical(
    derive_nccs_org_role(
      ntee_code = c("P20", "P20", "P20"),
      subsection_code = c(4, 92, NA),
      foundation_code = c(4, 17, 4)
    ),
    c("O", "O", "O")
  )
})

test_that("subsection and foundation codes are read the same however they are stored", {
  expect_identical(
    derive_nccs_org_role(
      ntee_code = c("P20", "P20", "P20", "P20"),
      subsection_code = c("03", "3", "3.0", " 3 "),
      foundation_code = c("04", "17", "4.0", "17")
    ),
    c("S", "S", "S", "S")
  )
})

test_that("a subsection or foundation code that is not a whole number is never a match", {
  expect_identical(
    derive_nccs_org_role(
      ntee_code = c("P20", "P20", "P20", "P20", "P20", "P20"),
      subsection_code = c(3.9, 3, "3.5", 3, 2.9, 3),
      foundation_code = c(4, 4.9, 4, "17.2", 4, 16.9)
    ),
    rep("O", times = 6)
  )
})

test_that("the through_2016 definition ignores foundation codes", {
  expect_identical(
    derive_nccs_org_role(
      ntee_code = c("P20", "P20"),
      subsection_code = c(3, 3),
      foundation_code = c(4, 17),
      definition = "through_2016"
    ),
    c("O", "O")
  )
})

test_that("the default definition is 2017_onward", {
  expect_identical(
    derive_nccs_org_role(ntee_code = "P20", subsection_code = 3, foundation_code = 4),
    "S"
  )
})

# ============================================================================
# 4. Wrong inputs stop with a clear message
# ============================================================================

test_that("2017_onward without subsection or foundation codes is an error", {
  expect_error(derive_nccs_org_role("P20"), "needs subsection_code and foundation_code")
  expect_error(derive_nccs_org_role("P20", subsection_code = 3), "needs subsection_code and foundation_code")
  expect_error(derive_nccs_org_role("P20", foundation_code = 4), "needs subsection_code and foundation_code")
})

test_that("codes of a different length than ntee_code are an error", {
  expect_error(
    derive_nccs_org_role(c("P20", "B11"), subsection_code = 3, foundation_code = c(4, 4)),
    "same length"
  )
  expect_error(
    derive_nccs_org_role(c("P20", "B11"), subsection_code = c(3, 3), foundation_code = 4),
    "same length"
  )
})

test_that("an unknown definition is an error", {
  expect_error(derive_nccs_org_role("P20", definition = "2019"))
})

# ============================================================================
# 5. Labels
# ============================================================================

test_that("letters become readable labels and anything else becomes NA", {
  expect_identical(
    look_up_nccs_org_label(c("O", "S", "M", "X", NA), NCCS_ORG_ROLE_LABELS),
    c("Operating", "Supporting", "Mutual benefit", NA, NA)
  )
})

test_that("a definition that is not one of the two stops with an error", {
  expect_error(
    derive_nccs_org_role(
      ntee_code = "B11",
      subsection_code = 3,
      foundation_code = 15,
      definition = "2018"
    ),
    "must be one of"
  )
})
