# Deriving the field of an organization, formerly NCCS_LEVEL_3 (17 groups).
# Each test calls derive_nccs_org_field() the way a user would and checks
# the two-letter value that comes back.
# Run: Rscript -e 'testthat::test_dir("tests/testthat")'
#
# The comparison with the original LEVEL3 column in the older BMF files is
# too large for a unit test; scripts/check_nccs_org_legal_type_and_field.R
# does it.

library(testthat)

repo_root <- normalizePath(file.path(testthat::test_path(), "..", ".."))
source(file.path(repo_root, "R", "nccs_org_role.R"))
source(file.path(repo_root, "R", "nccs_org_field.R"))

# ============================================================================
# 1. Supporting organizations, by common code
# ============================================================================

test_that("common code 11 is single organization support in any major group", {
  expect_identical(
    derive_nccs_org_field(c("B11", "E112", "T11", "Y11")),
    c("ZA", "ZA", "ZA", "ZA")
  )
})

test_that("common code 12 is fundraising within one field in any major group", {
  expect_identical(
    derive_nccs_org_field(c("A12", "P125", "T12", "Y12")),
    c("ZB", "ZB", "ZB", "ZB")
  )
})

# ============================================================================
# 2. Grantmaking codes in major group T
# ============================================================================

test_that("T2, T3, T7 and T9 codes each have their own group", {
  expect_identical(
    derive_nccs_org_field(c("T20", "T23", "T30", "T31", "T70", "T90", "T99")),
    c("ZC", "ZC", "ZD", "ZD", "ZE", "ZF", "ZF")
  )
})

test_that("every other T code is public, societal benefit", {
  expect_identical(
    derive_nccs_org_field(c("T01", "T032", "T19", "T40", "T50", "T60", "T80")),
    c("PB", "PB", "PB", "PB", "PB", "PB", "PB")
  )
})

# ============================================================================
# 3. Mutual benefit codes in major group Y
# ============================================================================

test_that("Y30 is pension and retirement funds, with or without a suffix", {
  expect_identical(
    derive_nccs_org_field(c("Y30", "Y301")),
    c("MR", "MR")
  )
})

test_that("every other Y code is other mutual benefit, including the other Y3 codes", {
  expect_identical(
    derive_nccs_org_field(c("Y20", "Y33", "Y34", "Y35", "Y42", "Y99")),
    c("MO", "MO", "MO", "MO", "MO", "MO")
  )
})

# ============================================================================
# 4. Every other code, by its first letter
# ============================================================================

test_that("each major group letter falls in its field", {
  major_group_letters <- c(
    "A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M",
    "N", "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z"
  )

  expected_fields <- c(
    "AR", "ED", "EN", "EN", "HE", "HE", "HE", "HE", "HS", "HS", "HS", "HS", "HS",
    "HS", "HS", "HS", "IN", "PB", "PB", "PB", "PB", "PB", "PB", "RE", "MO", "UN"
  )

  expect_identical(
    derive_nccs_org_field(paste0(major_group_letters, "40")),
    expected_fields
  )
})

# ============================================================================
# 5. Codes as the files store them, and codes that cannot be used
# ============================================================================

test_that("lower case, spaces and punctuation after the code do not change the field", {
  expect_identical(
    derive_nccs_org_field(c("b11", " P20 ", "B12,", "t22.")),
    c("ZA", "HS", "ZB", "ZC")
  )
})

test_that("a missing, blank or misshapen code gives a missing field", {
  expect_identical(
    derive_nccs_org_field(c(NA, "", "NA", "DISP", "12", "B")),
    c(NA_character_, NA_character_, NA_character_, NA_character_, NA_character_, NA_character_)
  )
})

test_that("no organizations in gives no organizations out", {
  expect_identical(derive_nccs_org_field(character(0)), character(0))
})

# ============================================================================
# 6. The value in words
# ============================================================================

test_that("each of the 17 fields has its label, and anything else has none", {
  expect_identical(
    look_up_nccs_org_label(
      c(
        "AR", "ED", "EN", "HE", "HS", "IN", "PB", "RE", "UN",
        "MO", "MR", "ZA", "ZB", "ZC", "ZD", "ZE", "ZF", NA, "XX"
      ),
      NCCS_ORG_FIELD_LABELS
    ),
    c(
      "Arts, culture and humanities",
      "Education",
      "Environment and animals",
      "Health",
      "Human services",
      "International, foreign affairs",
      "Public, societal benefit",
      "Religion related",
      "Unknown, unclassified",
      "Other mutual benefit",
      "Pension and retirement funds",
      "Single organization support",
      "Fundraising within one field",
      "Private grantmaking foundations",
      "Public foundations",
      "General fundraising",
      "Other supporting public benefit",
      NA_character_,
      NA_character_
    )
  )
})
