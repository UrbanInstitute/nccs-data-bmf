# Deriving the legal type of an organization, formerly NCCS_LEVEL_1 (public
# charity, private foundation, other tax-exempt organization). Each test
# calls derive_nccs_org_legal_type() the way a user would and checks the
# value that comes back.
# Run: Rscript -e 'testthat::test_dir("tests/testthat")'
#
# The comparison with the original LEVEL1 column in the older BMF files is
# too large for a unit test; scripts/check_nccs_org_legal_type_and_field.R
# does it.

library(testthat)

repo_root <- normalizePath(file.path(testthat::test_path(), "..", ".."))
source(file.path(repo_root, "R", "nccs_org_role.R"))
source(file.path(repo_root, "R", "nccs_org_legal_type.R"))

# ============================================================================
# 1. The rule
# ============================================================================

test_that("a 501(c)(3) with foundation code 2, 3 or 4 is a private foundation", {
  expect_identical(
    derive_nccs_org_legal_type(
      subsection_code = c(3, 3, 3),
      foundation_code = c(2, 3, 4)
    ),
    c("PF", "PF", "PF")
  )
})

test_that("a 501(c)(3) with any other foundation code is a public charity", {
  expect_identical(
    derive_nccs_org_legal_type(
      subsection_code = c(3, 3, 3, 3, 3),
      foundation_code = c(1, 5, 10, 15, 17)
    ),
    c("PC", "PC", "PC", "PC", "PC")
  )
})

test_that("any other subsection code is another tax-exempt organization", {
  expect_identical(
    derive_nccs_org_legal_type(
      subsection_code = c(1, 2, 4, 6, 19, 92),
      foundation_code = c(0, 0, 0, 0, 0, 0)
    ),
    c("O", "O", "O", "O", "O", "O")
  )
})

test_that("the foundation code does not matter outside 501(c)(3)", {
  expect_identical(
    derive_nccs_org_legal_type(
      subsection_code = c(4, 4, 4),
      foundation_code = c(4, 15, NA)
    ),
    c("O", "O", "O")
  )
})

# ============================================================================
# 2. Unknown stays unknown
# ============================================================================

test_that("a missing or zero subsection code gives a missing legal type", {
  expect_identical(
    derive_nccs_org_legal_type(
      subsection_code = c(NA, 0, "", "00"),
      foundation_code = c(15, 15, 4, 4)
    ),
    c(NA_character_, NA_character_, NA_character_, NA_character_)
  )
})

test_that("a 501(c)(3) with a missing or zero foundation code is missing, not a public charity", {
  expect_identical(
    derive_nccs_org_legal_type(
      subsection_code = c(3, 3, 3, 3),
      foundation_code = c(NA, 0, "", "00")
    ),
    c(NA_character_, NA_character_, NA_character_, NA_character_)
  )
})

# ============================================================================
# 3. The codes as the files store them
# ============================================================================

test_that("codes stored as text, padded text or decimals are read the same way", {
  expect_identical(
    derive_nccs_org_legal_type(
      subsection_code = c("03", "3", "3.0", 3L),
      foundation_code = c("04", "4", "4.0", 4L)
    ),
    c("PF", "PF", "PF", "PF")
  )
})

test_that("inputs of different lengths stop with an error", {
  expect_error(
    derive_nccs_org_legal_type(subsection_code = c(3, 3), foundation_code = 15),
    "same length"
  )
})

test_that("no organizations in gives no organizations out", {
  expect_identical(
    derive_nccs_org_legal_type(subsection_code = character(0), foundation_code = character(0)),
    character(0)
  )
})

# ============================================================================
# 4. The value in words
# ============================================================================

test_that("each legal type has its label, and anything else has none", {
  expect_identical(
    look_up_nccs_org_label(c("PC", "PF", "O", NA, "U", "pc"), NCCS_ORG_LEGAL_TYPE_LABELS),
    c(
      "Public charity",
      "Private foundation",
      "Other tax-exempt organization",
      NA_character_,
      NA_character_,
      NA_character_
    )
  )
})
