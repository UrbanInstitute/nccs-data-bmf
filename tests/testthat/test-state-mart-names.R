# Backlog Z19: per-state CSV file name follows the ADR 0039 contract
# (bmf_unified_XX.csv); the old bmf_master_XX.csv name is written as a copy
# only until STATE_MART_OLD_STEM_CUTOVER.

source(here::here("R", "master_state_marts.R"))

test_that("state mart CSV name matches the contract", {
  expect_equal(state_mart_csv_name("NY"), "bmf_unified_NY.csv")
  expect_equal(state_mart_csv_name("ZZ"), "bmf_unified_ZZ.csv")
})

test_that("old file name is available only as an explicit alias", {
  expect_equal(state_mart_csv_name("NY", old = TRUE), "bmf_master_NY.csv")
})

test_that("old file name is written through the cutover and not after", {
  expect_true(state_mart_write_old_stem(as.Date("2026-09-16")))
  expect_true(state_mart_write_old_stem(as.Date(STATE_MART_OLD_STEM_CUTOVER)))
  expect_false(state_mart_write_old_stem(as.Date(STATE_MART_OLD_STEM_CUTOVER) + 1))
})
