# Backlog Z20: the master build must refuse to run when a month folder under
# processed/bmf/ or processed/bmf-legacy/ has no processed CSV.

source(here::here("R", "master_bmf_builder.R"))

cur_keys <- c(
  "processed/bmf/2024_08/bmf_2024_08_processed.csv",
  "processed/bmf/2024_08/bmf_2024_08_data_dictionary.csv",
  "processed/bmf/2024_09/bmf_2024_09_data_dictionary.csv",   # no CSV
  "processed/bmf/2024_09/bmf_2024_09_quality_report.json",
  "processed/bmf/2024_10/bmf_2024_10_processed.csv"
)
cur_folder <- "processed/bmf/(\\d{4}_\\d{2})/"
cur_csv    <- "processed/bmf/(\\d{4}_\\d{2})/bmf_\\d{4}_\\d{2}_processed\\.csv$"

test_that("a month folder without its CSV stops the build", {
  expect_error(
    check_vintage_completeness(cur_keys, cur_folder, cur_csv, "current", allow = FALSE),
    "2024_09"
  )
})

test_that("the override turns the stop into a warning and reports the folders", {
  expect_warning(
    out <- check_vintage_completeness(cur_keys, cur_folder, cur_csv, "current", allow = TRUE),
    "2024_09"
  )
  expect_equal(out, "2024_09")
})

test_that("a complete listing passes silently", {
  ok <- cur_keys[!grepl("2024_09", cur_keys)]
  expect_silent(out <- check_vintage_completeness(ok, cur_folder, cur_csv, "current", allow = FALSE))
  expect_equal(out, character(0))
})

test_that("legacy keys use the legacy file name", {
  keys <- c("processed/bmf-legacy/2018_12/bmf_legacy_2018_12_processed.csv",
            "processed/bmf-legacy/2019_06/bmf_legacy_2019_06_quality_report.json")
  expect_error(
    check_vintage_completeness(
      keys, "processed/bmf-legacy/(\\d{4}_\\d{2})/",
      "processed/bmf-legacy/(\\d{4}_\\d{2})/bmf_legacy_\\d{4}_\\d{2}_processed\\.csv$",
      "legacy", allow = FALSE),
    "2019_06"
  )
})
