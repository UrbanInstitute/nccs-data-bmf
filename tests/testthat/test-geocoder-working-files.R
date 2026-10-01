# ADR 0037 changeover: working files of a geocoding run are written under the
# new names, and a run from before the rename can still be read.

source(here::here("R", "geocoder_working_files.R"))

test_that("a working file is found under the new name, or the old name when only that exists", {
  run_dir <- withr::local_tempdir()

  # Neither exists: the new name is returned, so the caller's error names it.
  expect_equal(
    basename(geocoder_working_file_path(run_dir, "bmf_unified_geocoder_manifest.json")),
    "bmf_unified_geocoder_manifest.json"
  )

  # Only the old name exists (a run staged before the rename): it is used.
  file.create(file.path(run_dir, "bmf_master_geocoder_manifest.json"))
  expect_message(
    legacy_path <- geocoder_working_file_path(run_dir, "bmf_unified_geocoder_manifest.json"),
    "before the rename"
  )
  expect_equal(basename(legacy_path), "bmf_master_geocoder_manifest.json")

  # Both exist: the new name wins.
  file.create(file.path(run_dir, "bmf_unified_geocoder_manifest.json"))
  expect_equal(
    basename(geocoder_working_file_path(run_dir, "bmf_unified_geocoder_manifest.json")),
    "bmf_unified_geocoder_manifest.json"
  )
})

test_that("batch outputs are listed under either naming, but never both at once", {
  old_run <- withr::local_tempdir()
  file.create(file.path(old_run, c("bmf_master_geocoder_batch_00_geocoded.csv",
                                   "bmf_master_geocoder_batch_01_geocoded.csv",
                                   "bmf_master_geocoder_batch_01.csv",        # an input, not an output
                                   "notes.txt")))
  expect_equal(
    sort(basename(geocoder_batch_output_files(old_run))),
    c("bmf_master_geocoder_batch_00_geocoded.csv", "bmf_master_geocoder_batch_01_geocoded.csv")
  )

  new_run <- withr::local_tempdir()
  file.create(file.path(new_run, "bmf_unified_geocoder_batch_01_geocoded.csv"))
  expect_equal(basename(geocoder_batch_output_files(new_run)), "bmf_unified_geocoder_batch_01_geocoded.csv")

  # Outputs of two runs side by side would be merged together: stop.
  file.create(file.path(new_run, "bmf_master_geocoder_batch_01_geocoded.csv"))
  expect_error(geocoder_batch_output_files(new_run), "two different runs")

  expect_length(geocoder_batch_output_files(withr::local_tempdir()), 0)
})

test_that("the old runner file still exists and passes the run on to the new runner", {
  shim_text <- readr::read_file(here::here("R", "run_master_geocoding.R"))
  expect_match(shim_text, 'source(here::here("R", "run_unified_geocoding.R"))', fixed = TRUE)
  expect_match(shim_text, "warning(", fixed = TRUE)

  runner_text <- readr::read_file(here::here("R", "run_unified_geocoding.R"))
  expect_match(runner_text, "UNIFIED_GEOCODING_MODE <- MASTER_GEOCODING_MODE", fixed = TRUE)
})
