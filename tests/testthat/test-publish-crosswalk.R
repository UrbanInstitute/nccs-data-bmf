# The shared crosswalk publisher must stop before writing the manifest when
# any upload fails (review of nccs-data-bmf #61), and must upload every file
# and then the manifest when all succeed.

# config.R is not sourced: it loads the lookup workbook. The publisher only
# needs the bucket name and the manifest helpers.
BMF_S3_BUCKET <- "none"
source(here::here("R", "manifest.R"))
source(here::here("R", "publish_crosswalk.R"))

build_fake_crosswalk <- function(dir) {
  rows <- data.frame(ein = c("01-0000001", "01-0000002"), value = c("a", "b"))
  parquet_path <- file.path(dir, "fake_crosswalk.parquet")
  arrow::write_parquet(rows, parquet_path)
  data.table::fwrite(rows, sub("\\.parquet$", ".csv", parquet_path))
  parquet_path
}

test_that("a failed upload stops the publish before the manifest", {
  parquet_path <- build_fake_crosswalk(withr::local_tempdir())
  attempted <- character()
  failing_uploader <- function(local_file, s3_key, bucket) {
    attempted <<- c(attempted, s3_key)
    !grepl("\\.csv$", s3_key)          # the CSV upload fails
  }
  # read_existing_manifest() is not reached: it needs S3, so stub it too
  local_mocked_bindings(read_existing_manifest = function(s3_key, bucket) NULL, .env = globalenv())

  expect_error(
    publish_crosswalk(parquet_path, s3_prefix = "test/fake/", vintage = "2026_09",
                      bucket = "none", uploader = failing_uploader),
    "upload failed for s3://none/test/fake/fake_crosswalk.csv"
  )
  expect_false(any(grepl("_manifest\\.json$", attempted)))
})

test_that("a successful publish uploads every file and then the manifest", {
  parquet_path <- build_fake_crosswalk(withr::local_tempdir())
  written <- character()
  recording_uploader <- function(local_file, s3_key, bucket) { written <<- c(written, s3_key); TRUE }
  local_mocked_bindings(read_existing_manifest = function(s3_key, bucket) NULL, .env = globalenv())

  result <- publish_crosswalk(parquet_path, s3_prefix = "test/fake/", vintage = "2026_09",
                              bucket = "none", uploader = recording_uploader)
  expect_equal(written, c("test/fake/fake_crosswalk.parquet", "test/fake/fake_crosswalk.csv", "test/fake/_manifest.json"))
  expect_equal(sort(result$uploaded), c("fake_crosswalk.csv", "fake_crosswalk.parquet"))
})
