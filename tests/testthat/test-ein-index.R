# The EIN index (ADR 0050): shards cut from a small fake Unified BMF must
# carry every row exactly once, in the contracted shape, and be readable
# back as gzip-compressed JSON.

source(here::here("R", "manifest.R"))
source(here::here("R", "build_ein_index.R"))

# A tiny geocoded-Unified-like table with EINs across three prefixes.
build_fake_unified <- function(path) {
  fake_rows <- tibble::tibble(
    ein                      = c("53-0196572", "53-0196573", "12-3456789", "98-7654321", "53-1111111"),
    org_name_display         = c("Urban Institute", "Second Org", "Third Org", "Fourth Org", "Fifth Org"),
    org_addr_city            = c("Washington", "Washington", "Boston", "Seattle", NA),
    org_addr_state           = c("DC", "DC", "MA", "WA", "DC"),
    org_addr_zip5            = c("20024", "20024", "02108", "98101", "20001"),
    ntee_code_clean          = c("T50", "B29", "A69", "Z99", "P72"),
    ntee_code_definition     = c("Philanthropy", "Charter Schools", "Symphony Orchestras", "Unknown", "Half-Way House"),
    nteev2                   = c("PSB-T50-RG", "EDU-B29-RG", "ART-A69-RG", "UNU-Z99-RG", "HMS-P72-RG"),
    ruling_date              = c("1968-01-01", "2001-05-01", "1990-01-01", "1900-01-01", "2010-01-01"),
    subsection_code          = c("03", "03", "03", NA, "03"),
    exempt_organization_type = c("Public charity", "Public charity", "Public charity", NA, "Public charity"),
    status_code_definition   = c("Unconditional exemption", NA, NA, NA, NA),
    first_vintage_ym         = c("1989-06", "2002-01", "1995-08", "2026-07", "2011-06"),
    last_vintage_ym          = c("2026-07", "2026-07", "2026-07", "2026-07", "2026-07"),
    geo_lat                  = c(38.9, 38.9, 42.3, 47.6, 38.9)   # an extra column the index must ignore
  )
  arrow::write_parquet(fake_rows, path)
  fake_rows
}

read_shard <- function(path) {
  connection <- gzfile(path, open = "rb")
  on.exit(close(connection))
  jsonlite::fromJSON(readLines(connection, warn = FALSE), simplifyVector = FALSE)
}

test_that("the shard prefix is the first four digits of the EIN", {
  expect_equal(ein_index_prefix("53-0196572"), "5301")
  expect_equal(ein_index_prefix("530196572"), "5301")
  expect_equal(ein_index_prefix("EIN-53-0196572"), "5301")
})

test_that("every organization lands in exactly one shard, in the contracted shape", {
  work_dir      <- withr::local_tempdir()
  parquet_path  <- file.path(work_dir, "bmf_unified_geocoded.parquet")
  fake_rows     <- build_fake_unified(parquet_path)
  output_dir    <- file.path(work_dir, "ein_index")

  built <- build_ein_index(geocoded_path = parquet_path, output_dir = output_dir,
                           vintage = "2026_09", source_uri = "s3://test/unified.parquet")

  shard_files <- sort(list.files(output_dir, pattern = "^[0-9]{4}\\.json$"))
  expect_equal(shard_files, c("1234.json", "5301.json", "5311.json", "9876.json"))

  shard_530 <- read_shard(file.path(output_dir, "5301.json"))
  expect_equal(shard_530$vintage, "2026_09")
  expect_equal(shard_530$prefix, "5301")
  expect_equal(unlist(shard_530$fields), EIN_INDEX_COLUMNS)
  expect_equal(length(shard_530$records), 2)

  first_record <- shard_530$records[[1]]
  expect_equal(first_record[[1]], "53-0196572")
  expect_equal(first_record[[2]], "Urban Institute")
  expect_equal(first_record[[6]], "T50")
  expect_equal(length(first_record), length(EIN_INDEX_COLUMNS))

  # A missing value is null, not the string "NA"
  shard_531 <- read_shard(file.path(output_dir, "5311.json"))
  expect_null(shard_531$records[[1]][[3]])

  # The manifest lists every shard and the row counts add up
  manifest <- jsonlite::fromJSON(file.path(output_dir, "_manifest.json"), simplifyVector = FALSE)
  expect_equal(manifest$vintage, "2026_09")
  expect_equal(sort(names(manifest$files)), shard_files)
  total_rows <- sum(vapply(manifest$files, function(entry) entry$row_count, numeric(1)))
  expect_equal(total_rows, nrow(fake_rows))
  expect_equal(manifest$inputs[[1]]$uri, "s3://test/unified.parquet")
})

test_that("a malformed or repeated EIN stops the build", {
  work_dir     <- withr::local_tempdir()
  parquet_path <- file.path(work_dir, "bmf_unified_geocoded.parquet")
  fake_rows    <- build_fake_unified(parquet_path)

  bad_rows <- fake_rows
  bad_rows$ein[2] <- "530196573"      # digits only, no hyphen
  arrow::write_parquet(bad_rows, parquet_path)
  expect_error(build_ein_index(parquet_path, output_dir = file.path(work_dir, "a"), vintage = "2026_09"),
               "not in XX-XXXXXXX form")

  dup_rows <- fake_rows
  dup_rows$ein[2] <- dup_rows$ein[1]
  arrow::write_parquet(dup_rows, parquet_path)
  expect_error(build_ein_index(parquet_path, output_dir = file.path(work_dir, "b"), vintage = "2026_09"),
               "appear more than once")
})

test_that("building twice from the same source gives identical shard hashes", {
  work_dir     <- withr::local_tempdir()
  parquet_path <- file.path(work_dir, "bmf_unified_geocoded.parquet")
  build_fake_unified(parquet_path)
  first  <- build_ein_index(parquet_path, output_dir = file.path(work_dir, "first"),  vintage = "2026_09", source_uri = "s3://t")
  Sys.sleep(1.1)
  second <- build_ein_index(parquet_path, output_dir = file.path(work_dir, "second"), vintage = "2026_09", source_uri = "s3://t")
  first_hashes  <- vapply(first$manifest$files,  function(f) f$sha256, character(1))
  second_hashes <- vapply(second$manifest$files, function(f) f$sha256, character(1))
  expect_equal(first_hashes, second_hashes)
})

test_that("a dry-run publish plans every shard for the vintage folder and the mirror", {
  work_dir     <- withr::local_tempdir()
  parquet_path <- file.path(work_dir, "bmf_unified_geocoded.parquet")
  build_fake_unified(parquet_path)
  output_dir   <- file.path(work_dir, "ein_index")
  build_ein_index(geocoded_path = parquet_path, output_dir = output_dir,
                  vintage = "2026_09", source_uri = "s3://test/unified.parquet")

  result <- publish_ein_index(output_dir = output_dir, s3_root = "test/ein-index/",
                              bucket = "none", dry_run = TRUE)
  expect_equal(result$vintage_prefix, "test/ein-index/v2026_09/")
  expect_equal(result$latest_prefix,  "test/ein-index/latest/")
  expect_equal(unname(result$uploaded), c(4, 4))
  expect_equal(unname(result$skipped),  c(0, 0))
})
