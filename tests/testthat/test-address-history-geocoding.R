# ADR 0051: distinct addresses, carryover from the geocoded Unified BMF, and
# reading geocoder outputs back, all on small fixtures with no S3.

source(here::here("R", "utils", "logging.R"))
source(here::here("R", "address.R"))                     # .build_full_address()
source(here::here("R", "address_normalize.R"))
source(here::here("R", "quality", "geocoding_checks.R")) # GEOCODER_COLUMN_MAP
source(here::here("R", "address_history_geocoding.R"))

address_spells_fixture <- tibble::tibble(
  spell_rank = c(0L, 1L, 0L, 1L, 2L),
  street     = c("500 L'ENFANT PLAZA SW", "2100 M ST NW", "500 L'ENFANT PLAZA SW", NA, "1 MAIN ST"),
  city       = c("WASHINGTON", "WASHINGTON", "WASHINGTON", "BOSTON", "SPRINGFIELD"),
  state      = c("DC", "DC", "DC", "MA", "MA"),
  zip5       = c("20024", "20037", "20024", "02108", "01103")
)

test_that("distinct addresses drop street-less spells and repeat addresses, and carry a one-line form", {
  distinct_addresses <- address_history_distinct_addresses(address_spells_fixture)

  expect_equal(nrow(distinct_addresses), 3)
  expect_false(any(is.na(distinct_addresses$street)))
  expect_true("500 L'ENFANT PLAZA SW, WASHINGTON, DC 20024" %in% distinct_addresses$f_address)
})

test_that("carryover matches the Unified BMF on the normalized address, geocoded row first", {
  geocoded_unified <- tibble::tibble(
    org_addr_street_raw = c("500 L'Enfant Plaza SW ", "500 L'ENFANT PLAZA SW", "9 Elm Ave"),
    org_addr_city_raw   = c("Washington", "WASHINGTON", "Albany"),
    org_addr_state_raw  = c("DC", "DC", "NY"),
    org_addr_zip_raw    = c("20024-2100", "20024", "12207"),
    geo_lat             = c(NA, 38.88, 42.65),
    geo_lon             = c(NA, -77.02, -73.75),
    geo_addr_type       = c(NA, "PointAddress", "PointAddress"),
    geo_score           = c(NA, "100", "98")
  )

  prior <- address_history_prior_geocodes(geocoded_unified)
  expect_equal(nrow(prior), 2)                       # two distinct addresses
  expect_equal(prior$geo_lat[prior$zip5 == "20024"], 38.88)   # the geocoded duplicate wins

  split <- address_history_split_carryover(address_history_distinct_addresses(address_spells_fixture), prior)
  expect_equal(nrow(split$carryover), 1)
  expect_equal(split$carryover$street, "500 L'ENFANT PLAZA SW")
  expect_equal(sort(split$delta$street), c("1 MAIN ST", "2100 M ST NW"))
})

test_that("the cache from an earlier round counts as attempted and wins over the Unified BMF", {
  distinct_addresses <- address_history_distinct_addresses(address_spells_fixture)
  prior  <- tibble::tibble(street = "500 L'ENFANT PLAZA SW", city = "WASHINGTON", state = "DC", zip5 = "20024",
                           geo_lat = 38.88, geo_lon = -77.02)
  cached <- tibble::tibble(street = c("500 L'ENFANT PLAZA SW", "1 MAIN ST"), city = c("WASHINGTON", "SPRINGFIELD"),
                           state = c("DC", "MA"), zip5 = c("20024", "01103"),
                           geo_lat = c(38.89, NA), geo_lon = c(-77.03, NA))   # 1 MAIN ST: attempted, not matched

  split <- address_history_split_carryover(distinct_addresses, prior, cached)

  expect_equal(nrow(split$carryover), 2)
  expect_equal(split$carryover$geo_lat[split$carryover$street == "500 L'ENFANT PLAZA SW"], 38.89)
  expect_equal(split$delta$street, "2100 M ST NW")
})

test_that("an incomplete run stops the build", {
  run_dir   <- withr::local_tempdir()
  input_dir <- file.path(run_dir, "input")
  dir.create(input_dir, recursive = TRUE)

  jsonlite::write_json(list(run_id = "addrhist_test", num_batches = 1L,
                            batches = list(list(batch_number = 1L, filename = "address_history_geocoder_batch_01.csv"))),
                       file.path(input_dir, "bmf_master_geocoder_manifest.json"), auto_unbox = TRUE)
  data.table::fwrite(data.frame(f_address = c("1 MAIN ST, SPRINGFIELD, MA 01103", "2 MAIN ST, SPRINGFIELD, MA 01103")),
                     file.path(input_dir, "address_history_geocoder_batch_01.csv"))
  ledger <- data.frame(batch_id = "addrhist_test_01", service_stem = "thiya-1-addrhist",
                       batch_file = "address_history_geocoder_batch_01.csv",
                       output_file = "address_history_geocoder_batch_01_geocoded.csv",
                       n_addresses = "2", submitted_at = "", output_seen_at = "", downloaded_at = "", status = "submitted")
  data.table::fwrite(ledger, file.path(run_dir, "geocode_ledger.tsv"), sep = "\t")

  geocodes <- tibble::tibble(f_address = "1 MAIN ST, SPRINGFIELD, MA 01103", geo_lat = 42.1, geo_lon = -72.6)

  # A batch still in flight stops the build.
  expect_error(address_history_stop_unless_complete(run_dir, geocodes), "not retrieved")

  # Every batch retrieved, but one submitted address has no output row: stop.
  ledger$status <- "retrieved"
  data.table::fwrite(ledger, file.path(run_dir, "geocode_ledger.tsv"), sep = "\t")
  expect_error(address_history_stop_unless_complete(run_dir, geocodes), "no row in the geocoder outputs")

  # Both addresses present (one unmatched, with empty coordinates): complete.
  geocodes_complete <- tibble::add_row(geocodes, f_address = "2 MAIN ST, SPRINGFIELD, MA 01103", geo_lat = NA, geo_lon = NA)
  expect_equal(address_history_stop_unless_complete(run_dir, geocodes_complete)$submitted, 2L)
})

test_that("the cache keeps unmatched addresses and uploads through the given function", {
  run_dir <- withr::local_tempdir()
  uploads <- character()
  fake_uploader <- function(local_file, s3_key, bucket) { uploads <<- c(uploads, s3_key); TRUE }

  address_geocodes <- tibble::tibble(street = c("1 MAIN ST", "2 MAIN ST"), city = "SPRINGFIELD", state = "MA",
                                     zip5 = "01103", geo_lat = c(42.1, NA), geo_lon = c(-72.6, NA))
  address_history_write_cache(address_geocodes, geocoding_dir = run_dir, bucket = "test", uploader = fake_uploader)

  cache <- arrow::read_parquet(file.path(run_dir, ADDRESS_HISTORY_CACHE_FILE))
  expect_equal(nrow(cache), 2)
  expect_equal(uploads, ADDRESS_HISTORY_CACHE_KEY)
})

test_that("geocoder outputs are read back with renamed columns, one row per address", {
  run_dir    <- withr::local_tempdir()
  output_dir <- file.path(run_dir, "output")
  dir.create(output_dir, recursive = TRUE)

  data.table::fwrite(
    data.frame(f_address = c("1 MAIN ST, SPRINGFIELD, MA 01103", "1 MAIN ST, SPRINGFIELD, MA 01103"),
               Latitude = c("", "42.10"), Longitude = c("", "-72.59"),
               Addr_type = c("", "StreetAddress"), Score = c("", "95")),
    file.path(output_dir, "address_history_geocoder_batch_01_geocoded.csv"))

  geocodes <- read_address_history_geocodes(run_dir)

  expect_equal(nrow(geocodes), 1)
  expect_equal(geocodes$geo_lat, 42.10)
  expect_equal(geocodes$geo_addr_type, "StreetAddress")
  expect_true(is.numeric(geocodes$geo_score))
})
