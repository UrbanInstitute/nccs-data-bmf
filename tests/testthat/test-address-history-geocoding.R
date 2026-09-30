# ADR 0051: distinct addresses, carryover from the geocoded Unified BMF, and
# reading geocoder outputs back, all on small fixtures with no S3.

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
