# ADR 0045 helpers: which geocodes get a block, prefix derivations, and the
# county-consistency gate. No spatial work here.

source(here::here("R", "census_geo_resolved.R"))

test_that("only address-level geocodes with coordinates get a block", {
  types <- c("PointAddress", "Subaddress", "StreetAddress", "StreetAddressExt", "StreetInt",
             "Postal", "PostalExt", "PostalLoc", "StreetName", "POI", "Locality", "DistanceMarker", NA)
  lat <- c(rep(40, 12), 40); lon <- c(rep(-75, 12), -75)
  expect_equal(census_geo_is_point_level(types, lat, lon),
               c(TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE))
  expect_false(census_geo_is_point_level("PointAddress", NA, -75))
})

test_that("block GEOID prefixes give tract, county and state", {
  block <- "110010001011000"
  expect_equal(census_geo_tract_from_block(block),  "11001000101")
  expect_equal(census_geo_county_from_block(block), "11001")
  expect_equal(census_geo_state_from_block(block),  "11")
  expect_equal(census_geo_county_from_block(NA_character_), NA_character_)
})

test_that("the county gate counts only comparable rows and reports the mismatch share", {
  blocks   <- c("110010001011000", "240050001011000", NA,      "360610001011000")
  counties <- c("11001",           "24510",           "11001", NA)
  gate <- census_geo_county_gate(blocks, counties)
  expect_equal(gate$n_comparable, 2)
  expect_equal(gate$n_mismatch, 1)
  expect_equal(gate$mismatch_share, 0.5)
  expect_equal(gate$mismatch_rows, 2L)
})
