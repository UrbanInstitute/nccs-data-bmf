# ADR 0051: the stable spell identifier shared by the address-resolved and
# address-geo-resolved crosswalks.

source(here::here("R", "address_spell_id.R"))

test_that("spell_id is 16 hexadecimal characters and deterministic", {
  first  <- address_spell_id("EIN-52-0880375", "500 L'ENFANT PLAZA SW", "WASHINGTON", "DC", "20024")
  second <- address_spell_id("EIN-52-0880375", "500 L'ENFANT PLAZA SW", "WASHINGTON", "DC", "20024")

  expect_equal(first, second)
  expect_match(first, "^[0-9a-f]{16}$")
})

test_that("spell_id matches the documented definition", {
  # First 16 hex characters of SHA-256 of the five fields joined with "|".
  expected <- substr(as.character(openssl::sha256("EIN-52-0880375|500 L'ENFANT PLAZA SW|WASHINGTON|DC|20024")), 1, 16)

  expect_equal(
    address_spell_id("EIN-52-0880375", "500 L'ENFANT PLAZA SW", "WASHINGTON", "DC", "20024"),
    expected
  )
})

test_that("a missing field hashes as an empty string, so pre-2009 spells without a street get an id", {
  with_missing_street <- address_spell_id("EIN-52-0880375", NA, "WASHINGTON", "DC", "20024")
  with_empty_street   <- substr(as.character(openssl::sha256("EIN-52-0880375||WASHINGTON|DC|20024")), 1, 16)

  expect_equal(with_missing_street, with_empty_street)
})

test_that("different organizations at the same address get different ids", {
  urban    <- address_spell_id("EIN-52-0880375", "500 L'ENFANT PLAZA SW", "WASHINGTON", "DC", "20024")
  neighbor <- address_spell_id("EIN-52-0000001", "500 L'ENFANT PLAZA SW", "WASHINGTON", "DC", "20024")

  expect_false(urban == neighbor)
})

test_that("spell_id is vectorized over many rows", {
  ids <- address_spell_id(
    EIN2   = c("EIN-52-0880375", "EIN-52-0880375"),
    street = c("500 L'ENFANT PLAZA SW", "2100 M ST NW"),
    city   = c("WASHINGTON", "WASHINGTON"),
    state  = c("DC", "DC"),
    zip5   = c("20024", "20037")
  )

  expect_length(ids, 2)
  expect_false(ids[[1]] == ids[[2]])
})
