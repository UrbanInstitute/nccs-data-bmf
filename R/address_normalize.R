# ============================================================================
# address_normalize.R
#
# The normalization that defines an address spell in the address-resolved
# crosswalk, in R. The builder applies the same rules in DuckDB SQL
# (normalize_text_sql / normalize_zip5_sql in
# scripts/build_address_resolved_crosswalk.R); the validator and the
# address-history geocoding step use these. Change one and change the other.
# ============================================================================

#' Trim, upper-case, and turn an empty string into NA.
address_normalize_text <- function(raw_text) {
  normalized <- stringr::str_to_upper(stringr::str_trim(as.character(raw_text)))
  dplyr::if_else(is.na(normalized) | normalized == "", NA_character_, normalized)
}

#' Reduce a raw ZIP to the 5-digit form the crosswalk keys on.
#'
#' Keep digits only, take the first five (dropping the ZIP+4 add-on the
#' current pipeline carries), and put back any leading zero the legacy
#' pipeline dropped. Empty input stays NA.
address_normalize_zip5 <- function(raw_zip) {
  zip_digits <- stringr::str_replace_all(as.character(raw_zip), "[^0-9]", "")
  zip_base   <- stringr::str_sub(zip_digits, 1L, 5L)
  dplyr::if_else(
    is.na(zip_base) | zip_base == "",
    NA_character_,
    stringr::str_pad(zip_base, width = 5L, side = "left", pad = "0")
  )
}
