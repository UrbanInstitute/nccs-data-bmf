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
#'
#' Two renderings of the same street, city or state ("Washington ",
#' "WASHINGTON") must compare equal, or one address would count as two.
#'
#' @param raw_text Character vector (or anything as.character() accepts): a
#'   raw street, city or state field.
#' @return Character vector of the same length: trimmed and upper case, with
#'   empty and missing values as NA.
address_normalize_text <- function(raw_text) {

  trimmed_text    <- stringr::str_trim(as.character(raw_text))
  upper_case_text <- stringr::str_to_upper(trimmed_text)

  is_empty <- is.na(upper_case_text) | upper_case_text == ""

  normalized_text <- dplyr::if_else(is_empty, NA_character_, upper_case_text)

  return(normalized_text)

}

#' Reduce a raw ZIP to the 5-digit form the crosswalk keys on.
#'
#' Keep digits only, take the first five (dropping the ZIP+4 add-on the
#' current pipeline carries), and put back any leading zero the legacy
#' pipeline dropped.
#'
#' @param raw_zip Character or numeric vector: a raw ZIP field, for example
#'   "02138-1234" or 2138.
#' @return Character vector of the same length: exactly five digits, or NA
#'   when the input is empty or missing.
address_normalize_zip5 <- function(raw_zip) {

  zip_digits <- stringr::str_replace_all(as.character(raw_zip), "[^0-9]", "")
  zip_base   <- stringr::str_sub(zip_digits, 1L, 5L)

  is_empty <- is.na(zip_base) | zip_base == ""

  zip5 <- dplyr::if_else(
    is_empty,
    NA_character_,
    stringr::str_pad(zip_base, width = 5L, side = "left", pad = "0")
  )

  return(zip5)

}
