# ============================================================================
# address_spell_id.R
#
# The stable identifier for one address spell (one organization at one
# address) in the address-resolved crosswalk, nccs-contracts ADR 0051.
#
# Why it exists: spell_rank is renumbered whenever an organization gains a
# new address, so it cannot be used to join the address history to any table
# built in a different month. spell_id depends only on the organization and
# the address, so it never changes between builds. It is published in the
# address-resolved crosswalk and in the address-geo-resolved crosswalk, and
# the two are joined on it.
#
# Definition (ADR 0051 section 3): the first 16 hexadecimal characters of
# the SHA-256 of EIN2, street, city, state and zip5, in their normalized
# published form, joined with "|", with a missing value written as an empty
# string. Change this in one place only; the validator recomputes it.
# ============================================================================

ADDRESS_SPELL_ID_LENGTH <- 16L

#' Stable identifier for an address spell.
#'
#' @param EIN2   EIN in the EIN-XX-XXXXXXX form (ADR 0036).
#' @param street Normalized street, upper case and trimmed; NA when absent.
#' @param city   Normalized city.
#' @param state  Normalized state.
#' @param zip5   Normalized 5-digit ZIP.
#' @return Character vector of 16-character hexadecimal identifiers.
address_spell_id <- function(EIN2, street, city, state, zip5) {

  missing_as_empty <- function(values) {
    dplyr::coalesce(as.character(values), "")
  }

  key_text <- paste(
    missing_as_empty(EIN2),
    missing_as_empty(street),
    missing_as_empty(city),
    missing_as_empty(state),
    missing_as_empty(zip5),
    sep = "|"
  )

  # openssl::sha256() hashes every element of a character vector at once.
  full_hash <- as.character(openssl::sha256(key_text))

  substr(full_hash, 1L, ADDRESS_SPELL_ID_LENGTH)
}
