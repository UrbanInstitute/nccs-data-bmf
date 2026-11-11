# ============================================================================
# nccs_org_field.R
# Derive the field an organization works in (17 groups) from its NTEE code.
# Older NCCS files called this NCCS_LEVEL_3. Decided in ADR 0059.
#
# Terms used in this file, each with one meaning:
#
#   field         The broad area an organization works in, as one of 17
#                 two-letter values: nine subject areas (AR, ED, EN, HE, HS,
#                 IN, PB, RE, UN), two kinds of mutual benefit (MO, MR) and
#                 six kinds of supporting organization (ZA to ZF). The older
#                 BMF files carry it as LEVEL3 and older CORE files as
#                 NCCS_LEVEL_3.
#   NTEE code     The activity code of an organization: one letter (the
#                 major group), one digit, then up to three more letters or
#                 digits. For example B11, T22, Y30.
#   major group   The first letter of an NTEE code.
#   common code   Characters 2 and 3 of an NTEE code when they name a kind
#                 of activity found in every major group. 11 is single
#                 organization support and 12 is fundraising.
#   blank         The field is left blank (NA in R) when the NTEE code is
#                 blank or is not a code. This is not the value "UN",
#                 which means the IRS gave a code in major group Z.
#
# This file needs R/nccs_org_role.R loaded first: it uses
# clean_nccs_org_role_ntee_code() from there, so that the role and the field
# read an NTEE code the same way, and look_up_nccs_org_label() to put a
# value in words.
# ============================================================================

# ============================================================================
# 1. Constants
# ============================================================================

# Supporting organizations named by their common code, in any major group.
NCCS_ORG_FIELD_BY_COMMON_CODE <- c(
  "11" = "ZA",
  "12" = "ZB"
)

# Grantmaking organizations in major group T, named by the first two
# characters of the code.
NCCS_ORG_FIELD_BY_GRANTMAKING_PREFIX <- c(
  T2 = "ZC",
  T3 = "ZD",
  T7 = "ZE",
  T9 = "ZF"
)

# Pension and retirement funds: the code Y30, with or without a suffix. The
# other Y3 codes (Y33, Y34, Y35) are other mutual benefit, as in the older
# files.
NCCS_ORG_FIELD_BY_PENSION_PREFIX <- c(
  Y30 = "MR"
)

# Every other code, by its major group.
NCCS_ORG_FIELD_BY_MAJOR_GROUP <- c(
  A = "AR",
  B = "ED",
  C = "EN", D = "EN",
  E = "HE", F = "HE", G = "HE", H = "HE",
  I = "HS", J = "HS", K = "HS", L = "HS", M = "HS", N = "HS", O = "HS", P = "HS",
  Q = "IN",
  R = "PB", S = "PB", T = "PB", U = "PB", V = "PB", W = "PB",
  X = "RE",
  Y = "MO",
  Z = "UN"
)

# The field in words. Use look_up_nccs_org_label() with this vector.
NCCS_ORG_FIELD_LABELS <- c(
  AR = "Arts, culture and humanities",
  ED = "Education",
  EN = "Environment and animals",
  HE = "Health",
  HS = "Human services",
  IN = "International, foreign affairs",
  PB = "Public, societal benefit",
  RE = "Religion related",
  UN = "Unknown, unclassified",
  MO = "Other mutual benefit",
  MR = "Pension and retirement funds",
  ZA = "Single organization support",
  ZB = "Fundraising within one field",
  ZC = "Private grantmaking foundations",
  ZD = "Public foundations",
  ZE = "General fundraising",
  ZF = "Other supporting public benefit"
)

# ============================================================================
# 2. Public function
# ============================================================================

#' Derive the field of an organization (formerly NCCS_LEVEL_3)
#'
#' @description
#' Rebuilds the grouping that older NCCS files carried as `LEVEL3` (BMF) and
#' `NCCS_LEVEL_3` (CORE): one of 17 two-letter values.
#'
#' @details
#' The rules use the NTEE code alone. The first one that fits wins:
#' \enumerate{
#'   \item ZA, single organization support: the common code is 11.
#'   \item ZB, fundraising within one field: the common code is 12.
#'   \item ZC, private grantmaking foundations: the code starts with T2.
#'   \item ZD, public foundations: the code starts with T3.
#'   \item ZE, general fundraising: the code starts with T7.
#'   \item ZF, other supporting public benefit: the code starts with T9.
#'   \item MR, pension and retirement funds: the code starts with Y30.
#'   \item Every other code, by its major group: A is AR; B is ED; C and D
#'     are EN; E to H are HE; I to P are HS; Q is IN; R to W are PB; X is
#'     RE; Y is MO; Z is UN.
#' }
#' An organization whose NTEE code is blank gets a blank (`NA`). Punctuation
#' after a
#' code is ignored ("B12," is read as B12).
#'
#' Checked against the older BMF files of July 2013 and December 2017: the
#' rule gives the original `LEVEL3` on every row.
#' `scripts/check_nccs_org_legal_type_and_field.R` repeats the comparison.
#'
#' Limits. A T2, T3, T7 or T9 code that never appeared in the older files
#' follows the pattern of the ones that did. That is an extension, not
#' something observed.
#'
#' @param ntee_code character vector of NTEE codes, for example the
#'   `ntee_code_raw` column of the Unified BMF. Upper or lower case, with or
#'   without surrounding spaces.
#'
#' @return character vector the same length as `ntee_code`, holding one of
#'   the 17 two-letter values or a blank (`NA`).
#'
#' @examples
#' derive_nccs_org_field(c("B11", "T22", "Y30", "Y42", "P20", NA))
#' # "ZA" "ZC" "MR" "MO" "HS" NA
#'
#' @export
derive_nccs_org_field <- function(ntee_code) {
  clean_ntee_code <- clean_nccs_org_role_ntee_code(ntee_code)

  # The parts of the code the rules look at.
  major_group <- stringr::str_sub(clean_ntee_code, start = 1, end = 1)

  first_two_characters <- stringr::str_sub(clean_ntee_code, start = 1, end = 2)

  first_three_characters <- stringr::str_sub(clean_ntee_code, start = 1, end = 3)

  common_code <- stringr::str_sub(clean_ntee_code, start = 2, end = 3)

  # What each rule would give. A code the rule does not cover gives a blank.
  field_from_common_code <- unname(NCCS_ORG_FIELD_BY_COMMON_CODE[common_code])

  field_from_grantmaking_prefix <- unname(
    NCCS_ORG_FIELD_BY_GRANTMAKING_PREFIX[first_two_characters]
  )

  field_from_pension_prefix <- unname(
    NCCS_ORG_FIELD_BY_PENSION_PREFIX[first_three_characters]
  )

  field_from_major_group <- unname(NCCS_ORG_FIELD_BY_MAJOR_GROUP[major_group])

  # The first rule that gives a value wins.
  field <- dplyr::coalesce(
    field_from_common_code,
    field_from_grantmaking_prefix,
    field_from_pension_prefix,
    field_from_major_group
  )

  return(field)
}
