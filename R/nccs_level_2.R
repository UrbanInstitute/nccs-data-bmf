# ============================================================================
# nccs_level_2.R
# Rebuild the old NCCS_LEVEL_2 grouping from three BMF fields
#
# Terms used in this file, each with one meaning:
#
#   grouping         The old NCCS_LEVEL_2 value: "O" (operating),
#                    "S" (supporting) or "M" (mutual benefit). Older BMF
#                    files carry it as LEVEL2.
#   NTEE code        The activity code of an organization: one letter (the
#                    major group) followed by up to four letters or digits,
#                    for example B11, E112 or B1170.
#   common code      Characters 2 and 3 of an NTEE code when they name a
#                    role that exists in every major group. 11 is support for
#                    a single organization; 12 is fund raising and fund
#                    distribution.
#   subsection code  The IRS code for the kind of tax exemption. 3 means
#                    501(c)(3).
#   foundation code  The IRS code for the kind of 501(c)(3). 4 is a private
#                    non-operating foundation; 17 is an organization run for
#                    the benefit of another public charity.
#   definition       Which of the two historical ways of building the
#                    grouping to use: "through_2016" (NTEE code alone) or
#                    "2017_onward" (NTEE code, subsection code and foundation
#                    code).
# ============================================================================

# ============================================================================
# 1. Constants
# ============================================================================

# A real NTEE code: one letter, then up to four letters or digits.
NCCS_LEVEL_2_NTEE_CODE_PATTERN <- "^[A-Z][A-Z0-9]{0,4}$"

# The two common codes that mark a supporting organization.
NCCS_LEVEL_2_SUPPORT_COMMON_CODES <- c("11", "12")

# Major group Y holds mutual and membership benefit organizations.
NCCS_LEVEL_2_MUTUAL_BENEFIT_MAJOR_GROUP <- "Y"

# Major group T holds philanthropy, voluntarism and grantmaking. Its codes
# are supporting, except the ones below and every code whose second character
# is 0 (T01, T033), which the older files kept as operating.
NCCS_LEVEL_2_GRANTMAKING_MAJOR_GROUP <- "T"
NCCS_LEVEL_2_OPERATING_GRANTMAKING_CODES <- c("T19", "T40", "T50")
NCCS_LEVEL_2_OPERATING_SECOND_CHARACTER <- "0"

# From September 2017 on, the older files also counted a 501(c)(3) with one
# of these foundation codes as supporting, whatever its NTEE code.
NCCS_LEVEL_2_SUPPORT_SUBSECTION_CODE <- 3L
NCCS_LEVEL_2_SUPPORT_FOUNDATION_CODES <- c(4L, 17L)

NCCS_LEVEL_2_DEFINITIONS <- c("2017_onward", "through_2016")

NCCS_LEVEL_2_LABELS <- c(
  O = "Operating",
  S = "Supporting",
  M = "Mutual benefit"
)

# ============================================================================
# 2. Reading the inputs
# ============================================================================

#' Clean a vector of NTEE codes
#'
#' @details
#' Trims surrounding spaces and converts to upper case. A value that is
#' missing, blank or not a real NTEE code (one letter, then up to four
#' letters or digits) becomes `NA`, so that it is never mistaken for an
#' operating organization later.
#'
#' @param ntee_code character vector of NTEE codes as stored in a file.
#'
#' @return character vector the same length as `ntee_code`, holding a clean
#'   NTEE code or `NA`.
#'
#' @keywords internal
clean_nccs_level_2_ntee_code <- function(ntee_code) {
  trimmed_code <- stringr::str_trim(as.character(ntee_code))

  upper_case_code <- stringr::str_to_upper(trimmed_code)

  is_real_ntee_code <- stringr::str_detect(
    upper_case_code,
    pattern = NCCS_LEVEL_2_NTEE_CODE_PATTERN
  )

  clean_ntee_code <- dplyr::if_else(
    is_real_ntee_code,
    upper_case_code,
    NA_character_,
    missing = NA_character_
  )

  return(clean_ntee_code)
}

#' Read a subsection code or foundation code as a whole number
#'
#' @details
#' The older and current BMF files store these codes as "03", "3", 3 or
#' "3.0" depending on the file. All four become 3. A blank or unreadable
#' value becomes `NA`.
#'
#' @param irs_code character, integer or numeric vector of subsection codes
#'   or foundation codes.
#'
#' @return integer vector the same length as `irs_code`.
#'
#' @keywords internal
parse_nccs_level_2_irs_code <- function(irs_code) {
  code_as_text <- as.character(irs_code)

  code_as_number <- suppressWarnings(as.numeric(code_as_text))

  code_as_integer <- as.integer(code_as_number)

  return(code_as_integer)
}

# ============================================================================
# 3. The two rules
# ============================================================================

#' Derive the grouping from the NTEE code alone
#'
#' @details
#' Applies the NTEE code rules in order, where the first one that fits wins:
#' \enumerate{
#'   \item Supporting: the common code is 11 or 12 (B11, E112).
#'   \item Mutual benefit: the major group is Y.
#'   \item Supporting: the major group is T, except T0x, T19, T40 and T50.
#'   \item Operating: every other NTEE code.
#' }
#' `NA` in gives `NA` out.
#'
#' @param clean_ntee_code character vector from
#'   [clean_nccs_level_2_ntee_code()].
#'
#' @return character vector the same length as `clean_ntee_code`, holding
#'   "O", "S", "M" or `NA`.
#'
#' @keywords internal
derive_nccs_level_2_from_ntee_code <- function(clean_ntee_code) {
  major_group <- stringr::str_sub(clean_ntee_code, start = 1, end = 1)

  second_character <- stringr::str_sub(clean_ntee_code, start = 2, end = 2)

  common_code <- stringr::str_sub(clean_ntee_code, start = 2, end = 3)

  first_three_characters <- stringr::str_sub(clean_ntee_code, start = 1, end = 3)

  is_operating_grantmaking_code <-
    second_character %in% NCCS_LEVEL_2_OPERATING_SECOND_CHARACTER |
    first_three_characters %in% NCCS_LEVEL_2_OPERATING_GRANTMAKING_CODES

  is_supporting_grantmaking_code <-
    major_group %in% NCCS_LEVEL_2_GRANTMAKING_MAJOR_GROUP &
    !is_operating_grantmaking_code

  grouping_from_ntee_code <- dplyr::case_when(
    is.na(clean_ntee_code) ~ NA_character_,
    common_code %in% NCCS_LEVEL_2_SUPPORT_COMMON_CODES ~ "S",
    major_group %in% NCCS_LEVEL_2_MUTUAL_BENEFIT_MAJOR_GROUP ~ "M",
    is_supporting_grantmaking_code ~ "S",
    .default = "O"
  )

  return(grouping_from_ntee_code)
}

#' Flag organizations that are supporting because of their foundation code
#'
#' @details
#' An organization is flagged when its subsection code is 3 (501(c)(3)) and
#' its foundation code is 4 or 17. A missing subsection code or foundation
#' code is never flagged. Stops if either input is absent or is not the
#' expected length.
#'
#' @param subsection_code vector of subsection codes, in any stored form.
#' @param foundation_code vector of foundation codes, in any stored form.
#' @param expected_length number of organizations, which both inputs must
#'   match.
#'
#' @return logical vector of length `expected_length`, `TRUE` where the
#'   foundation code makes the organization supporting.
#'
#' @keywords internal
is_supporting_by_foundation_code <- function(subsection_code,
                                             foundation_code,
                                             expected_length) {
  if (is.null(subsection_code) || is.null(foundation_code)) {
    rlang::abort(paste(
      "definition = \"2017_onward\" needs subsection_code and foundation_code.",
      "Use definition = \"through_2016\" to work from the NTEE code alone."
    ))
  }

  has_expected_lengths <-
    length(subsection_code) == expected_length &&
    length(foundation_code) == expected_length

  if (!has_expected_lengths) {
    rlang::abort(
      "subsection_code and foundation_code must be the same length as ntee_code."
    )
  }

  is_support_subsection <-
    parse_nccs_level_2_irs_code(subsection_code) %in% NCCS_LEVEL_2_SUPPORT_SUBSECTION_CODE

  is_support_foundation <-
    parse_nccs_level_2_irs_code(foundation_code) %in% NCCS_LEVEL_2_SUPPORT_FOUNDATION_CODES

  is_supporting <- is_support_subsection & is_support_foundation

  return(is_supporting)
}

# ============================================================================
# 4. Public functions
# ============================================================================

#' Derive the old NCCS_LEVEL_2 grouping
#'
#' @description
#' Rebuilds the grouping that older NCCS files carried as `LEVEL2` (BMF) and
#' `NCCS_LEVEL_2` (CORE): operating ("O"), supporting ("S") or mutual benefit
#' ("M").
#'
#' @details
#' The old grouping had two definitions over time.
#'
#' `definition = "through_2016"` uses the NTEE code alone, applying these
#' rules in order, where the first one that fits wins:
#' \enumerate{
#'   \item Supporting: characters 2 and 3 of the NTEE code are 11 or 12
#'     (B11, E112).
#'   \item Mutual benefit: the NTEE code starts with Y.
#'   \item Supporting: the NTEE code starts with T, except T0x, T19, T40 and
#'     T50.
#'   \item Operating: every other NTEE code.
#' }
#' This is how the BMF files from June 1989 through August 2016 were built.
#'
#' `definition = "2017_onward"` (the default) puts one rule ahead of those: a
#' 501(c)(3) organization (subsection code 3) with foundation code 4 or 17 is
#' supporting, whatever its NTEE code. This is how the BMF files from
#' September 2017 through August 2022 were built.
#'
#' The result is `NA` when the NTEE code is missing, blank or not a real NTEE
#' code (one letter, then up to four letters or digits), unless the
#' foundation code rule already settles the answer.
#'
#' Evidence. The rules were worked out by comparing the original `LEVEL2`
#' column with the other columns in the older BMF files under `legacy/bmf/`.
#' They were not taken from written documentation. Files tested, with the
#' share of organizations where the rebuilt grouping equals the original:
#' June 1989, June 2014 and July 2015 (100%), July 2013 and August 2016 (all
#' but 1 and 2 organizations) under "through_2016"; September 2017, December
#' 2017 and December 2018 (100%), August 2019, April 2020 and August 2022
#' (all but 2 or 3 organizations) under "2017_onward". No file between August
#' 2016 and September 2017 exists, so the exact month of the change is not
#' known. `scripts/check_nccs_level_2.R` repeats two of these comparisons.
#'
#' Limits. Every T code that appears in the older files (39 codes) is
#' reproduced, but a T code that never appeared there is handled by the
#' general pattern, which is an extension, not something observed.
#'
#' @param ntee_code character vector of NTEE codes, for example the
#'   `ntee_code_raw` column of the Unified BMF. Upper or lower case, with or
#'   without surrounding spaces.
#' @param subsection_code vector of subsection codes, the same length as
#'   `ntee_code`. Needed for "2017_onward"; ignored for "through_2016".
#' @param foundation_code vector of foundation codes, the same length as
#'   `ntee_code`. Needed for "2017_onward"; ignored for "through_2016".
#' @param definition which historical definition to rebuild: "2017_onward"
#'   (default) or "through_2016".
#'
#' @return character vector the same length as `ntee_code`, holding "O", "S",
#'   "M" or `NA`.
#'
#' @examples
#' derive_nccs_level_2(
#'   ntee_code = c("B11", "Y42", "T22", "T40", "P20", "P20", NA),
#'   subsection_code = c(3, 3, 3, 3, 3, 3, 3),
#'   foundation_code = c(15, 15, 4, 15, 15, 17, 15)
#' )
#' # "S" "M" "S" "O" "O" "S" NA
#'
#' derive_nccs_level_2(c("B11", "P20"), definition = "through_2016")
#' # "S" "O"
#'
#' @export
derive_nccs_level_2 <- function(ntee_code,
                                subsection_code = NULL,
                                foundation_code = NULL,
                                definition = "2017_onward") {
  definition <- rlang::arg_match(definition, values = NCCS_LEVEL_2_DEFINITIONS)

  clean_ntee_code <- clean_nccs_level_2_ntee_code(ntee_code)

  grouping_from_ntee_code <- derive_nccs_level_2_from_ntee_code(clean_ntee_code)

  if (definition == "through_2016") {
    return(grouping_from_ntee_code)
  }

  is_supporting <- is_supporting_by_foundation_code(
    subsection_code = subsection_code,
    foundation_code = foundation_code,
    expected_length = length(clean_ntee_code)
  )

  grouping <- dplyr::if_else(is_supporting, "S", grouping_from_ntee_code)

  return(grouping)
}

#' Label an NCCS_LEVEL_2 grouping in words
#'
#' @details
#' "O" becomes "Operating", "S" becomes "Supporting" and "M" becomes
#' "Mutual benefit". Any other value, including `NA`, returns `NA`.
#'
#' @param nccs_level_2 character vector of "O", "S", "M" or `NA`, as returned
#'   by [derive_nccs_level_2()].
#'
#' @return character vector the same length as `nccs_level_2`.
#'
#' @export
label_nccs_level_2 <- function(nccs_level_2) {
  grouping_label <- unname(NCCS_LEVEL_2_LABELS[as.character(nccs_level_2)])

  return(grouping_label)
}
