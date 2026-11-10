# ============================================================================
# nccs_level_2.R
# Rebuild the old NCCS_LEVEL_2 grouping (operating, supporting, mutual
# benefit) from an NTEE code, subsection code and foundation code
# ============================================================================

# ============================================================================
# Constants
# ============================================================================

# NTEE "common code" endings that mark a support organization, in any major
# group: 11 = single organization support, 12 = fund raising and fund
# distribution. They sit in characters 2 and 3 of the code (B11, E112).
NCCS_LEVEL_2_SUPPORT_COMMON_CODES <- c("11", "12")

# Codes in major group T (philanthropy, voluntarism and grantmaking) that the
# old variable kept as operating, together with every T code whose second
# character is 0 (T01, T033). All other T codes seen in the older files were
# supporting.
NCCS_LEVEL_2_OPERATING_T_CODES <- c("T19", "T40", "T50")

# From September 2017 on, the old variable also counted 501(c)(3)
# organizations with these IRS foundation codes as supporting, whatever their
# NTEE code: 4 = private non-operating foundation, 17 = organization run for
# the benefit of another public charity (a 509(a)(3) supporting organization).
NCCS_LEVEL_2_SUPPORT_SUBSECTION_CODE <- 3L
NCCS_LEVEL_2_SUPPORT_FOUNDATION_CODES <- c(4L, 17L)

NCCS_LEVEL_2_DEFINITIONS <- c("2017_onward", "through_2016")

NCCS_LEVEL_2_LABELS <- c(
  O = "Operating",
  S = "Supporting",
  M = "Mutual benefit"
)

# ============================================================================
# Helper Functions
# ============================================================================

#' Read an IRS code as a whole number
#'
#' @description
#' Turns a subsection or foundation code into an integer, however it is
#' stored.
#'
#' @details
#' The older and current BMF files store these codes as "03", "3", 3 or
#' "3.0" depending on the file. All four become 3. Blank or unreadable values
#' become `NA`.
#'
#' @param code character, integer or numeric vector of codes.
#'
#' @return integer vector the same length as `code`.
#'
#' @keywords internal
parse_nccs_level_2_code <- function(code) {
  code_as_text <- stringr::str_trim(as.character(code))
  suppressWarnings(as.integer(as.numeric(code_as_text)))
}

# ============================================================================
# Derivation Functions
# ============================================================================

#' Derive the old NCCS_LEVEL_2 grouping
#'
#' @description
#' Rebuilds the grouping that older NCCS files carried as `LEVEL2` (BMF) and
#' `NCCS_LEVEL_2` (CORE): operating ("O"), supporting ("S") or mutual benefit
#' ("M").
#'
#' @details
#' The old variable had two definitions over time. Both start from the same
#' NTEE code rules, applied in this order, where the first one that fits wins:
#' \enumerate{
#'   \item Supporting: characters 2 and 3 of the code are 11 or 12
#'     (for example B11, E112).
#'   \item Mutual benefit: the code starts with Y.
#'   \item Supporting: the code starts with T, except T0x, T19, T40 and T50.
#'   \item Operating: every other code.
#' }
#'
#' `definition = "through_2016"` uses the NTEE code rules alone. This is how
#' the BMF files from June 1989 through August 2016 were built.
#'
#' `definition = "2017_onward"` (the default) adds one rule ahead of the
#' others: a 501(c)(3) organization (subsection code 3) with foundation code
#' 4 or 17 is supporting, whatever its NTEE code. This is how the BMF files
#' from September 2017 through August 2022 were built.
#'
#' A missing or blank NTEE code returns `NA`, unless the foundation code rule
#' already settles the answer.
#'
#' Evidence. The rules were worked out by comparing the original `LEVEL2`
#' column with the other columns in the older BMF files under `legacy/bmf/`.
#' They were not taken from written documentation. Files tested, with the
#' share of organizations where the rebuilt value equals the original:
#' June 1989, June 2014 and July 2015 (100%), July 2013 and August 2016 (all
#' but 1 and 2 organizations) under "through_2016"; September 2017, December
#' 2017 and December 2018 (100%), August 2019, April 2020 and August 2022
#' (all but 2 or 3 organizations) under "2017_onward". No file between August
#' 2016 and September 2017 exists, so the exact month of the change is not
#' known. `scripts/check_nccs_level_2.R` repeats two of these comparisons.
#'
#' Limits. Every T code that appears in the older files (39 codes) is
#' reproduced, but a T code that never appeared there is handled by the
#' general pattern, which is an extension, not something observed. Values
#' that are not real NTEE codes ("123", "UNDEFINED") fall through to
#' operating, so pass a column of real codes with missing values left as
#' `NA`, such as `ntee_code_raw` in the Unified BMF.
#'
#' @param ntee_code character vector of NTEE codes, for example the
#'   `ntee_code_raw` column of the Unified BMF. Codes are usually 3 or 4
#'   characters; 5-character codes are accepted. Upper or lower case, with or
#'   without surrounding spaces.
#' @param subsection_code vector of IRS subsection codes, the same length as
#'   `ntee_code` (3 means 501(c)(3)). Needed for "2017_onward"; ignored for
#'   "through_2016".
#' @param foundation_code vector of IRS foundation codes, the same length as
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

  # ---- 1. NTEE code rules ---------------------------------------------------

  clean_code <- stringr::str_to_upper(stringr::str_trim(as.character(ntee_code)))
  clean_code <- dplyr::na_if(clean_code, "")

  major_group <- stringr::str_sub(clean_code, start = 1, end = 1)
  second_character <- stringr::str_sub(clean_code, start = 2, end = 2)
  common_code <- stringr::str_sub(clean_code, start = 2, end = 3)
  first_three_characters <- stringr::str_sub(clean_code, start = 1, end = 3)

  is_support_common_code <- common_code %in% NCCS_LEVEL_2_SUPPORT_COMMON_CODES
  is_operating_t_code <- second_character %in% "0" |
    first_three_characters %in% NCCS_LEVEL_2_OPERATING_T_CODES

  # ---- 2. Foundation code rule (2017 onward only) ---------------------------

  is_support_by_foundation_code <- rep(FALSE, length(clean_code))

  if (definition == "2017_onward") {
    if (is.null(subsection_code) || is.null(foundation_code)) {
      rlang::abort(paste(
        "definition = \"2017_onward\" needs subsection_code and foundation_code.",
        "Use definition = \"through_2016\" to work from the NTEE code alone."
      ))
    }

    if (length(subsection_code) != length(clean_code) ||
        length(foundation_code) != length(clean_code)) {
      rlang::abort(
        "subsection_code and foundation_code must be the same length as ntee_code."
      )
    }

    is_support_by_foundation_code <-
      parse_nccs_level_2_code(subsection_code) %in% NCCS_LEVEL_2_SUPPORT_SUBSECTION_CODE &
      parse_nccs_level_2_code(foundation_code) %in% NCCS_LEVEL_2_SUPPORT_FOUNDATION_CODES
  }

  # ---- 3. Combine -----------------------------------------------------------

  dplyr::case_when(
    is_support_by_foundation_code ~ "S",
    is.na(clean_code) ~ NA_character_,
    is_support_common_code ~ "S",
    major_group == "Y" ~ "M",
    major_group == "T" & !is_operating_t_code ~ "S",
    .default = "O"
  )
}

#' Label an NCCS_LEVEL_2 value in words
#'
#' @description
#' Turns the one-letter NCCS_LEVEL_2 value into a readable label.
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
  unname(NCCS_LEVEL_2_LABELS[as.character(nccs_level_2)])
}
