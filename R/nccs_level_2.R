# ============================================================================
# nccs_level_2.R
# Rebuild the old NCCS_LEVEL_2 grouping (operating, supporting, mutual
# benefit) from an NTEE code
# ============================================================================

# ============================================================================
# Constants
# ============================================================================

# NTEE "common code" endings that mark a support organization, in any major
# group: 11 = single organization support, 12 = fund raising and fund
# distribution. They sit in characters 2 and 3 of the code (B11, E112).
NCCS_LEVEL_2_SUPPORT_COMMON_CODES <- c("11", "12")

# Codes in major group T (philanthropy, voluntarism and grantmaking) that the
# old variable kept as operating. Every other T code was supporting.
NCCS_LEVEL_2_OPERATING_T_CODES <- c("T19", "T40", "T50")

NCCS_LEVEL_2_LABELS <- c(
  O = "Operating",
  S = "Supporting",
  M = "Mutual benefit"
)

# ============================================================================
# Derivation Functions
# ============================================================================

#' Derive the old NCCS_LEVEL_2 grouping from an NTEE code
#'
#' @description
#' Rebuilds the grouping that older NCCS files carried as `LEVEL2` (BMF) and
#' `NCCS_LEVEL_2` (CORE): operating ("O"), supporting ("S") or mutual benefit
#' ("M").
#'
#' @details
#' The rules are applied in this order, and the first one that fits wins:
#' \enumerate{
#'   \item Supporting: characters 2 and 3 of the code are 11 or 12
#'     (for example B11, E112).
#'   \item Mutual benefit: the code starts with Y.
#'   \item Supporting: the code starts with T, except T0x, T19, T40 and T50.
#'   \item Operating: every other code.
#' }
#' A missing or blank code returns `NA`, because the grouping cannot be known
#' without an NTEE code.
#'
#' The rules were recovered from the July 2013 BMF
#' (`legacy/bmf/BMF-2013-07-501CX-NONPROFIT-PX.csv`), which still carries the
#' original `LEVEL2` column. They reproduce it for 1,427,583 of 1,427,584
#' organizations, across public charities, private foundations and other
#' 501(c) types. `scripts/check_nccs_level_2.R` repeats that comparison.
#'
#' @param ntee_code character vector of NTEE codes, 1 to 4 characters long,
#'   for example the `ntee_code_raw` column of the Unified BMF. Upper or lower
#'   case, with or without surrounding spaces.
#'
#' @return character vector the same length as `ntee_code`, holding "O", "S",
#'   "M" or `NA`.
#'
#' @examples
#' derive_nccs_level_2(c("B11", "Y42", "T22", "T40", "P20", NA))
#' # "S" "M" "S" "O" "O" NA
#'
#' @export
derive_nccs_level_2 <- function(ntee_code) {
  clean_code <- stringr::str_to_upper(stringr::str_trim(as.character(ntee_code)))
  clean_code <- dplyr::na_if(clean_code, "")

  major_group <- stringr::str_sub(clean_code, start = 1, end = 1)
  second_character <- stringr::str_sub(clean_code, start = 2, end = 2)
  common_code <- stringr::str_sub(clean_code, start = 2, end = 3)
  first_three_characters <- stringr::str_sub(clean_code, start = 1, end = 3)

  is_support_common_code <- common_code %in% NCCS_LEVEL_2_SUPPORT_COMMON_CODES
  is_operating_t_code <- second_character %in% "0" |
    first_three_characters %in% NCCS_LEVEL_2_OPERATING_T_CODES

  dplyr::case_when(
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
