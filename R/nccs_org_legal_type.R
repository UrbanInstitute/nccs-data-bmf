# ============================================================================
# nccs_org_legal_type.R
# Derive the legal type of an organization (public charity, private
# foundation, other tax-exempt organization) from two IRS codes. Older NCCS
# files called this NCCS_LEVEL_1. Decided in ADR 0059.
#
# Terms used in this file, each with one meaning:
#
#   legal type       What kind of tax-exempt organization this is: "PC"
#                    (501(c)(3) public charity), "PF" (501(c)(3) private
#                    foundation) or "O" (any other tax-exempt
#                    organization). The older BMF files carry it as LEVEL1
#                    and older CORE files as NCCS_LEVEL_1.
#   subsection code  The IRS code for the kind of tax exemption. 3 means
#                    501(c)(3). 0 means the IRS file gives no subsection.
#   foundation code  The IRS code for the kind of 501(c)(3). 2, 3 and 4 are
#                    private foundations. 0 means the IRS file gives none.
#   blank            The legal type is left blank (NA in R) when a code it
#                    needs is blank. It is never a guess.
#
# This file needs R/nccs_org_role.R loaded first: it uses
# parse_nccs_org_role_irs_code() from there to read the two codes, so that
# the role and the legal type read a code the same way, and
# look_up_nccs_org_label() to put a value in words.
# ============================================================================

# ============================================================================
# 1. Constants
# ============================================================================

# The subsection code of a 501(c)(3).
NCCS_ORG_LEGAL_TYPE_CHARITY_SUBSECTION_CODE <- 3L

# The foundation codes of a private foundation: 2 (operating, exempt from
# excise tax), 3 (operating) and 4 (non-operating).
NCCS_ORG_LEGAL_TYPE_PRIVATE_FOUNDATION_CODES <- c(2L, 3L, 4L)

# The value both codes use when the IRS file gives no code.
NCCS_ORG_LEGAL_TYPE_NO_CODE <- 0L

# The legal type in words. Use look_up_nccs_org_label() with this vector.
NCCS_ORG_LEGAL_TYPE_LABELS <- c(
  PC = "Public charity",
  PF = "Private foundation",
  O = "Other tax-exempt organization"
)

# ============================================================================
# 2. Public function
# ============================================================================

#' Derive the legal type of an organization (formerly NCCS_LEVEL_1)
#'
#' @description
#' Rebuilds the grouping that older NCCS files carried as `LEVEL1` (BMF) and
#' `NCCS_LEVEL_1` (CORE): public charity ("PC"), private foundation ("PF")
#' or other tax-exempt organization ("O").
#'
#' @details
#' The rules, where the first one that fits wins:
#' \enumerate{
#'   \item Blank: the subsection code is blank or 0.
#'   \item Other tax-exempt organization: the subsection code is not 3.
#'   \item Blank: the foundation code is blank or 0.
#'   \item Private foundation: the foundation code is 2, 3 or 4.
#'   \item Public charity: every other foundation code.
#' }
#'
#' Difference from the older files. They marked a 501(c)(3) with no
#' foundation code as a public charity. Here it is blank, because nothing
#' says which of the two it is (ADR 0059 Decision 4). The older files' value
#' "U" for a subsection code of 0 is also left blank.
#'
#' Checked against the older BMF files of July 2013 and December 2017: the
#' rule gives the original `LEVEL1` on every row that has a subsection code
#' and, for a 501(c)(3), a foundation code.
#' `scripts/check_nccs_org_legal_type_and_field.R` repeats the comparison.
#'
#' @param subsection_code vector of subsection codes, for example the
#'   `subsection_code` column of the Unified BMF. Stored as "03", "3", 3 or
#'   "3.0", all read as 3.
#' @param foundation_code vector of foundation codes, the same length as
#'   `subsection_code`, for example the `foundation_code` column of the
#'   Unified BMF.
#'
#' @return character vector the same length as `subsection_code`, holding
#'   "PC", "PF", "O" or a blank (`NA`).
#'
#' @examples
#' derive_nccs_org_legal_type(
#'   subsection_code = c(3, 3, 4, 3, 0),
#'   foundation_code = c(15, 4, 0, NA, 15)
#' )
#' # "PC" "PF" "O" NA NA
#'
#' @export
derive_nccs_org_legal_type <- function(subsection_code, foundation_code) {
  if (length(subsection_code) != length(foundation_code)) {
    rlang::abort("subsection_code and foundation_code must be the same length.")
  }

  # Read both codes as whole numbers, so "03", "3" and 3 are the same code.
  subsection_number <- parse_nccs_org_role_irs_code(subsection_code)

  foundation_number <- parse_nccs_org_role_irs_code(foundation_code)

  # Name each condition before the rules use it.
  has_no_subsection_code <-
    is.na(subsection_number) |
    subsection_number %in% NCCS_ORG_LEGAL_TYPE_NO_CODE

  is_charity_subsection <-
    subsection_number %in% NCCS_ORG_LEGAL_TYPE_CHARITY_SUBSECTION_CODE

  has_no_foundation_code <-
    is.na(foundation_number) |
    foundation_number %in% NCCS_ORG_LEGAL_TYPE_NO_CODE

  is_private_foundation_code <-
    foundation_number %in% NCCS_ORG_LEGAL_TYPE_PRIVATE_FOUNDATION_CODES

  # The first rule that fits wins.
  legal_type <- dplyr::case_when(
    has_no_subsection_code ~ NA_character_,
    !is_charity_subsection ~ "O",
    has_no_foundation_code ~ NA_character_,
    is_private_foundation_code ~ "PF",
    .default = "PC"
  )

  return(legal_type)
}
