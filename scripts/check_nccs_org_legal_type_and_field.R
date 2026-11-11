# ============================================================================
# check_nccs_org_legal_type_and_field.R
# Compare the rebuilt legal type and the rebuilt field with the original
# columns in two older BMF files (ADR 0059).
#
# Terms used in this file, each with one meaning:
#
#   legal type        Public charity, private foundation or other tax-exempt
#                     organization (see R/nccs_org_legal_type.R). Formerly
#                     NCCS_LEVEL_1.
#   field             The area an organization works in, 17 groups (see
#                     R/nccs_org_field.R). Formerly NCCS_LEVEL_3.
#   original value    The LEVEL1 or LEVEL3 column that an older BMF file
#                     carries.
#   rebuilt value     What derive_nccs_org_legal_type() or
#                     derive_nccs_org_field() returns for the same row.
#   comparable row    A row where the rebuilt value is meant to equal the
#                     original value. Every row is comparable for the field.
#                     For the legal type, a row is comparable when the
#                     rebuilt value is not missing: ADR 0059 leaves the
#                     legal type missing where the older files guessed
#                     (no subsection code, or a 501(c)(3) with no foundation
#                     code).
#   match share       The share of comparable rows where the rebuilt value
#                     equals the original value.
#
# Two older BMF files are checked: July 2013 and December 2017.
#
# Usage (from the repo root):
#   Rscript scripts/check_nccs_org_legal_type_and_field.R [local-2013-file] [local-2017-file]
#
# With no paths, each file (about 350 MB) is read straight from S3 over HTTPS.
# The script stops with an error if any match share is below 100%, or if more
# than 0.2% of rows are not comparable for the legal type. Small worked
# examples live in tests/testthat/test-nccs-org-legal-type.R and
# tests/testthat/test-nccs-org-field.R.
# ============================================================================

source("R/nccs_org_role.R")
source("R/nccs_org_legal_type.R")
source("R/nccs_org_field.R")

# ============================================================================
# 1. Settings
# ============================================================================

OLDER_BMF_URL_TEMPLATE <- paste0(
  "https://nccsdata.s3.amazonaws.com/legacy/bmf/",
  "BMF-%s-501CX-NONPROFIT-PX.csv"
)

OLDER_BMF_COLUMNS <- c("NTEECC", "LEVEL1", "LEVEL3", "SUBSECCD", "FNDNCD")

# Both rules reproduced every comparable row when ADR 0059 was written.
MINIMUM_MATCH_SHARE <- 1

# In December 2017, 1,723 of 1,544,575 rows (0.11%) were not comparable for
# the legal type. A much larger share would mean the codes are being read
# wrongly.
MAXIMUM_NOT_COMPARABLE_SHARE <- 0.002

command_arguments <- commandArgs(trailingOnly = TRUE)

older_bmf_source_2013 <- sprintf(OLDER_BMF_URL_TEMPLATE, "2013-07")

older_bmf_source_2017 <- sprintf(OLDER_BMF_URL_TEMPLATE, "2017-12")

if (length(command_arguments) >= 1) {
  older_bmf_source_2013 <- command_arguments[[1]]
}

if (length(command_arguments) >= 2) {
  older_bmf_source_2017 <- command_arguments[[2]]
}

# ============================================================================
# 2. Comparison
# ============================================================================

#' Report one match share and stop if it is too low
#'
#' @details
#' Prints one line with the number of comparable rows and the number that
#' match. Stops with an error when the match share is below
#' `MINIMUM_MATCH_SHARE`.
#'
#' @param is_match logical vector, one value per comparable row.
#' @param column_label what was compared, for the printed line and the
#'   error message, for example "legal type (LEVEL1)".
#' @param file_label name of the older BMF file, for the error message.
#'
#' @return the match share, a number between 0 and 1.
report_match_share <- function(is_match, column_label, file_label) {
  comparable_rows <- length(is_match)

  matching_rows <- sum(is_match)

  match_share <- matching_rows / comparable_rows

  cat(sprintf(
    "Rebuilt %s matches the original for %s of %s comparable rows (%.4f%%)\n",
    column_label,
    format(matching_rows, big.mark = ","),
    format(comparable_rows, big.mark = ","),
    100 * match_share
  ))

  if (match_share < MINIMUM_MATCH_SHARE) {
    stop("The rule no longer reproduces the original ", column_label, " in ", file_label)
  }

  return(match_share)
}

#' Compare the rebuilt legal type and field with one older BMF file
#'
#' @details
#' Reads one older BMF file, rebuilds both values with
#' [derive_nccs_org_legal_type()] and [derive_nccs_org_field()], and prints
#' the match share of each. Stops with an error if the file has no rows, if
#' either match share is below `MINIMUM_MATCH_SHARE`, or if more than
#' `MAXIMUM_NOT_COMPARABLE_SHARE` of rows are not comparable for the legal
#' type.
#'
#' @param older_bmf_source path or URL of the older BMF file.
#'
#' @return a named numeric vector with the two match shares, `legal_type`
#'   and `field`.
compare_with_original_columns <- function(older_bmf_source) {
  file_label <- basename(older_bmf_source)

  older_bmf <- data.table::fread(
    older_bmf_source,
    select = OLDER_BMF_COLUMNS,
    colClasses = "character",
    na.strings = c("", "NA")
  )

  if (nrow(older_bmf) == 0) {
    stop("No rows read from ", older_bmf_source)
  }

  comparison <- older_bmf |>
    dplyr::as_tibble() |>
    dplyr::mutate(
      rebuilt_legal_type = derive_nccs_org_legal_type(
        subsection_code = SUBSECCD,
        foundation_code = FNDNCD
      ),
      rebuilt_field = derive_nccs_org_field(NTEECC),
      is_legal_type_match = dplyr::coalesce(rebuilt_legal_type == LEVEL1, FALSE),
      is_field_match = dplyr::coalesce(rebuilt_field == LEVEL3, FALSE)
    )

  cat(sprintf("\n%s, %s rows\n", file_label, format(nrow(comparison), big.mark = ",")))

  # Legal type: only rows where the rule gives a value are comparable.
  comparable_for_legal_type <- comparison |>
    dplyr::filter(!is.na(rebuilt_legal_type))

  not_comparable_share <- 1 - nrow(comparable_for_legal_type) / nrow(comparison)

  cat(sprintf(
    "Legal type left missing by the rule on %s rows (%.2f%%)\n",
    format(nrow(comparison) - nrow(comparable_for_legal_type), big.mark = ","),
    100 * not_comparable_share
  ))

  if (not_comparable_share > MAXIMUM_NOT_COMPARABLE_SHARE) {
    stop("Too many rows have no rebuilt legal type in ", file_label)
  }

  legal_type_match_share <- report_match_share(
    is_match = comparable_for_legal_type$is_legal_type_match,
    column_label = "legal type (LEVEL1)",
    file_label = file_label
  )

  # Field: every row is comparable.
  field_match_share <- report_match_share(
    is_match = comparison$is_field_match,
    column_label = "field (LEVEL3)",
    file_label = file_label
  )

  match_shares <- c(
    legal_type = legal_type_match_share,
    field = field_match_share
  )

  return(match_shares)
}

match_shares_2013 <- compare_with_original_columns(older_bmf_source_2013)

match_shares_2017 <- compare_with_original_columns(older_bmf_source_2017)
