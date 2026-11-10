# ============================================================================
# check_nccs_org_role.R
# Compare the rebuilt role with the original one in two older BMF files.
#
# Terms used in this file, each with one meaning:
#
#   role               What an organization mainly does: operating,
#                      supporting or mutual benefit (see
#                      R/nccs_org_role.R). Formerly NCCS_LEVEL_2.
#   original role      The LEVEL2 column that an older BMF file carries.
#   rebuilt role       What derive_nccs_org_role() returns for the same row.
#   match share        The share of rows where the rebuilt role equals
#                      the original role. A row with a missing original
#                      role counts as not matching.
#
# One older BMF file is checked for each historical definition:
#   July 2013     -> definition "through_2016"
#   December 2017 -> definition "2017_onward"
#
# Usage (from the repo root):
#   Rscript scripts/check_nccs_org_role.R [local-2013-file] [local-2017-file]
#
# With no paths, each file (about 350 MB) is read straight from S3 over HTTPS.
# The script stops with an error if the match share of either file is below
# 99.99%. Small worked examples live in tests/testthat/test-nccs-org-role.R.
# ============================================================================

source("R/nccs_org_role.R")

# ============================================================================
# 1. Settings
# ============================================================================

OLDER_BMF_URL_TEMPLATE <- paste0(
  "https://nccsdata.s3.amazonaws.com/legacy/bmf/",
  "BMF-%s-501CX-NONPROFIT-PX.csv"
)

MINIMUM_MATCH_SHARE <- 0.9999

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

#' Compare the rebuilt role with the original role in one older BMF file
#'
#' @details
#' Reads one older BMF file, rebuilds the role with
#' [derive_nccs_org_role()] and prints the number of organizations and matches
#' by `LEVEL1` (public charity, private foundation, other). Stops with an
#' error if the file has no rows or if the match share is below
#' `minimum_match_share`.
#'
#' @param older_bmf_source path or URL of the older BMF file.
#' @param definition definition passed to [derive_nccs_org_role()].
#' @param minimum_match_share lowest acceptable match share.
#'
#' @return the match share, a number between 0 and 1.
compare_with_original_role <- function(older_bmf_source,
                                           definition,
                                           minimum_match_share) {
  older_bmf <- data.table::fread(
    older_bmf_source,
    select = c("NTEECC", "LEVEL1", "LEVEL2", "SUBSECCD", "FNDNCD"),
    colClasses = "character",
    na.strings = c("", "NA")
  )

  if (nrow(older_bmf) == 0) {
    stop("No rows read from ", older_bmf_source)
  }

  comparison <- older_bmf |>
    dplyr::as_tibble() |>
    dplyr::mutate(
      rebuilt_role = derive_nccs_org_role(
        ntee_code = NTEECC,
        subsection_code = SUBSECCD,
        foundation_code = FNDNCD,
        definition = definition
      ),
      is_match = dplyr::coalesce(rebuilt_role == LEVEL2, FALSE)
    )

  match_summary <- comparison |>
    dplyr::group_by(LEVEL1) |>
    dplyr::summarise(
      organizations = dplyr::n(),
      matching = sum(is_match),
      .groups = "drop"
    )

  total_organizations <- sum(match_summary$organizations)

  total_matching <- sum(match_summary$matching)

  match_share <- total_matching / total_organizations

  cat(sprintf("\n%s, definition \"%s\"\n", basename(older_bmf_source), definition))

  print(match_summary)

  cat(sprintf(
    "Rebuilt role matches the original for %s of %s organizations (%.4f%%)\n",
    format(total_matching, big.mark = ","),
    format(total_organizations, big.mark = ","),
    100 * match_share
  ))

  if (match_share < minimum_match_share) {
    stop(
      "derive_nccs_org_role() no longer reproduces the original LEVEL2 column in ",
      basename(older_bmf_source)
    )
  }

  return(match_share)
}

match_share_2013 <- compare_with_original_role(
  older_bmf_source = older_bmf_source_2013,
  definition = "through_2016",
  minimum_match_share = MINIMUM_MATCH_SHARE
)

match_share_2017 <- compare_with_original_role(
  older_bmf_source = older_bmf_source_2017,
  definition = "2017_onward",
  minimum_match_share = MINIMUM_MATCH_SHARE
)
