# ============================================================================
# check_nccs_level_2.R
# Compare derive_nccs_level_2() with the original LEVEL2 column in two older
# BMF files, one for each historical definition:
#   July 2013     -> definition "through_2016" (NTEE code alone)
#   December 2017 -> definition "2017_onward"  (NTEE code plus foundation code)
#
# Usage (from the repo root):
#   Rscript scripts/check_nccs_level_2.R [local-2013-file] [local-2017-file]
#
# With no paths, each file (about 350 MB) is read straight from S3 over HTTPS.
# The script stops with an error if fewer than 99.99% of rows match in either
# file. A row with a missing original value counts as not matching.
# ============================================================================

source("R/nccs_level_2.R")

# ============================================================================
# 1. Settings
# ============================================================================

LEGACY_BMF_URL_TEMPLATE <- paste0(
  "https://nccsdata.s3.amazonaws.com/legacy/bmf/",
  "BMF-%s-501CX-NONPROFIT-PX.csv"
)
MINIMUM_MATCH_SHARE <- 0.9999

command_arguments <- commandArgs(trailingOnly = TRUE)

source_2013 <- sprintf(LEGACY_BMF_URL_TEMPLATE, "2013-07")
source_2017 <- sprintf(LEGACY_BMF_URL_TEMPLATE, "2017-12")

if (length(command_arguments) >= 1) {
  source_2013 <- command_arguments[[1]]
}

if (length(command_arguments) >= 2) {
  source_2017 <- command_arguments[[2]]
}

# ============================================================================
# 2. Small worked examples
# ============================================================================

example_codes <- c(
  "B11", "E112", "Y42", "Y115", "T22", "T40", "T50", "T033", "T19", "T193",
  "P20", " b11 ", "B1170", "S0241", "", "  ", NA
)
expected_through_2016 <- c(
  "S", "S", "M", "S", "S", "O", "O", "O", "O", "O",
  "O", "S", "S", "O", NA, NA, NA
)

stopifnot(identical(
  derive_nccs_level_2(example_codes, definition = "through_2016"),
  expected_through_2016
))

stopifnot(identical(
  derive_nccs_level_2(
    ntee_code = c("P20", "P20", "P20", "P20", NA, NA, "Y42", "P20"),
    subsection_code = c("03", "3", 3, "3.0", 3, 3, 3, 4),
    foundation_code = c("04", "17", 15, "4.0", 17, 15, 4, 4)
  ),
  c("S", "S", "O", "S", "S", NA, "S", "O")
))

stopifnot(identical(
  derive_nccs_level_2(character(0), definition = "through_2016"),
  character(0)
))

stopifnot(identical(
  label_nccs_level_2(c("O", "S", "M", "X", NA)),
  c("Operating", "Supporting", "Mutual benefit", NA, NA)
))

# ============================================================================
# 3. Comparison with the original column
# ============================================================================

#' Compare the rebuilt grouping with the original column in one older BMF file
#'
#' @description
#' Reads one older BMF file, rebuilds the grouping with
#' [derive_nccs_level_2()] and reports how many organizations match the
#' original `LEVEL2` column.
#'
#' @details
#' Prints a table of organizations and matches by `LEVEL1` (public charity,
#' private foundation, other). Stops with an error if the file has no rows or
#' if the share of matching rows is below `minimum_match_share`. A row whose
#' original value is missing counts as not matching.
#'
#' @param legacy_bmf_source path or URL of the older BMF file.
#' @param definition definition passed to [derive_nccs_level_2()].
#' @param minimum_match_share lowest acceptable share of matching rows.
#'
#' @return the share of matching rows, invisibly.
compare_with_original_level_2 <- function(legacy_bmf_source,
                                          definition,
                                          minimum_match_share) {
  legacy_bmf <- data.table::fread(
    legacy_bmf_source,
    select = c("NTEECC", "LEVEL1", "LEVEL2", "SUBSECCD", "FNDNCD"),
    colClasses = "character",
    na.strings = c("", "NA")
  )

  if (nrow(legacy_bmf) == 0) {
    stop("No rows read from ", legacy_bmf_source)
  }

  comparison <- legacy_bmf |>
    dplyr::as_tibble() |>
    dplyr::mutate(
      rebuilt_level_2 = derive_nccs_level_2(
        ntee_code = NTEECC,
        subsection_code = SUBSECCD,
        foundation_code = FNDNCD,
        definition = definition
      ),
      is_match = dplyr::coalesce(rebuilt_level_2 == LEVEL2, FALSE)
    )

  match_summary <- comparison |>
    dplyr::group_by(LEVEL1) |>
    dplyr::summarise(
      organizations = dplyr::n(),
      matching = sum(is_match),
      .groups = "drop"
    )

  match_share <- sum(match_summary$matching) / sum(match_summary$organizations)

  cat(sprintf("\n%s, definition \"%s\"\n", basename(legacy_bmf_source), definition))
  print(match_summary)
  cat(sprintf(
    "Rebuilt value matches the original for %s of %s organizations (%.4f%%)\n",
    format(sum(match_summary$matching), big.mark = ","),
    format(sum(match_summary$organizations), big.mark = ","),
    100 * match_share
  ))

  if (match_share < minimum_match_share) {
    stop(
      "derive_nccs_level_2() no longer reproduces the original LEVEL2 column in ",
      basename(legacy_bmf_source)
    )
  }

  invisible(match_share)
}

compare_with_original_level_2(
  legacy_bmf_source = source_2013,
  definition = "through_2016",
  minimum_match_share = MINIMUM_MATCH_SHARE
)

compare_with_original_level_2(
  legacy_bmf_source = source_2017,
  definition = "2017_onward",
  minimum_match_share = MINIMUM_MATCH_SHARE
)
