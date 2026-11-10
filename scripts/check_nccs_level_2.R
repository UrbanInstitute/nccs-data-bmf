# ============================================================================
# check_nccs_level_2.R
# Compare derive_nccs_level_2() with the original LEVEL2 column in the
# July 2013 BMF, the last file that still carries it.
#
# Usage (from the repo root):
#   Rscript scripts/check_nccs_level_2.R [path-to-local-copy-of-the-2013-file]
#
# With no path, the file (about 335 MB) is read straight from S3 over HTTPS.
# The script stops with an error if fewer than 99.99% of rows match.
# ============================================================================

source("R/nccs_level_2.R")

# ============================================================================
# Settings
# ============================================================================

LEGACY_BMF_URL <- paste0(
  "https://nccsdata.s3.amazonaws.com/legacy/bmf/",
  "BMF-2013-07-501CX-NONPROFIT-PX.csv"
)
MINIMUM_MATCH_SHARE <- 0.9999

command_arguments <- commandArgs(trailingOnly = TRUE)
legacy_bmf_source <- if (length(command_arguments) >= 1) {
  command_arguments[[1]]
} else {
  LEGACY_BMF_URL
}

# ============================================================================
# Small worked examples
# ============================================================================

example_codes <- c("B11", "E112", "Y42", "Y115", "T22", "T40", "T033", "T19", "P20", " b11 ", "", NA)
expected_values <- c("S", "S", "M", "S", "S", "O", "O", "O", "O", "S", NA, NA)

stopifnot(identical(derive_nccs_level_2(example_codes), expected_values))
stopifnot(identical(
  label_nccs_level_2(c("O", "S", "M", NA)),
  c("Operating", "Supporting", "Mutual benefit", NA)
))

# ============================================================================
# Comparison with the original column
# ============================================================================

legacy_bmf <- data.table::fread(
  legacy_bmf_source,
  select = c("NTEECC", "LEVEL1", "LEVEL2"),
  colClasses = "character",
  na.strings = c("", "NA")
)

comparison <- legacy_bmf |>
  dplyr::as_tibble() |>
  dplyr::mutate(
    rebuilt_level_2 = derive_nccs_level_2(NTEECC),
    is_match = !is.na(rebuilt_level_2) & rebuilt_level_2 == LEVEL2
  )

match_summary <- comparison |>
  dplyr::group_by(LEVEL1) |>
  dplyr::summarise(
    organizations = dplyr::n(),
    matching = sum(is_match),
    .groups = "drop"
  )

print(match_summary)

match_share <- sum(match_summary$matching) / sum(match_summary$organizations)

cat(sprintf(
  "Rebuilt value matches the original for %s of %s organizations (%.4f%%)\n",
  format(sum(match_summary$matching), big.mark = ","),
  format(sum(match_summary$organizations), big.mark = ","),
  100 * match_share
))

if (match_share < MINIMUM_MATCH_SHARE) {
  stop("derive_nccs_level_2() no longer reproduces the original LEVEL2 column.")
}
