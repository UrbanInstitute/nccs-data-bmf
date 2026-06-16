# ============================================================================
# check_ntee_university_coverage.R
#
# Regression guard for the NTEE cleaner / nteev2 fix (ADR 0032). Confirms that
# valid university (B40-B43, B50) and hospital (E20-E24) NTEE codes survive
# cleaning and reach the UNI / HOS subsectors instead of being mangled to
# INVALID and collapsing into EDU / HEL.
#
# Background: a 4-char raw code is [letter][2-digit subgroup][trailing modifier].
# The old digit-modifier branch did paste0(.first, .last, "0"), discarding the
# subgroup ("B430" -> "B00" -> INVALID), and the subsector keyed off a parallel
# nteev2_code formula that could never emit B41/B42/B43 -- so no real university
# ever reached "UNI". See R/transform_ntee_code.R and nccs-contracts ADR 0032.
#
# Usage (from project root):
#   Rscript scripts/check_ntee_university_coverage.R
#
# Exits non-zero if any assertion fails.
# ============================================================================
suppressPackageStartupMessages({
  library(data.table)
  library(openxlsx)
})

wb <- "data/lookup/bmf_code_lookup.xlsx"
if (!file.exists(wb)) stop("Lookup workbook not found at: ", wb)

lookup_ls <- list(
  ntee_code             = as.data.table(read.xlsx(wb, sheet = "ntee_code")),
  ntee_code_major_group = as.data.table(read.xlsx(wb, sheet = "ntee_code_major_group")),
  ntee_common_code      = as.data.table(read.xlsx(wb, sheet = "ntee_common_code")),
  nteev2_subsector      = as.data.table(read.xlsx(wb, sheet = "nteev2_subsector"))
)
PROCESSING_YEAR <- "2025"
source("R/transform_ntee_code.R")

# Canonical fixtures: raw NTEE_CD -> expected (clean, subsector, composite).
# Covers 4-char trailing-zero, 3-char, 4-char trailing-letter, hospital, and
# the genuine-upstream-gap (null) case.
# "QZZ" guards the INVALID->UNU policy (ADR 0032 Decision 4): it is unvalidated
# (not in the lookup) but its first letter "Q" would map to IFA under the old
# first-letter fall-through. The null row likewise must land UNU, not a sector.
fixtures <- data.table(
  ein      = sprintf("00-%07d", seq_len(8)),
  NTEE_CD  = c("B430",  "B43",  "B420",  "B500",  "E220",  "B05",  "QZZ",     NA_character_),
  exp_clean= c("B43",   "B43",  "B42",   "B50",   "E22",   "B05",  "INVALID", "INVALID"),
  exp_sub  = c("UNI",   "UNI",  "UNI",   "UNI",   "HOS",   "EDU",  "UNU",     "UNU")
)

out <- suppressWarnings(suppressMessages(
  transform_ntee_code(fixtures[, .(ein, NTEE_CD)], input_ntee_col = "NTEE_CD",
                      year = "2025", write_scd = FALSE)
))
res <- merge(fixtures, out[, .(ein, ntee_code_raw, ntee_code_clean, nteev2_subsector, nteev2)],
             by = "ein", sort = FALSE)
res[, ok := ntee_code_clean == exp_clean & nteev2_subsector == exp_sub]

cat("--- NTEE university/hospital coverage check (ADR 0032) ---\n")
print(res[, .(NTEE_CD, ntee_code_clean, exp_clean, nteev2_subsector, exp_sub, nteev2, ok)])

failures <- res[ok == FALSE]
if (nrow(failures) > 0) {
  stop(sprintf("FAIL: %d fixture(s) did not match expected clean/subsector.",
               nrow(failures)))
}

# Invariant: the university/hospital subsector constants must be reachable,
# i.e. every code in them is a valid lookup code (a clean 3-char code can equal
# it). Guards against a future edit that keys subsector off a derived value.
valid <- lookup_ls$ntee_code$ntee_code
unreachable <- setdiff(c(NTEEV2_SUBSECTOR_UNIVERSITY, NTEEV2_SUBSECTOR_HOSPITAL), valid)
if (length(unreachable) > 0) {
  stop("FAIL: subsector codes absent from ntee_code lookup (unreachable): ",
       paste(unreachable, collapse = ", "))
}

cat("\nPASS: all fixtures matched; UNI/HOS code sets are reachable.\n")
