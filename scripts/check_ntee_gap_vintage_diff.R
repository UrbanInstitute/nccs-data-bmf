# Z18 acceptance check: compare ONE processed monthly file before and after the
# lookup fix that added 12 NTEE codes (PR #54). Run for every legacy and
# current file.
#
# Usage:
#   Rscript scripts/check_ntee_gap_vintage_diff.R BEFORE.csv AFTER.csv [label]
# Emits one pipe-delimited line:
#   label|rows_before|rows_after|ein_set_identical|schema_identical|gap_rows|
#   rows_changed|rows_changed_outside_gap_rows|gap_rows_not_z99_before|
#   gap_rows_wrong_after|cols_differing_outside_allowed
# Exit status is non-zero unless ALL of these hold:
#   * both files carry the full required schema, and the same schema;
#   * same row count and the same multiset of EINs;
#   * rows pair on the invariant columns (everything outside the NTEE-derived
#     set), so duplicate EINs are paired by content, not position; rows that are
#     identical on every invariant column share a raw code and pair freely;
#   * no column outside the NTEE-derived set differs (missing values count);
#   * the set of changed rows is exactly the set of gap rows (rows whose
#     corrected clean code is one of the 12), so every gap row changed and
#     nothing else did;
#   * every gap row was Z99 / INVALID before (proves BEFORE is a pre-fix file);
#   * every NTEE-derived value on a gap row after equals what the current
#     transform_ntee_code() derives for that raw code, with no missing values.
# Sources the transform and the lookup workbook from the repo, as the tests do.

suppressMessages(library(data.table))
repo_root <- normalizePath(file.path(dirname(sub("^--file=", "",
  grep("^--file=", commandArgs(), value = TRUE)[1])), ".."))
source(file.path(repo_root, "R", "transform_ntee_code.R"))

GAP_CODES <- c("B29", "E6A", "F31", "K2A", "K2B", "K2C",
               "L4A", "L4B", "M99", "P76", "P7A", "P83")
# The only columns the lookup fix may change (ntee_code_major_group moves
# from UNDEFINED to the real group once the code is recognised).
ALLOWED <- c("ntee_code_clean", "ntee_code_definition", "ntee_code_major_group",
             "naics_code", "nteev2", "nteev2_code", "nteev2_subsector",
             "nteev2_subsector_definition", "nteev2_org_type")
REQUIRED <- c("ein", "ntee_code_raw", ALLOWED)

args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) >= 2)
label <- if (length(args) >= 3) args[[3]] else basename(args[[1]])
legacy_mode <- grepl("legacy", label, fixed = TRUE)
fail <- function(tag, ...) { cat(sprintf("%s|%s|%s\n", label, tag, paste(..., collapse = ";"))); quit(status = 1) }

rd <- function(p) fread(p, colClasses = "character", showProgress = FALSE, na.strings = c("", "NA"))
b <- rd(args[[1]]); a <- rd(args[[2]])

missing_b <- setdiff(REQUIRED, names(b)); missing_a <- setdiff(REQUIRED, names(a))
if (length(missing_b) || length(missing_a))
  fail("MISSING-REQUIRED-COLUMNS", paste0("before=", paste(missing_b, collapse = ",")),
       paste0("after=", paste(missing_a, collapse = ",")))
if (!setequal(names(b), names(a)))
  fail("SCHEMA-DRIFT", paste0("missing_after=", paste(setdiff(names(b), names(a)), collapse = ",")),
       paste0("added_after=", paste(setdiff(names(a), names(b)), collapse = ",")))
if (nrow(b) != nrow(a)) fail("ROW-COUNT", nrow(b), nrow(a))
ein_ok <- identical(sort(b$ein), sort(a$ein))

# Pair rows on every invariant column (ADR 0048 practice). Row order is not an
# identity; duplicate EINs are paired by their full invariant content.
invariant <- setdiff(names(b), ALLOWED)
# Sort on the invariant columns, then on the NTEE-derived ones, so rows that
# are identical on every invariant column (for example blank-EIN rows) pair
# deterministically. Such rows share a raw code, so every check below gives
# the same verdict whichever of them is paired with which.
setorderv(b, c(invariant, ALLOWED), na.last = TRUE)
setorderv(a, c(invariant, ALLOWED), na.last = TRUE)
neq <- function(x, y) (is.na(x) != is.na(y)) | (!is.na(x) & !is.na(y) & x != y)
inv_diff <- Filter(function(cl) any(neq(b[[cl]], a[[cl]])), invariant)
if (length(inv_diff)) {
  # Either an invariant column really changed, or rows could not be paired.
  cat(sprintf("%s|%d|%d|%s|TRUE|NA|NA|NA|NA|NA|%s\n", label, nrow(b), nrow(a), ein_ok,
              paste(inv_diff, collapse = ",")))
  quit(status = 1)
}

changed <- Reduce(`|`, lapply(ALLOWED, function(cl) neq(b[[cl]], a[[cl]])))

# Expected NTEE-derived values for every distinct raw code, from the current
# transform and lookups. Gap rows are identified from the RAW code (via the
# transform's own cleaning), never from the possibly-wrong AFTER values.
lookup_path <- file.path(repo_root, "data", "lookup", "bmf_code_lookup.xlsx")
read_sheet <- function(s) setDT(openxlsx::read.xlsx(lookup_path, sheet = s))
legacy_xw <- fread(file.path(repo_root, "data", "lookup", "ntee_legacy_5char_lookup.csv"))
legacy_xw[, NTEE := toupper(trimws(NTEE))]; setkey(legacy_xw, NTEE)
uniq_raw <- unique(a$ntee_code_raw)
exp_u <- NULL
invisible(capture.output(exp_u <- suppressMessages(suppressWarnings(transform_ntee_code(
  data.table(ein = seq_along(uniq_raw), NTEE_CD = uniq_raw),
  ntee_code_lookup         = read_sheet("ntee_code"),
  ntee_major_group_lookup  = read_sheet("ntee_code_major_group"),
  ntee_common_code_lookup  = read_sheet("ntee_common_code"),
  nteev2_subsector_lookup  = read_sheet("nteev2_subsector"),
  ntee_legacy_5char_lookup = legacy_xw,
  year = "0000", path = NULL, write_scd = FALSE, legacy_mode = legacy_mode
)))))
idx    <- match(a$ntee_code_raw, uniq_raw)          # row order is preserved by the transform
is_gap <- exp_u$ntee_code_clean[idx] %chin% GAP_CODES
changed_outside_gap <- sum(changed & !is_gap)
gap_not_z99_before  <- sum(is_gap & !(b$nteev2_code %in% "Z99" & b$ntee_code_clean %in% NTEE_INVALID))
gap_wrong_after <- 0L
if (any(is_gap)) {
  wrong <- Reduce(`|`, lapply(ALLOWED, function(cl)
    is.na(a[[cl]][is_gap]) | neq(as.character(a[[cl]][is_gap]), as.character(exp_u[[cl]][idx[is_gap]]))))
  gap_wrong_after <- sum(wrong)
}

cat(sprintf("%s|%d|%d|%s|TRUE|%d|%d|%d|%d|%d|\n", label, nrow(b), nrow(a), ein_ok,
            sum(is_gap), sum(changed), changed_outside_gap, gap_not_z99_before, gap_wrong_after))
ok <- ein_ok && changed_outside_gap == 0 && all(changed[is_gap]) &&
      gap_not_z99_before == 0 && gap_wrong_after == 0
if (!ok) quit(status = 1)
