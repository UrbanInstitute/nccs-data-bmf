# Z18 acceptance check: compare ONE processed monthly BMF file before and after
# the lookup fix that added 12 NTEE codes (PR #54). Run it for every legacy and
# current-monthly file.
#
# Usage:
#   Rscript scripts/check_ntee_gap_vintage_diff.R BEFORE.csv AFTER.csv [label]
#
# Prints one pipe-delimited result line:
#   label | rows_before | rows_after | ein_set_identical | schema_identical |
#   gap_rows | rows_changed | rows_changed_outside_gap_rows |
#   gap_rows_not_z99_before | gap_rows_wrong_after | cols_differing_outside_allowed
#
# The exit status is 0 only when ALL of these hold:
#   1. both files have every required column, and the same set of columns;
#   2. same row count and the same multiset of EINs;
#   3. no column outside the NTEE-derived set differs (missing values count);
#   4. the rows that changed are exactly the "gap rows": rows whose raw code is
#      one of the 12 restored codes. Every gap row changed, nothing else did;
#   5. every gap row was INVALID / Z99 in the BEFORE file, which proves the
#      BEFORE file really is a pre-fix file;
#   6. every NTEE-derived value on a gap row in the AFTER file equals what the
#      current transform_ntee_code() derives for that raw code, and none is
#      missing.
#
# Rows are paired by sorting both files on every column that is not allowed to
# change, so duplicate EINs pair by content rather than by position. Rows that
# are identical on all of those columns share a raw code, so any pairing among
# them gives the same verdict.
#
# The transform and the lookup workbook are read from this repo, the same way
# tests/testthat/test-ntee-v2.R does it.

# ---------------------------------------------------------------------------
# Setup
# ---------------------------------------------------------------------------

script_path <- sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE)[1])
repo_root   <- normalizePath(file.path(dirname(script_path), ".."))
# R/transform_ntee_code.R uses data.table operators such as %chin% without a
# package prefix, so the package has to be attached for it to run. Everything
# in this script itself is namespaced.
suppressMessages(library(data.table))
source(file.path(repo_root, "R", "transform_ntee_code.R"))

restored_codes <- c("B29", "E6A", "F31", "K2A", "K2B", "K2C",
                    "L4A", "L4B", "M99", "P76", "P7A", "P83")

# The only columns the lookup fix may change. ntee_code_major_group moves from
# UNDEFINED to the real major group once the code is recognised.
columns_allowed_to_change <- c(
  "ntee_code_clean", "ntee_code_definition", "ntee_code_major_group",
  "naics_code", "nteev2", "nteev2_code", "nteev2_subsector",
  "nteev2_subsector_definition", "nteev2_org_type"
)
required_columns <- c("ein", "ntee_code_raw", columns_allowed_to_change)

arguments   <- commandArgs(trailingOnly = TRUE)
stopifnot(length(arguments) >= 2)
before_path <- arguments[[1]]
after_path  <- arguments[[2]]
label       <- if (length(arguments) >= 3) arguments[[3]] else basename(before_path)
legacy_mode <- grepl("legacy", label, fixed = TRUE)

# Print one pipe-delimited failure line and exit 1, so a pre-comparison problem
# (missing columns, schema drift, row count) is still recorded in the diff log.
stop_with <- function(tag, ...) {
  cat(sprintf("%s|%s|%s\n", label, tag, paste(..., collapse = ";")))
  quit(status = 1)
}

# TRUE where two vectors differ, counting NA versus a value as a difference,
# because plain != would return NA there and hide the change.
values_differ <- function(x, y) {
  (is.na(x) != is.na(y)) | (!is.na(x) & !is.na(y) & x != y)
}

# Read a processed BMF CSV with every column as character, so the before and
# after files compare on text and not on how fread guessed each type.
read_processed_file <- function(path) {
  data.table::fread(path, colClasses = "character", showProgress = FALSE,
                    na.strings = c("", "NA"))
}

# ---------------------------------------------------------------------------
# 1. Read both files and check the schema
# ---------------------------------------------------------------------------

before_rows <- read_processed_file(before_path)
after_rows  <- read_processed_file(after_path)

missing_in_before <- setdiff(required_columns, names(before_rows))
missing_in_after  <- setdiff(required_columns, names(after_rows))
if (length(missing_in_before) > 0 || length(missing_in_after) > 0) {
  stop_with("MISSING-REQUIRED-COLUMNS",
            paste0("before=", paste(missing_in_before, collapse = ",")),
            paste0("after=",  paste(missing_in_after,  collapse = ",")))
}

if (!setequal(names(before_rows), names(after_rows))) {
  stop_with("SCHEMA-DRIFT",
            paste0("missing_after=", paste(setdiff(names(before_rows), names(after_rows)), collapse = ",")),
            paste0("added_after=",   paste(setdiff(names(after_rows), names(before_rows)), collapse = ",")))
}

if (nrow(before_rows) != nrow(after_rows)) {
  stop_with("ROW-COUNT", nrow(before_rows), nrow(after_rows))
}

ein_set_identical <- identical(sort(before_rows$ein), sort(after_rows$ein))

# ---------------------------------------------------------------------------
# 2. Pair the rows and check that nothing outside the allowed columns changed
# ---------------------------------------------------------------------------

invariant_columns <- setdiff(names(before_rows), columns_allowed_to_change)
sort_columns      <- c(invariant_columns, columns_allowed_to_change)

data.table::setorderv(before_rows, sort_columns, na.last = TRUE)
data.table::setorderv(after_rows,  sort_columns, na.last = TRUE)

# TRUE if a column has any row-level difference between the paired files;
# used to find invariant columns that changed when only NTEE columns should.
column_differs <- function(column_name) {
  any(values_differ(before_rows[[column_name]], after_rows[[column_name]]))
}
invariant_columns_that_differ <- purrr::keep(invariant_columns, column_differs)

if (length(invariant_columns_that_differ) > 0) {
  # Either an invariant column really changed, or the rows could not be paired.
  cat(sprintf("%s|%d|%d|%s|TRUE|NA|NA|NA|NA|NA|%s\n",
              label, nrow(before_rows), nrow(after_rows), ein_set_identical,
              paste(invariant_columns_that_differ, collapse = ",")))
  quit(status = 1)
}

# ---------------------------------------------------------------------------
# 3. Work out the expected NTEE values for every distinct raw code
# ---------------------------------------------------------------------------

lookup_path <- file.path(repo_root, "data", "lookup", "bmf_code_lookup.xlsx")
# Read one sheet of the BMF code lookup workbook as a data.table, so the
# expected NTEE values come from the same source the pipeline uses.
read_lookup_sheet <- function(sheet_name) {
  data.table::setDT(openxlsx::read.xlsx(lookup_path, sheet = sheet_name))
}

legacy_crosswalk <- data.table::fread(
  file.path(repo_root, "data", "lookup", "ntee_legacy_5char_lookup.csv")
)
legacy_crosswalk[, NTEE := toupper(trimws(NTEE))]
data.table::setkey(legacy_crosswalk, NTEE)

distinct_raw_codes <- unique(after_rows$ntee_code_raw)
transform_input    <- data.table::data.table(
  ein     = seq_along(distinct_raw_codes),
  NTEE_CD = distinct_raw_codes
)

# Row order is preserved by the transform, so expected_by_code[i] describes
# distinct_raw_codes[i].
expected_by_code <- NULL
invisible(capture.output(
  expected_by_code <- suppressMessages(suppressWarnings(transform_ntee_code(
    transform_input,
    ntee_code_lookup         = read_lookup_sheet("ntee_code"),
    ntee_major_group_lookup  = read_lookup_sheet("ntee_code_major_group"),
    ntee_common_code_lookup  = read_lookup_sheet("ntee_common_code"),
    nteev2_subsector_lookup  = read_lookup_sheet("nteev2_subsector"),
    ntee_legacy_5char_lookup = legacy_crosswalk,
    year = "0000", path = NULL, write_scd = FALSE, legacy_mode = legacy_mode
  )))
))

# For each row of the AFTER file, which distinct raw code does it carry?
code_index_per_row <- match(after_rows$ntee_code_raw, distinct_raw_codes)

# ---------------------------------------------------------------------------
# 4. Classify the rows
# ---------------------------------------------------------------------------

# A gap row carries one of the 12 restored codes, judged from the RAW code via
# the transform's own cleaning, never from the possibly wrong AFTER values.
expected_clean_per_row <- expected_by_code$ntee_code_clean[code_index_per_row]
is_gap_row             <- expected_clean_per_row %in% restored_codes

# Did any allowed column change on this row?
changed_per_column <- purrr::map(columns_allowed_to_change, function(column_name) {
  values_differ(before_rows[[column_name]], after_rows[[column_name]])
})
row_changed <- purrr::reduce(changed_per_column, `|`)

# Was the row INVALID / Z99 before the fix, as every gap row must have been?
was_unknown_before <- before_rows$nteev2_code %in% "Z99" &
                      before_rows$ntee_code_clean %in% NTEE_INVALID

# ---------------------------------------------------------------------------
# 5. Compare the gap rows' AFTER values with the expected values
# ---------------------------------------------------------------------------

gap_rows_wrong_after <- 0L
if (any(is_gap_row)) {
  gap_code_index <- code_index_per_row[is_gap_row]

  wrong_per_column <- purrr::map(columns_allowed_to_change, function(column_name) {
    actual   <- after_rows[[column_name]][is_gap_row]
    expected <- as.character(expected_by_code[[column_name]][gap_code_index])
    is.na(actual) | values_differ(actual, expected)
  })
  gap_rows_wrong_after <- sum(purrr::reduce(wrong_per_column, `|`))
}

# ---------------------------------------------------------------------------
# 6. Report and decide
# ---------------------------------------------------------------------------

gap_rows                      <- sum(is_gap_row)
rows_changed                  <- sum(row_changed)
rows_changed_outside_gap_rows <- sum(row_changed & !is_gap_row)
gap_rows_not_z99_before       <- sum(is_gap_row & !was_unknown_before)
every_gap_row_changed         <- all(row_changed[is_gap_row])

cat(sprintf("%s|%d|%d|%s|TRUE|%d|%d|%d|%d|%d|\n",
            label, nrow(before_rows), nrow(after_rows), ein_set_identical,
            gap_rows, rows_changed, rows_changed_outside_gap_rows,
            gap_rows_not_z99_before, gap_rows_wrong_after))

all_checks_pass <- ein_set_identical &&
  rows_changed_outside_gap_rows == 0 &&
  every_gap_row_changed &&
  gap_rows_not_z99_before == 0 &&
  gap_rows_wrong_after == 0

if (!all_checks_pass) quit(status = 1)
