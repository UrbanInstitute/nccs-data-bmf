# Z18 acceptance check: compare ONE processed monthly file before and after the
# lookup fix that added 12 NTEE codes. Run for every legacy and current file.
#
# Usage:
#   Rscript scripts/check_ntee_gap_vintage_diff.R BEFORE.csv AFTER.csv [label]
# Emits one pipe-delimited line:
#   label|rows_before|rows_after|ein_set_identical|schema_identical|
#   rows_changed|rows_changed_outside_gap_codes|gap_rows_before_z99|
#   gap_rows_after_z99|cols_differing_outside_allowed
# Exits non-zero when: row count or EIN set differ, the schema differs, any
# column outside the NTEE-derived set differs, any changed row does not carry
# one of the 12 codes, or a gap-code row still publishes as Z99.

suppressMessages(library(data.table))

GAP_CODES <- c("B29", "E6A", "F31", "K2A", "K2B", "K2C",
               "L4A", "L4B", "M99", "P76", "P7A", "P83")
# The only columns the lookup fix may change.
ALLOWED <- c("ntee_code_clean", "ntee_code_definition", "naics_code",
             "nteev2", "nteev2_code", "nteev2_subsector",
             "nteev2_subsector_definition", "nteev2_org_type")

args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) >= 2)
label <- if (length(args) >= 3) args[[3]] else basename(args[[1]])
rd <- function(p) fread(p, colClasses = "character", showProgress = FALSE)
b <- rd(args[[1]]); a <- rd(args[[2]])

schema_ok <- setequal(names(b), names(a))
if (!schema_ok) {
  cat(sprintf("%s|SCHEMA-DRIFT|missing_after=%s|added_after=%s\n", label,
              paste(setdiff(names(b), names(a)), collapse = ","),
              paste(setdiff(names(a), names(b)), collapse = ",")))
  quit(status = 1)
}
if (anyDuplicated(b$ein) || anyDuplicated(a$ein)) {
  # Align on row order instead: both files are written in the same order.
  b[, .row := .I]; a[, .row := .I]; key_col <- ".row"
} else key_col <- "ein"
ein_ok <- identical(sort(b$ein), sort(a$ein))
setkeyv(b, key_col); setkeyv(a, key_col)

same_col <- function(cl) identical(b[[cl]], a[[cl]])
diff_outside <- Filter(Negate(same_col), setdiff(names(b), c(ALLOWED, ".row")))

changed_any <- Reduce(`|`, lapply(intersect(ALLOWED, names(b)),
                                  function(cl) b[[cl]] != a[[cl]]), FALSE)
changed_any[is.na(changed_any)] <- FALSE
raw3 <- substr(toupper(trimws(a$ntee_code_raw)), 1, 3)
is_gap <- raw3 %chin% GAP_CODES
changed_outside_gap <- sum(changed_any & !is_gap)
gap_before_z99 <- sum(is_gap & b$nteev2_code == "Z99", na.rm = TRUE)
gap_after_z99  <- sum(is_gap & a$nteev2_code == "Z99", na.rm = TRUE)

cat(sprintf("%s|%d|%d|%s|%s|%d|%d|%d|%d|%s\n", label, nrow(b), nrow(a), ein_ok,
            schema_ok, sum(changed_any), changed_outside_gap,
            gap_before_z99, gap_after_z99, paste(diff_outside, collapse = ",")))
ok <- ein_ok && nrow(b) == nrow(a) && length(diff_outside) == 0 &&
      changed_outside_gap == 0 && gap_after_z99 == 0
if (!ok) quit(status = 1)
