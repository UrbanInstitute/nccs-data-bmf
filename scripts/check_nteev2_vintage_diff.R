# ADR 0048 acceptance criterion D, stage 2 (round-2 review R2-B4): compare ONE
# processed vintage before/after reprocessing. Run for every legacy (~85),
# historical current-monthly (~37), and current file; collect the one-line
# outputs into the ADR outcome table.
#
# Usage:
#   Rscript scripts/check_nteev2_vintage_diff.R BEFORE.csv AFTER.csv [label]
# Emits one pipe-delimited line:
#   label|rows_before|rows_after|ein_set_identical|flagged_before|changed|
#   flagged_after|other_cols_identical|cols_differing
# and exits non-zero if row count or EIN set differ, a specialty code survives
# in AFTER, or any column other than nteev2_code / nteev2 differs.

suppressMessages({library(data.table)})
source("R/transform_ntee_code.R")   # NTEEV2_SPECIALTY_PATTERN

args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) >= 2)
label <- if (length(args) >= 3) args[[3]] else basename(args[[1]])
rd <- function(p) if (grepl("[.]parquet$", p)) setDT(arrow::read_parquet(p)) else fread(p)
b <- rd(args[[1]]); a <- rd(args[[2]])

ein_ok  <- identical(sort(b$ein), sort(a$ein))
setkey(b, ein); setkey(a, ein)
common <- intersect(names(b), names(a))
allowed <- c("nteev2_code", "nteev2")
diff_cols <- Filter(function(cl) !identical(b[[cl]], a[[cl]]), setdiff(common, allowed))
flag_b <- sum(grepl(NTEEV2_SPECIALTY_PATTERN, b$nteev2_code), na.rm = TRUE)
flag_a <- sum(grepl(NTEEV2_SPECIALTY_PATTERN, a$nteev2_code), na.rm = TRUE)
changed <- sum(b$nteev2_code != a$nteev2_code |
               (is.na(b$nteev2_code) != is.na(a$nteev2_code)), na.rm = TRUE)

cat(sprintf("%s|%d|%d|%s|%d|%d|%d|%s|%s\n", label, nrow(b), nrow(a), ein_ok,
            flag_b, changed, flag_a, length(diff_cols) == 0,
            paste(diff_cols, collapse = ",")))
if (!ein_ok || nrow(b) != nrow(a) || flag_a > 0 || length(diff_cols) > 0) quit(status = 1)
