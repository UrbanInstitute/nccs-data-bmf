# ============================================================================
# check_ntee_legacy_coverage.R
#
# Diagnostic helper: compute crosswalk coverage of the pre-2003 5-char NTEE
# codes that appear in legacy BMF checkpoints. Run BEFORE wiring the
# crosswalk into transform_ntee_code() to confirm the lookup is worth it.
#
# Usage (from project root, on EC2 or locally):
#   Rscript scripts/check_ntee_legacy_coverage.R
#
# Reads any data/checkpoints/bmf_*_03_classification.parquet that exists,
# extracts distinct NTEE_CD values per vintage, joins against the
# vendored crosswalk, and prints:
#   - total distinct 5-char codes
#   - matched vs unmatched count + pct
#   - top 20 unmatched codes by row-share
# ============================================================================
suppressPackageStartupMessages({
  library(arrow)
  library(data.table)
})

xw_path <- "data/lookup/ntee_legacy_5char_lookup.csv"
if (!file.exists(xw_path)) {
  stop("Crosswalk not found at: ", xw_path)
}
xw <- fread(xw_path)
xw_5char <- xw[nchar(NTEE) == 5, .(NTEE_5char = toupper(trimws(NTEE)),
                                   nteev2     = NTEE2)]
data.table::setkey(xw_5char, NTEE_5char)

ckpt_dir <- "data/checkpoints"
ckpts <- list.files(ckpt_dir,
                    pattern = "^bmf_\\d{4}_\\d{2}_03_classification\\.parquet$",
                    full.names = TRUE)

if (length(ckpts) == 0) {
  stop("No bmf_YYYY_MM_03_classification.parquet checkpoints found in ",
       ckpt_dir)
}

cat(sprintf("Found %d checkpoint(s):\n", length(ckpts)))
for (p in ckpts) cat("  ", basename(p), "\n")
cat("\n")

# Aggregate distinct 5-char codes across all checkpoints, weighted by row count.
all_codes <- list()
for (p in ckpts) {
  vintage <- regmatches(basename(p), regexpr("\\d{4}_\\d{2}", basename(p)))
  dt <- as.data.table(read_parquet(p, col_select = "NTEE_CD"))
  dt[, NTEE_CD := toupper(trimws(as.character(NTEE_CD)))]
  five <- dt[nchar(NTEE_CD) == 5, .N, by = NTEE_CD]
  if (nrow(five) > 0) {
    five[, vintage := vintage]
    all_codes[[length(all_codes) + 1]] <- five
  }
}

if (length(all_codes) == 0) {
  cat("No 5-char NTEE codes found in any checkpoint. Crosswalk integration not needed.\n")
  quit(status = 0)
}

agg <- rbindlist(all_codes)
total_rows <- agg[, sum(N)]
distinct_codes <- agg[, uniqueN(NTEE_CD)]

# Join against crosswalk (case-insensitive — both already uppercased).
agg[, matched := NTEE_CD %in% xw_5char$NTEE_5char]

# Code-level coverage (each distinct 5-char code counts once).
code_match_pct <- 100 * agg[, uniqueN(NTEE_CD[matched])] / distinct_codes
# Row-level coverage (weighted by how often each code appears in raw data).
row_match_pct  <- 100 * agg[matched == TRUE, sum(N)] / total_rows

cat(sprintf("Distinct 5-char codes across all checkpoints: %d\n", distinct_codes))
cat(sprintf("Total rows with a 5-char code:               %s\n",
            format(total_rows, big.mark = ",")))
cat(sprintf("Code-level coverage (distinct codes matched): %.2f %%\n",
            code_match_pct))
cat(sprintf("Row-level coverage (rows where code matched): %.2f %%\n",
            row_match_pct))
cat("\n")

# Top 20 UNMATCHED codes by row-share — these are the gaps the crosswalk doesn't cover.
unmatched <- agg[matched == FALSE,
                 .(N = sum(N), n_vintages = uniqueN(vintage)),
                 by = NTEE_CD][order(-N)]
cat("Top 20 UNMATCHED 5-char codes by row count:\n")
print(head(unmatched, 20))
cat("\n")

# Per-vintage breakdown.
per_vintage <- agg[, .(
  n_rows        = sum(N),
  n_distinct    = uniqueN(NTEE_CD),
  n_matched_rows = sum(N[matched]),
  pct_rows_matched = round(100 * sum(N[matched]) / sum(N), 2)
), by = vintage][order(vintage)]
cat("Per-vintage row coverage:\n")
print(per_vintage)
