# ADR 0048 acceptance criterion D: reconcile flagged-defective rows against
# rows the fix actually changes, on a Unified BMF artifact.
#
# Usage:
#   Rscript scripts/check_nteev2_reconciliation.R [path/to/bmf_unified.parquet]
# Default path: data/master/bmf_unified.parquet (next to its _manifest.json).
#
# Method (per the ADR): recompute via the FULL transform_ntee_code() on
# ntee_code_raw with legacy_mode per row source — NOT via the helper on
# ntee_code_clean, which is the wrong oracle for 5-char legacy rows resolved
# by the vendored crosswalk. Reports:
#   flagged   rows matching NTEEV2_SPECIALTY_PATTERN in the artifact
#   changed   rows whose recomputed nteev2_code differs from the artifact
#   delta     changed - flagged, split into explained classes
# A non-zero delta is a finding to investigate before publication.

suppressMessages({library(data.table); library(duckdb)})
source("R/config.R")             # lookup_ls (workbook + legacy crosswalk)
source("R/transform_ntee_code.R")

args <- commandArgs(trailingOnly = TRUE)
path <- if (length(args) >= 1) args[[1]] else "data/master/bmf_unified.parquet"
stopifnot(file.exists(path))
man_path <- file.path(dirname(path), "_manifest.json")
if (file.exists(man_path)) {
  man <- jsonlite::fromJSON(man_path)
  message(sprintf("Artifact: %s (vintage %s, git_sha %s)",
                  path, man$vintage %||% "?", man$git_sha %||% "?"))
}
`%||%` <- function(a, b) if (is.null(a)) b else a

con <- dbConnect(duckdb(shared_home = FALSE))
d <- setDT(dbGetQuery(con, sprintf(
  "select ein, ntee_code_raw, ntee_code_clean, nteev2_code, nteev2,
          nteev2_subsector, nteev2_org_type, bmf_source, last_vintage_ym
   from read_parquet(%s)", shQuote(path.expand(path)))))
dbDisconnect(con, shutdown = TRUE)
n <- nrow(d)

flagged <- d[grepl(NTEEV2_SPECIALTY_PATTERN, nteev2_code)]

# Recompute through the full transform, split by source (legacy rows may
# carry pre-2003 5-char codes; legacy_mode enables the vendored crosswalk).
recompute <- function(rows, legacy) {
  inp <- data.table(ein = rows$ein, NTEE_CD = rows$ntee_code_raw)
  out <- transform_ntee_code(inp, year = format(Sys.Date(), "%Y"),
                             path = NULL, write_scd = FALSE,
                             legacy_mode = legacy)
  rows[, new_code := out$nteev2_code]
  rows
}
d <- rbind(recompute(d[bmf_source == "legacy"],  TRUE),
           recompute(d[bmf_source != "legacy"], FALSE))

changed <- d[new_code != nteev2_code | (is.na(new_code) != is.na(nteev2_code))]
delta <- nrow(changed) - nrow(flagged)

cat(sprintf("rows %s | flagged %s (%.2f%%) | changed %s | delta %+d\n",
            format(n, big.mark = ","), format(nrow(flagged), big.mark = ","),
            100 * nrow(flagged) / n, format(nrow(changed), big.mark = ","), delta))
cat("flagged-but-unchanged:", nrow(fsetdiff(flagged[, .(ein)], changed[, .(ein)])), "\n")
extra <- changed[!ein %in% flagged$ein]
cat("changed-but-unflagged:", nrow(extra), "(pre-0048 stale values, ADR 0048 Decision #3a)\n")
if (nrow(extra) > 0) {
  cat("\nchanged-but-unflagged by source and last vintage (top 15):\n")
  print(extra[, .N, by = .(bmf_source, last_vintage_ym)][order(-N)][1:min(15, .N)])
}
cat("\nper-vintage changed counts (write to CSV for the ADR outcome table):\n")
tab <- changed[, .N, by = .(bmf_source, last_vintage_ym)][order(bmf_source, last_vintage_ym)]
print(tab, nrows = 200)
fwrite(tab, "data/crosswalks/nteev2_reconciliation_by_vintage.csv")
