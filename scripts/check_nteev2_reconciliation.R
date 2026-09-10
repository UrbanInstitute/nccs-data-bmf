# ADR 0048 acceptance criterion D, stage 1: class-based reconciliation on a
# Unified BMF artifact (round-2 review R2-B2/R2-B3 definition).
#
# Usage:
#   Rscript scripts/check_nteev2_reconciliation.R [artifact.parquet] [--out path.csv]
# Default artifact: data/master/bmf_unified.parquet. NOTHING is written unless
# --out is given (a verification run must not dirty the worktree; R2 finding).
#
# Definitions (all computed from ntee_code_raw via the CURRENT cleaner, never
# from the artifact's stored ntee_code_clean, which is itself stale on
# pre-ADR-0032 rows, e.g. raw B112 stored clean B20):
#   new_code   full transform_ntee_code() WITH the x00 rule (this branch)
#   old_code   same cleaning, pre-0048 derivation (identity on clean; legacy
#              5-char: crosswalk NTEE2 middle slot, else letter+pos4-5, no x00)
#   flagged    artifact nteev2_code matches the specialty pattern
#   stale      artifact nteev2_code != old_code   (pre-existing drift,
#              independent of 0048: pre-0032 vintages etc.)
#   x00_moved  old_code != new_code               (rows the x00 rule moves)
#   changed    artifact nteev2_code != new_code   (what the reprocess changes)
#   cancelled  artifact == new_code but in stale-or-x00 (drift that the fix's
#              value coincides with; explained, reported, not an error)
# Contract asserted:  stale UNION x00_moved  ==  changed UNION cancelled,
# with zero rows outside the classes (false positives / negatives = 0), and
# flagged is a subset of changed.

suppressMessages({library(data.table); library(duckdb)})
`%||%` <- function(a, b) if (is.null(a)) b else a   # defined BEFORE any use
source("R/config.R")
source("R/transform_ntee_code.R")

args <- commandArgs(trailingOnly = TRUE)
out_path <- if (length(w <- which(args == "--out"))) args[w + 1] else NULL
args <- setdiff(args, c("--out", out_path))
path <- if (length(args) >= 1) args[[1]] else "data/master/bmf_unified.parquet"
stopifnot(file.exists(path))
man_path <- file.path(dirname(path), "_manifest.json")
if (file.exists(man_path)) {
  man <- jsonlite::fromJSON(man_path)
  message(sprintf("Artifact: %s (vintage %s, git_sha %s)",
                  path, man$vintage %||% "?", man$git_sha %||% "?"))
}

con <- dbConnect(duckdb(shared_home = FALSE))
d <- setDT(dbGetQuery(con, sprintf(
  "select ein, ntee_code_raw, nteev2_code, bmf_source, last_vintage_ym
   from read_parquet(%s)", shQuote(path.expand(path)))))
dbDisconnect(con, shutdown = TRUE)

xw <- lookup_ls$ntee_legacy_5char

recompute <- function(rows, legacy) {
  inp <- data.table(ein = rows$ein, NTEE_CD = rows$ntee_code_raw)
  out <- transform_ntee_code(inp, year = format(Sys.Date(), "%Y"),
                             path = NULL, write_scd = FALSE, legacy_mode = legacy)
  rows[, `:=`(new_code = out$nteev2_code, clean_re = out$ntee_code_clean)]
  rows
}
d <- rbind(recompute(d[bmf_source == "legacy"],  TRUE),
           recompute(d[bmf_source != "legacy"], FALSE))

# Pre-0048 derivation on the RECOMPUTED clean code
d[, old_code := fifelse(clean_re %chin% c("INVALID", "UNDEFINED"), "Z99", clean_re)]
if (!is.null(xw)) {
  is5 <- d[, bmf_source == "legacy" & nchar(ntee_code_raw) == 5]
  m <- xw[d[is5], on = c(NTEE = "ntee_code_raw")]
  d[is5, old_code := fifelse(!is.na(m$NTEE2),
                             tstrsplit(m$NTEE2, "-", fixed = TRUE)[[2]],
                             paste0(substr(ntee_code_raw, 1, 1), substr(ntee_code_raw, 4, 5)))]
}

d[, `:=`(flagged   = grepl(NTEEV2_SPECIALTY_PATTERN, nteev2_code),
         stale     = nteev2_code != old_code,
         x00_moved = old_code != new_code,
         changed   = nteev2_code != new_code)]
d[, in_union  := stale | x00_moved]
d[, cancelled := in_union & !changed]

n <- nrow(d)
cnt <- function(col) sum(d[[col]], na.rm = TRUE)
cat(sprintf(paste0(
  "rows %s\nflagged   %s (%.2f%%)\nstale     %s\nx00_moved %s\n",
  "changed   %s\ncancelled %s (drift coinciding with the corrected value)\n"),
  format(n, big.mark = ","), format(cnt("flagged"), big.mark = ","), 100 * cnt("flagged") / n,
  format(cnt("stale"), big.mark = ","), format(cnt("x00_moved"), big.mark = ","),
  format(cnt("changed"), big.mark = ","), format(cnt("cancelled"), big.mark = ",")))

fp <- d[changed & !in_union, .N]          # changed but in no defect class
fn <- d[flagged & !changed, .N]           # flagged-defective yet untouched
cat(sprintf("false positives (changed outside classes): %d\nfalse negatives (flagged but unchanged): %d\n", fp, fn))
ok <- fp == 0L && fn == 0L
cat(sprintf("set equation stale+x00 == changed+cancelled: %s\n",
            if (ok) "HOLDS" else "VIOLATED — investigate before publication"))

tab <- d[changed == TRUE, .N, by = .(bmf_source, last_vintage_ym)][order(bmf_source, last_vintage_ym)]
cat("\nchanged rows by Unified-BMF winning source/vintage (context only — the\nauthoritative per-processed-vintage check is scripts/check_nteev2_vintage_diff.R):\n")
print(tab, nrows = 200)
if (!is.null(out_path)) { fwrite(tab, out_path); cat("written:", out_path, "\n") }
if (!ok) quit(status = 1)
