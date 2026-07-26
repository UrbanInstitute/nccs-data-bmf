# ============================================================================
# validate_legacy_republish.R
#
# Source-vs-output validation gate for the ADR 0041 legacy street re-publish
# (standing policy per ADR 0042: no accidental NA coercions, row loss, or
# silent recodes in any re-published dataset).
#
# The nccsdata bucket has no object versioning, so prior published objects
# are unrecoverable; this gate therefore validates each re-published vintage
# against its RAW source file (the stronger standard):
#
#   V1 rows      : processed row count == raw data row count
#   V2 columns   : processed column set == the deterministic expectation
#                  derived from the raw header via the crosswalk +
#                  RAW_TO_OUTPUT_MAP (+ derived columns when all backing
#                  inputs are populated)
#   V3 street    : org_addr_street_raw non-null count == raw ADDRESS
#                  non-empty count (exact)
#   V4 coercion  : for every vintage, raw non-empty counts == processed
#                  *_raw non-null counts for CITY, STATE, ZIP, NAME
#
# Run on the batch box from the repo root after run_all_legacy.sh:
#   Rscript scripts/validate_legacy_republish.R
# Writes logs/legacy/validation_gate.tsv and exits nonzero on any failure.
# ============================================================================

suppressPackageStartupMessages(library(data.table))
library(here)
source(here::here("R", "utils", "logging.R"))
source(here::here("R", "legacy_bmf_adapter.R"))

RAW_DIR  <- here::here("data", "raw", "legacy")
PROC_DIR <- here::here("data", "processed")
OUT_TSV  <- here::here("logs", "legacy", "validation_gate.tsv")

xw <- load_crosswalk_v2()

raw_files <- list.files(RAW_DIR, pattern = "^BMF-\\d{4}-\\d{2}-501CX", full.names = TRUE)
if (length(raw_files) == 0L) stop("No raw legacy files found under ", RAW_DIR)

# value-level checks: raw source column -> processed raw-passthrough column
VAL_MAP <- c(CITY = "org_addr_city_raw", STATE = "org_addr_state_raw",
             ZIP5 = "org_addr_zip_raw",  NAME  = "org_name_raw",
             ADDRESS = "org_addr_street_raw")

results <- list()
for (rf in raw_files) {
  vym  <- regmatches(basename(rf), regexpr("\\d{4}-\\d{2}", basename(rf)))
  vtag <- gsub("-", "_", vym)
  proc_csv <- file.path(PROC_DIR, sprintf("bmf_legacy_%s_processed.csv", vtag))
  dict_csv <- file.path(PROC_DIR, sprintf("bmf_legacy_%s_data_dictionary.csv", vtag))
  if (!file.exists(proc_csv)) next   # vintage not part of this batch

  raw  <- fread(rf, colClasses = "character", showProgress = FALSE)
  dict <- fread(dict_csv)

  # V2: deterministic column expectation from the raw header
  upper <- toupper(names(raw))
  renames <- xw[disposition == "rename" & legacy_name_upper %in% upper]
  populated <- unique(renames$current_name)
  expected <- compute_legacy_output_columns(populated)
  actual <- dict$column_name
  miss  <- setdiff(expected, actual)
  extra <- setdiff(actual, expected)

  # read only the processed columns needed for value checks
  need <- intersect(unname(VAL_MAP), actual)
  proc <- fread(proc_csv, select = need, colClasses = "character", showProgress = FALSE)

  # V1: row parity
  rows_ok <- nrow(proc) == nrow(raw)

  # V3 + V4: per-column non-empty parity (raw) vs non-null (processed)
  val_fail <- character(0)
  for (src in names(VAL_MAP)) {
    dst <- VAL_MAP[[src]]
    if (!src %in% names(raw) || !dst %in% names(proc)) next
    n_raw  <- sum(!is.na(raw[[src]]) & trimws(raw[[src]]) != "")
    n_proc <- sum(!is.na(proc[[dst]]) & proc[[dst]] != "")
    if (n_raw != n_proc) val_fail <- c(val_fail, sprintf("%s:%d!=%d", src, n_raw, n_proc))
  }

  street_expected <- "ADDRESS" %in% names(raw)
  street_present  <- "org_addr_street_raw" %in% actual

  ok <- rows_ok && length(miss) == 0L && length(extra) == 0L &&
        length(val_fail) == 0L && (street_expected == street_present)

  results[[vtag]] <- data.table(
    vintage = vtag, rows_raw = nrow(raw), rows_proc = nrow(proc),
    rows_ok = rows_ok, cols_missing = paste(miss, collapse = ";"),
    cols_extra = paste(extra, collapse = ";"),
    street_expected = street_expected, street_present = street_present,
    value_mismatches = paste(val_fail, collapse = ";"),
    passed = ok
  )
  log_info(sprintf("%s: %s (rows %d/%d, value checks %s)",
                   vtag, ifelse(ok, "PASS", "FAIL"), nrow(proc), nrow(raw),
                   ifelse(length(val_fail), paste(val_fail, collapse = ","), "clean")))
  rm(raw, proc); gc(verbose = FALSE)
}

res <- rbindlist(results)
dir.create(dirname(OUT_TSV), recursive = TRUE, showWarnings = FALSE)
fwrite(res, OUT_TSV, sep = "\t")
n_fail <- res[passed == FALSE, .N]
log_info(sprintf("Validation gate: %d vintages checked, %d failed. TSV: %s",
                 nrow(res), n_fail, OUT_TSV))
if (n_fail > 0L) quit(status = 1L)
