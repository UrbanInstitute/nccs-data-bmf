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
#
# Style per workspace CODE_CONVENTIONS.md: verbose names, explicit
# namespacing, purrr::map over loops. data.table::fread is retained for the
# reads (the processed CSVs run to 1.4M+ rows each; fread is well beyond the
# conventions' 1.5x performance threshold vs the tidyverse readers here).
# ============================================================================

suppressPackageStartupMessages(library(data.table))
library(here)
source(here::here("R", "utils", "logging.R"))
source(here::here("R", "legacy_bmf_adapter.R"))

raw_legacy_dir      <- here::here("data", "raw", "legacy")
processed_dir       <- here::here("data", "processed")
output_tsv_path     <- here::here("logs", "legacy", "validation_gate.tsv")

crosswalk <- load_crosswalk_v2()

raw_file_paths <- list.files(raw_legacy_dir,
                             pattern = "^BMF-\\d{4}-\\d{2}-501CX",
                             full.names = TRUE)
if (length(raw_file_paths) == 0L) {
  stop("No raw legacy files found under ", raw_legacy_dir)
}

# Value-level checks: raw source column -> processed raw-passthrough column.
value_check_map <- c(CITY = "org_addr_city_raw", STATE = "org_addr_state_raw",
                     ZIP5 = "org_addr_zip_raw",  NAME  = "org_name_raw",
                     ADDRESS = "org_addr_street_raw")

#' Count values that are neither NA nor empty/whitespace-only.
count_nonempty <- function(values) {
  sum(!is.na(values) & trimws(values) != "")
}

#' Validate one re-published vintage against its raw source file.
#' Returns a one-row data.table of check results, or NULL when the vintage
#' was not part of this batch (no processed CSV present).
validate_one_vintage <- function(raw_file_path) {
  # Extract the vintage stamp from the filename: basename() drops the
  # directory, regexpr() locates the first YYYY-MM digit pattern, and
  # regmatches() pulls that substring out (basename -> regexpr -> regmatches).
  # The list.files() pattern above guarantees exactly one match.
  vintage_year_month <- regmatches(
    basename(raw_file_path),
    regexpr("\\d{4}-\\d{2}", basename(raw_file_path))
  )
  vintage_tag <- gsub("-", "_", vintage_year_month)

  processed_csv_path  <- file.path(
    processed_dir, sprintf("bmf_legacy_%s_processed.csv", vintage_tag))
  dictionary_csv_path <- file.path(
    processed_dir, sprintf("bmf_legacy_%s_data_dictionary.csv", vintage_tag))
  if (!file.exists(processed_csv_path)) {
    return(NULL)   # vintage not part of this batch
  }

  raw_data   <- data.table::fread(raw_file_path, colClasses = "character",
                                  showProgress = FALSE)
  dictionary <- data.table::fread(dictionary_csv_path)

  # V2: deterministic column expectation. Raw header names (upper-cased) ->
  # crosswalk rename rows -> populated current-schema columns ->
  # compute_legacy_output_columns() gives the exact output set the slim
  # Phase 11 schema must produce for this vintage.
  raw_column_names_upper   <- toupper(names(raw_data))
  rename_rows              <- crosswalk[disposition == "rename" &
                                        legacy_name_upper %in% raw_column_names_upper]
  populated_current_columns <- unique(rename_rows$current_name)
  expected_output_columns   <- compute_legacy_output_columns(populated_current_columns)
  actual_output_columns     <- dictionary$column_name
  missing_columns    <- setdiff(expected_output_columns, actual_output_columns)
  unexpected_columns <- setdiff(actual_output_columns, expected_output_columns)

  # Read only the processed columns the value checks need.
  value_check_columns <- intersect(unname(value_check_map), actual_output_columns)
  processed_data <- data.table::fread(processed_csv_path,
                                      select = value_check_columns,
                                      colClasses = "character",
                                      showProgress = FALSE)

  # V1: row parity.
  rows_match <- nrow(processed_data) == nrow(raw_data)

  # V3 + V4: per-column non-empty parity, raw vs processed. Each source
  # column maps to its processed passthrough (value_check_map: raw ->
  # processed); a count mismatch means values were coerced or dropped.
  value_mismatches <- purrr::map(names(value_check_map), function(source_column) {
    processed_column <- value_check_map[[source_column]]
    if (!source_column %in% names(raw_data) ||
        !processed_column %in% names(processed_data)) {
      return(NULL)
    }
    n_nonempty_raw       <- count_nonempty(raw_data[[source_column]])
    n_nonempty_processed <- count_nonempty(processed_data[[processed_column]])
    if (n_nonempty_raw != n_nonempty_processed) {
      sprintf("%s:%d!=%d", source_column, n_nonempty_raw, n_nonempty_processed)
    } else {
      NULL
    }
  })
  value_mismatches <- unlist(value_mismatches)

  street_expected <- "ADDRESS" %in% names(raw_data)
  street_present  <- "org_addr_street_raw" %in% actual_output_columns

  vintage_passed <- rows_match &&
    length(missing_columns) == 0L && length(unexpected_columns) == 0L &&
    length(value_mismatches) == 0L && (street_expected == street_present)

  log_info(sprintf("%s: %s (rows %d/%d, value checks %s)",
                   vintage_tag, ifelse(vintage_passed, "PASS", "FAIL"),
                   nrow(processed_data), nrow(raw_data),
                   ifelse(length(value_mismatches) > 0L,
                          paste(value_mismatches, collapse = ","), "clean")))

  data.table::data.table(
    vintage = vintage_tag,
    rows_raw = nrow(raw_data), rows_proc = nrow(processed_data),
    rows_ok = rows_match,
    cols_missing = paste(missing_columns, collapse = ";"),
    cols_extra = paste(unexpected_columns, collapse = ";"),
    street_expected = street_expected, street_present = street_present,
    value_mismatches = paste(value_mismatches, collapse = ";"),
    passed = vintage_passed
  )
}

# One result row per batch vintage: raw file paths -> per-vintage check rows
# -> single results table (NULLs from out-of-batch vintages drop out in
# rbindlist).
results_table <- data.table::rbindlist(
  purrr::map(raw_file_paths, validate_one_vintage)
)

dir.create(dirname(output_tsv_path), recursive = TRUE, showWarnings = FALSE)
data.table::fwrite(results_table, output_tsv_path, sep = "\t")
n_failed <- results_table[passed == FALSE, .N]
log_info(sprintf("Validation gate: %d vintages checked, %d failed. TSV: %s",
                 nrow(results_table), n_failed, output_tsv_path))
if (n_failed > 0L) {
  quit(status = 1L)
}
