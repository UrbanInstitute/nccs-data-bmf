# ============================================================================
# run_legacy_pipeline.R
# Pipeline orchestrator for legacy NCCS 501CX-NONPROFIT-PX BMF files.
#
# Mirrors run_pipeline.R, with these differences:
#   - Phase 1: read from local data/raw/legacy/, not S3
#   - Phase 1.5 (NEW): harmonize legacy schema -> current schema
#   - Phase 2: relaxed pre-validation (BMF_LEGACY_MIN_COLUMNS)
#   - Phase 11: slim processed output to columns backed by populated input
#
# Usage:
#   LEGACY_BMF_FILE <- "data/raw/legacy/BMF-2013-07-501CX-NONPROFIT-PX.csv"
#   source("R/run_legacy_pipeline.R")
# ============================================================================

# ============================================================================
# Pipeline Configuration
# ============================================================================

ENABLE_CHECKPOINTS <- TRUE
CHECKPOINT_DIR <- "data/checkpoints"
STRICT_QUALITY_GATES <- TRUE
ENABLE_S3_UPLOAD <- FALSE  # Legacy outputs default to local-only

if (!exists("LEGACY_BMF_FILE") || is.null(LEGACY_BMF_FILE)) {
  stop("Set LEGACY_BMF_FILE before sourcing run_legacy_pipeline.R, e.g. LEGACY_BMF_FILE <- 'data/raw/legacy/BMF-2013-07-501CX-NONPROFIT-PX.csv'")
}

PROCESSING_YEAR <- NULL
PROCESSING_MONTH <- NULL

# ============================================================================
# Library Loading
# ============================================================================

library(data.table)

# ============================================================================
# Source Helper Scripts
# ============================================================================

source(here::here("R", "config.R"))
source(here::here("R", "input_validation.R"))
source(here::here("R", "utils", "logging.R"))
source(here::here("R", "utils", "transform_utils.R"))

source(here::here("R", "quality", "pre_checks.R"))
source(here::here("R", "quality", "legacy_pre_checks.R"))
source(here::here("R", "quality", "post_checks.R"))

source(here::here("R", "legacy_bmf_adapter.R"))

source(here::here("R", "ein.R"))
source(here::here("R", "organization_name.R"))
source(here::here("R", "dba_name.R"))
source(here::here("R", "ico_name.R"))
source(here::here("R", "group_exemption_number.R"))
source(here::here("R", "ruling_date.R"))
source(here::here("R", "address.R"))

source(here::here("R", "affiliation_code.R"))
source(here::here("R", "deductibility_code.R"))
source(here::here("R", "foundation_code.R"))
source(here::here("R", "organization_code.R"))
source(here::here("R", "status_code.R"))
source(here::here("R", "accounting_period.R"))
source(here::here("R", "filing_requirement_code.R"))
source(here::here("R", "transform_code.R"))

source(here::here("R", "activity_code.R"))
source(here::here("R", "subsection_classification_codes.R"))
source(here::here("R", "transform_tax_period.R"))
source(here::here("R", "financial_codes.R"))
source(here::here("R", "asset_amount.R"))
source(here::here("R", "transform_ntee_code.R"))

source(here::here("R", "checkpoints.R"))

# ============================================================================
# PHASE 1: EXTRACTION (local file)
# ============================================================================

log_phase_start("EXTRACTION (legacy)")

if (!file.exists(LEGACY_BMF_FILE)) {
  log_error(sprintf("Legacy BMF file not found: %s", LEGACY_BMF_FILE))
}

# Parse YYYY-MM from filename: BMF-YYYY-MM-501CX-NONPROFIT-PX.csv
fname <- basename(LEGACY_BMF_FILE)
ym_match <- stringr::str_match(fname, "BMF-(\\d{4})-(\\d{2})-501CX")
if (is.na(ym_match[1, 1])) {
  log_error(sprintf("Cannot parse YYYY-MM from filename: %s", fname))
}
PROCESSING_YEAR <- ym_match[1, 2]
PROCESSING_MONTH <- ym_match[1, 3]
log_info(sprintf("Processing legacy BMF for %s-%s", PROCESSING_YEAR, PROCESSING_MONTH))

bmf_raw <- data.table::fread(LEGACY_BMF_FILE, na.strings = c("", "NA"))
log_info(sprintf("Loaded %s rows, %d cols",
                 format(nrow(bmf_raw), big.mark = ","), ncol(bmf_raw)))

# ============================================================================
# PHASE 1.5: HARMONIZATION (legacy-only)
# ============================================================================

log_phase_start("HARMONIZATION")

crosswalk_v2 <- load_crosswalk_v2()
populated_raw_columns <- crosswalk_v2[
  disposition == "rename" & legacy_name_upper %in% toupper(names(bmf_raw)),
  unique(current_name)
]

harmonization <- harmonize_legacy_bmf(bmf_raw, crosswalk_v2)
bmf_raw <- harmonization$dt

harmonization_report_path <- sprintf(
  "data/quality/bmf_legacy_%s_%s_harmonization_report.json",
  PROCESSING_YEAR, PROCESSING_MONTH
)
save_harmonization_report(harmonization$report, harmonization_report_path)

# ============================================================================
# PHASE 2: PRE-TRANSFORMATION VALIDATION (legacy mode)
# ============================================================================

log_phase_start("PRE-TRANSFORMATION VALIDATION (legacy)")

pre_check_results <- validate_legacy_bmf_structure(
  bmf_raw,
  strict = STRICT_QUALITY_GATES
)

save_checkpoint(bmf_raw, "01_raw")

# ============================================================================
# PHASE 3: IDENTITY TRANSFORMATIONS
# ============================================================================

log_phase_start("IDENTITY TRANSFORMATIONS")

bmf <- data.table::copy(bmf_raw)

log_transform_start("EIN");                      bmf <- transform_ein(bmf)
log_transform_start("Organization Name");        bmf <- transform_organization_name(bmf)
log_transform_start("DBA Name");                 bmf <- transform_dba_name(bmf)
log_transform_start("ICO Name");                 bmf <- transform_bmf_ico_name(bmf)
log_transform_start("Group Exemption Number");   bmf <- transform_bmf_group_exemption_number(bmf)
log_transform_start("Ruling Date");              bmf <- transform_bmf_ruling_date(bmf)
log_transform_start("Address");                  bmf <- transform_address(bmf)

save_checkpoint(bmf, "02_identity")

# ============================================================================
# PHASE 4: CLASSIFICATION TRANSFORMATIONS
# ============================================================================

log_phase_start("CLASSIFICATION TRANSFORMATIONS")

log_transform_start("Subsection/Classification Codes"); bmf <- transform_bmf_subsection_classification_codes(bmf)
log_transform_start("Affiliation Code");                bmf <- transform_bmf_affiliation_code(bmf)
log_transform_start("Deductibility Code");              bmf <- transform_bmf_deductibility_code(bmf)
log_transform_start("Foundation Code");                 bmf <- transform_bmf_foundation_code(bmf)
log_transform_start("Organization Code");               bmf <- transform_bmf_organization_code(bmf)
log_transform_start("Status Code");                     bmf <- transform_bmf_status_code(bmf)

save_checkpoint(bmf, "03_classification")

# ============================================================================
# PHASE 5: ACTIVITY TRANSFORMATIONS
# ============================================================================

log_phase_start("ACTIVITY TRANSFORMATIONS")

log_transform_start("Activity Code"); bmf <- transform_bmf_activity_code(bmf)
log_transform_start("NTEE Code");     bmf <- transform_ntee_code(bmf)

save_checkpoint(bmf, "04_activity")

# ============================================================================
# PHASE 6: TEMPORAL TRANSFORMATIONS
# ============================================================================

log_phase_start("TEMPORAL TRANSFORMATIONS")

log_transform_start("Tax Period");        bmf <- transform_tax_period(bmf)
log_transform_start("Accounting Period"); bmf <- transform_accounting_period(bmf)

save_checkpoint(bmf, "05_temporal")

# ============================================================================
# PHASE 7: FINANCIAL TRANSFORMATIONS
# ============================================================================

log_phase_start("FINANCIAL TRANSFORMATIONS")

log_transform_start("Asset Code");     bmf <- transform_bmf_asset_code(bmf, lookup = lookup_ls$asset_code)
log_transform_start("Income Code");    bmf <- transform_bmf_income_code(bmf, lookup = lookup_ls$income_code)
log_transform_start("Asset Amount");   bmf <- transform_asset_amount(bmf)
log_transform_start("Income Amount");  bmf <- transform_income_amount(bmf)
log_transform_start("Revenue Amount"); bmf <- transform_revenue_amount(bmf)

save_checkpoint(bmf, "06_financial")

# ============================================================================
# PHASE 8: FILING REQUIREMENT TRANSFORMATIONS
# ============================================================================

log_phase_start("FILING REQUIREMENT TRANSFORMATIONS")

log_transform_start("Filing Requirement Code");    bmf <- transform_bmf_filing_requirement_code(bmf)
log_transform_start("PF Filing Requirement Code"); bmf <- transform_bmf_pf_filing_requirement_code(bmf)

save_checkpoint(bmf, "07_filing")

# ============================================================================
# PHASE 9: POST-TRANSFORMATION VALIDATION
# ============================================================================

log_phase_start("POST-TRANSFORMATION VALIDATION")

quality_report <- generate_quality_report(bmf, pre_check_results = pre_check_results)
print_quality_report(quality_report)
save_quality_report(
  quality_report,
  sprintf("data/quality/bmf_legacy_%s_%s_quality_report.json", PROCESSING_YEAR, PROCESSING_MONTH)
)

quality_html_dir <- here::here("docs", "quality-reports")
if (!dir.exists(quality_html_dir)) dir.create(quality_html_dir, recursive = TRUE)
quality_html_path <- file.path(quality_html_dir,
  sprintf("bmf_legacy_%s_%s_quality_report.html", PROCESSING_YEAR, PROCESSING_MONTH))
tryCatch({
  render_quality_report(quality_report, quality_html_path, format = "html")
  log_info(sprintf("Quality report HTML rendered: %s", quality_html_path))
}, error = function(e) {
  log_warn(sprintf("HTML quality report render failed (non-fatal): %s", conditionMessage(e)))
})

# ============================================================================
# PHASE 10: INTERMEDIATE OUTPUT (FULL schema, all cols)
# ============================================================================

log_phase_start("INTERMEDIATE OUTPUT")

if (!dir.exists("data/intermediate")) dir.create("data/intermediate", recursive = TRUE)

intermediate_path <- sprintf("data/intermediate/bmf_legacy_%s_%s_intermediate.parquet",
                             PROCESSING_YEAR, PROCESSING_MONTH)
arrow::write_parquet(bmf, intermediate_path)
log_info(sprintf("Intermediate BMF saved: %s", intermediate_path))

# ============================================================================
# PHASE 11: PROCESSED OUTPUT (slim — only columns backed by populated input)
# ============================================================================

log_phase_start("PROCESSED OUTPUT (legacy slim schema)")

raw_columns_to_remove <- c(
  "EIN", "NAME", "ICO", "STREET", "CITY", "STATE", "ZIP",
  "GROUP", "SUBSECTION", "AFFILIATION", "CLASSIFICATION",
  "RULING", "DEDUCTIBILITY", "FOUNDATION", "ACTIVITY",
  "ORGANIZATION", "STATUS", "TAX_PERIOD", "ASSET_CD",
  "INCOME_CD", "FILING_REQ_CD", "PF_FILING_REQ_CD",
  "ASSET_AMT", "INCOME_AMT", "REVENUE_AMT", "NTEE_CD",
  "SORT_NAME", "ACCT_PD", "REGION", "RYEAR", "ID"
)

legacy_keep_cols <- compute_legacy_output_columns(populated_raw_columns)
all_transformed_cols <- setdiff(names(bmf), raw_columns_to_remove)
cols_to_keep <- intersect(all_transformed_cols, legacy_keep_cols)
dropped_na_cols <- setdiff(all_transformed_cols, legacy_keep_cols)

log_info(sprintf("Populated raw inputs: %d (%s)",
                 length(populated_raw_columns),
                 paste(populated_raw_columns, collapse = ", ")))
log_info(sprintf("Legacy processed schema: keeping %d transformed cols, dropping %d NA-only cols",
                 length(cols_to_keep), length(dropped_na_cols)))
if (length(dropped_na_cols) > 0) {
  log_info(sprintf("  Dropped: %s", paste(dropped_na_cols, collapse = ", ")))
}

bmf_processed <- bmf[, ..cols_to_keep]

if (!dir.exists("data/processed")) dir.create("data/processed", recursive = TRUE)

processed_path <- sprintf("data/processed/bmf_legacy_%s_%s_processed.csv",
                          PROCESSING_YEAR, PROCESSING_MONTH)
data.table::fwrite(bmf_processed, processed_path)
log_info(sprintf("Processed BMF saved: %s", processed_path))

log_info("Generating data dictionary")
data_dictionary <- generate_data_dictionary(bmf_processed)
dictionary_path <- sprintf("data/processed/bmf_legacy_%s_%s_data_dictionary.csv",
                           PROCESSING_YEAR, PROCESSING_MONTH)
data.table::fwrite(data_dictionary, dictionary_path)
log_info(sprintf("Data dictionary saved: %s (%d columns)",
                 dictionary_path, nrow(data_dictionary)))

# ============================================================================
# PIPELINE COMPLETE
# ============================================================================

log_phase_start("PIPELINE COMPLETE")
log_info(sprintf("Total rows processed: %s", format(nrow(bmf), big.mark = ",")))
log_info(sprintf("Intermediate cols: %d  Processed cols: %d", ncol(bmf), ncol(bmf_processed)))
log_info(sprintf("Quality gate: %s",
                 ifelse(quality_report$passed, "PASSED", "FAILED")))
