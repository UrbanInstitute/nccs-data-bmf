# ============================================================================
# run_pipeline.R
# BMF Data Processing Pipeline Orchestrator
# IRM Reference: https://www.irs.gov/irm/part25/irm_25-007-001
# ============================================================================

# ============================================================================
# Pipeline Configuration
# ============================================================================

# Enable checkpointing (saves intermediate results to parquet)
ENABLE_CHECKPOINTS <- TRUE
CHECKPOINT_DIR <- "data/checkpoints"

# Enable strict quality gates (stops on validation failures)
STRICT_QUALITY_GATES <- TRUE

# Enable S3 upload of processed data and quality report
ENABLE_S3_UPLOAD <- TRUE

# BMF source configuration - set these before sourcing to override defaults
# If not set, downloads most recent BMF file from S3
if (!exists("BMF_YEAR")) BMF_YEAR <- NULL
if (!exists("BMF_MONTH")) BMF_MONTH <- NULL

# Processing year/month - will be set automatically from downloaded file
PROCESSING_YEAR <- NULL
PROCESSING_MONTH <- NULL

# ============================================================================
# Library Loading
# ============================================================================

library(data.table)

# ============================================================================
# Source Helper Scripts
# ============================================================================

# Configuration (must be first - sets paths)
source(here::here("R", "config.R"))

# Shared utilities
source(here::here("R", "input_validation.R"))
source(here::here("R", "utils", "logging.R"))
source(here::here("R", "utils", "transform_utils.R"))

# Quality gates
source(here::here("R", "quality", "pre_checks.R"))
source(here::here("R", "quality", "post_checks.R"))

# Identity transforms
source(here::here("R", "ein.R"))
source(here::here("R", "organization_name.R"))
source(here::here("R", "dba_name.R"))
source(here::here("R", "ico_name.R"))
source(here::here("R", "group_exemption_number.R"))
source(here::here("R", "ruling_date.R"))
source(here::here("R", "address.R"))

# Lookup transforms
source(here::here("R", "affiliation_code.R"))
source(here::here("R", "deductibility_code.R"))
source(here::here("R", "foundation_code.R"))
source(here::here("R", "organization_code.R"))
source(here::here("R", "status_code.R"))
source(here::here("R", "accounting_period.R"))
source(here::here("R", "filing_requirement_code.R"))
source(here::here("R", "transform_code.R"))

# Complex transforms
source(here::here("R", "activity_code.R"))
source(here::here("R", "subsection_classification_codes.R"))
source(here::here("R", "transform_tax_period.R"))
source(here::here("R", "financial_codes.R"))
source(here::here("R", "asset_amount.R"))
source(here::here("R", "transform_ntee_code.R"))

# save/load data
source(here::here("R", "checkpoints.R"))

# ============================================================================
# PHASE 1: EXTRACTION
# ============================================================================

log_phase_start("EXTRACTION")

# Download BMF from S3 (defaults to most recent if BMF_YEAR/BMF_MONTH not set)
log_info("Downloading BMF from S3")
bmf_raw <- download_bmf_from_s3(
  bucket = BMF_S3_BUCKET,
  prefix = BMF_S3_PREFIX,
  year = BMF_YEAR,
  month = BMF_MONTH
)

# Extract processing year/month from downloaded file
source_ym <- attr(bmf_raw, "source_year_month")
PROCESSING_YEAR <- stringr::str_extract(source_ym, "^\\d{4}")
PROCESSING_MONTH <- stringr::str_extract(source_ym, "\\d{2}$")

log_info(sprintf("Processing BMF for %s-%s", PROCESSING_YEAR, PROCESSING_MONTH))

# Save raw file locally for reference
raw_output_path <- sprintf("data/raw/bmf_%s_%s.csv", PROCESSING_YEAR, PROCESSING_MONTH)
data.table::fwrite(bmf_raw, raw_output_path)
log_info(sprintf("Raw BMF saved to %s", raw_output_path))

# ============================================================================
# PHASE 2: PRE-TRANSFORMATION VALIDATION
# ============================================================================

log_phase_start("PRE-TRANSFORMATION VALIDATION")

pre_check_results <- validate_raw_bmf_structure(
  bmf_raw,
  strict = STRICT_QUALITY_GATES
)

save_checkpoint(bmf_raw, "01_raw")

# ============================================================================
# PHASE 3: IDENTITY TRANSFORMATIONS
# ============================================================================

log_phase_start("IDENTITY TRANSFORMATIONS")

# Start with a fresh copy for transformations
bmf <- data.table::copy(bmf_raw)

# EIN
log_transform_start("EIN")
bmf <- transform_ein(bmf)

# Organization Name
log_transform_start("Organization Name")
bmf <- transform_organization_name(bmf)

# DBA Name
log_transform_start("DBA Name")
bmf <- transform_dba_name(bmf)

# In-Care-Of Name
log_transform_start("ICO Name")
bmf <- transform_bmf_ico_name(bmf)

# Group Exemption Number
log_transform_start("Group Exemption Number")
bmf <- transform_bmf_group_exemption_number(bmf)

# Ruling Date
log_transform_start("Ruling Date")
bmf <- transform_bmf_ruling_date(bmf)

# Address
log_transform_start("Address")
bmf <- transform_address(bmf)

save_checkpoint(bmf, "02_identity")

# ============================================================================
# PHASE 4: CLASSIFICATION TRANSFORMATIONS
# ============================================================================

log_phase_start("CLASSIFICATION TRANSFORMATIONS")

# Subsection and Classification Codes (dimension table pattern)
log_transform_start("Subsection/Classification Codes")
bmf <- transform_bmf_subsection_classification_codes(bmf)

# Affiliation Code
log_transform_start("Affiliation Code")
bmf <- transform_bmf_affiliation_code(bmf)

# Deductibility Code
log_transform_start("Deductibility Code")
bmf <- transform_bmf_deductibility_code(bmf)

# Foundation Code
log_transform_start("Foundation Code")
bmf <- transform_bmf_foundation_code(bmf)

# Organization Code
log_transform_start("Organization Code")
bmf <- transform_bmf_organization_code(bmf)

# Status Code
log_transform_start("Status Code")
bmf <- transform_bmf_status_code(bmf)

save_checkpoint(bmf, "03_classification")

# ============================================================================
# PHASE 5: ACTIVITY TRANSFORMATIONS
# ============================================================================

log_phase_start("ACTIVITY TRANSFORMATIONS")

# Activity Code (dimension table pattern)
log_transform_start("Activity Code")
bmf <- transform_bmf_activity_code(bmf)

# NTEE Code (dimension table pattern)
log_transform_start("NTEE Code")
bmf <- transform_ntee_code(bmf)

save_checkpoint(bmf, "04_activity")

# ============================================================================
# PHASE 6: TEMPORAL TRANSFORMATIONS
# ============================================================================

log_phase_start("TEMPORAL TRANSFORMATIONS")

# Tax Period
log_transform_start("Tax Period")
bmf <- transform_tax_period(bmf)

# Accounting Period
log_transform_start("Accounting Period")
bmf <- transform_accounting_period(bmf)

save_checkpoint(bmf, "05_temporal")

# ============================================================================
# PHASE 7: FINANCIAL TRANSFORMATIONS
# ============================================================================

log_phase_start("FINANCIAL TRANSFORMATIONS")

# Asset Code
log_transform_start("Asset Code")
bmf <- transform_bmf_asset_code(bmf, lookup = lookup_ls$asset_code)

# Income Code
log_transform_start("Income Code")
bmf <- transform_bmf_income_code(bmf, lookup = lookup_ls$income_code)

# Asset Amount
log_transform_start("Asset Amount")
bmf <- transform_asset_amount(bmf)

# Income Amount
log_transform_start("Income Amount")
bmf <- transform_income_amount(bmf)

# Revenue Amount
log_transform_start("Revenue Amount")
bmf <- transform_revenue_amount(bmf)

save_checkpoint(bmf, "06_financial")

# ============================================================================
# PHASE 8: FILING REQUIREMENT TRANSFORMATIONS
# ============================================================================

log_phase_start("FILING REQUIREMENT TRANSFORMATIONS")

# Filing Requirement Code
log_transform_start("Filing Requirement Code")
bmf <- transform_bmf_filing_requirement_code(bmf)

# PF Filing Requirement Code
log_transform_start("PF Filing Requirement Code")
bmf <- transform_bmf_pf_filing_requirement_code(bmf)

save_checkpoint(bmf, "07_filing")

# ============================================================================
# PHASE 9: POST-TRANSFORMATION VALIDATION
# ============================================================================

log_phase_start("POST-TRANSFORMATION VALIDATION")

# Generate quality report
quality_report <- generate_quality_report(
  bmf,
  pre_check_results = pre_check_results
)

# Print and save report
print_quality_report(quality_report)
save_quality_report(
  quality_report,
  sprintf("data/quality/bmf_%s_%s_quality_report.json", PROCESSING_YEAR, PROCESSING_MONTH)
)

# Render quality report to HTML for GitHub Pages
quality_html_dir <- here::here("docs", "quality-reports")
if (!dir.exists(quality_html_dir)) {
  dir.create(quality_html_dir, recursive = TRUE)
}
quality_html_path <- file.path(quality_html_dir, sprintf("bmf_%s_%s_quality_report.html", PROCESSING_YEAR, PROCESSING_MONTH))
# The HTML render (and its index) are cosmetic GitHub Pages artifacts. A render
# failure (e.g. a quarto CLI/environment issue) must NOT abort the pipeline
# before the data outputs in Phases 10-11 are written. Keep it non-fatal.
tryCatch({
  render_quality_report(quality_report, quality_html_path, format = "html")
  log_info(sprintf("Quality report HTML rendered: %s", quality_html_path))
  source(here::here("R", "utils", "render_quality_report_index.R"))
  render_quality_report_index(quality_html_dir)
}, error = function(e) {
  log_warn(sprintf("Quality report HTML render skipped (non-fatal): %s",
                   conditionMessage(e)))
})

# ============================================================================
# PHASE 10: INTERMEDIATE OUTPUT (ALL COLUMNS)
# ============================================================================

log_phase_start("INTERMEDIATE OUTPUT")

# Create output directories if needed
if (!dir.exists("data/intermediate")) {
  dir.create("data/intermediate", recursive = TRUE)
}

# Save intermediate BMF (all columns - original + transformed)
intermediate_path <- sprintf("data/intermediate/bmf_%s_%s_intermediate.parquet", PROCESSING_YEAR, PROCESSING_MONTH)
arrow::write_parquet(bmf, intermediate_path)
log_info(sprintf("Intermediate BMF saved: %s", intermediate_path))

# Upload intermediate to S3 if enabled
if (ENABLE_S3_UPLOAD) {
  log_info("Uploading intermediate BMF and quality report to S3")
  quality_report_path <- sprintf("data/quality/bmf_%s_%s_quality_report.json", PROCESSING_YEAR, PROCESSING_MONTH)
  upload_intermediate_results <- upload_bmf_results(
    parquet_path = intermediate_path,
    quality_report_path = quality_report_path,
    year = PROCESSING_YEAR,
    month = PROCESSING_MONTH
  )
}

# ============================================================================
# PHASE 11: PROCESSED OUTPUT (TRANSFORMED COLUMNS ONLY)
# ============================================================================

log_phase_start("PROCESSED OUTPUT")

# Define raw columns to remove (original IRS BMF columns)
raw_columns_to_remove <- c(
  "EIN", "NAME", "ICO", "STREET", "CITY", "STATE", "ZIP",
  "GROUP", "SUBSECTION", "AFFILIATION", "CLASSIFICATION",
  "RULING", "DEDUCTIBILITY", "FOUNDATION", "ACTIVITY",
  "ORGANIZATION", "STATUS", "TAX_PERIOD", "ASSET_CD",
  "INCOME_CD", "FILING_REQ_CD", "PF_FILING_REQ_CD",
  "ASSET_AMT", "INCOME_AMT", "REVENUE_AMT", "NTEE_CD",
  "SORT_NAME", "ACCT_PD", "REGION", "RYEAR", "ID"
)

# Create processed BMF by removing raw columns
cols_to_keep <- setdiff(names(bmf), raw_columns_to_remove)
bmf_processed <- bmf[, ..cols_to_keep]

log_info(sprintf("Processed BMF: removed %d raw columns, keeping %d transformed columns",
                 length(raw_columns_to_remove), length(cols_to_keep)))

# Create output directory if needed
if (!dir.exists("data/processed")) {
  dir.create("data/processed", recursive = TRUE)
}

# Save processed BMF (transformed columns only)
processed_path <- sprintf("data/processed/bmf_%s_%s_processed.csv", PROCESSING_YEAR, PROCESSING_MONTH)
data.table::fwrite(bmf_processed, processed_path)
log_info(sprintf("Processed BMF saved: %s", processed_path))

# Generate data dictionary
log_info("Generating data dictionary")
data_dictionary <- generate_data_dictionary(bmf_processed)
dictionary_path <- sprintf("data/processed/bmf_%s_%s_data_dictionary.csv", PROCESSING_YEAR, PROCESSING_MONTH)
data.table::fwrite(data_dictionary, dictionary_path)
log_info(sprintf("Data dictionary saved: %s (%d columns)", dictionary_path, nrow(data_dictionary)))

# Upload processed to S3 if enabled
if (ENABLE_S3_UPLOAD) {
  log_info("Uploading processed BMF, quality report, and data dictionary to S3")
  quality_report_path <- sprintf("data/quality/bmf_%s_%s_quality_report.json", PROCESSING_YEAR, PROCESSING_MONTH)
  upload_processed_results <- upload_processed_bmf(
    csv_path = processed_path,
    quality_report_path = quality_report_path,
    dictionary_path = dictionary_path,
    year = PROCESSING_YEAR,
    month = PROCESSING_MONTH
  )

  # Upload bucket README
  log_info("Uploading S3 bucket README")
  upload_s3_readme()
}

# Note: Dimension tables are not saved per-file.
# Unified dimension tables will be built in the cumulative repo from all processed BMFs.

# ============================================================================
# PIPELINE COMPLETE
# ============================================================================

log_phase_start("PIPELINE COMPLETE")
log_info(sprintf("Total rows processed: %s", format(nrow(bmf), big.mark = ",")))
log_info(sprintf("Total columns: %d", ncol(bmf)))
log_info(sprintf("Quality gate: %s",
                 ifelse(quality_report$passed, "PASSED", "FAILED")))

# ============================================================================
# TODO / FUTURE ENHANCEMENTS
# ============================================================================
