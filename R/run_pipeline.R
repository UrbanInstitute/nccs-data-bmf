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

# In-Care-Of Name
log_transform_start("ICO Name")
bmf <- transform_ico_name(bmf)

# Group Exemption Number
log_transform_start("Group Exemption Number")
bmf <- transform_group_exemption_number(bmf)

# Ruling Date
log_transform_start("Ruling Date")
bmf <- transform_ruling_date(bmf)

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
cl_code_dim_table <- create_cl_code_dim_table(
  bmf,
  lookup = classification_code_lookup,
  year = PROCESSING_YEAR
)
bmf <- transform_subsection_classification_codes(
  bmf,
  dim_table = cl_code_dim_table,
  orgtype_lookup = subsection_orgtype_lookup
)

# Affiliation Code
log_transform_start("Affiliation Code")
bmf <- transform_affiliation_code(bmf)

# Deductibility Code
log_transform_start("Deductibility Code")
bmf <- transform_deductibility_code(bmf)

# Foundation Code
log_transform_start("Foundation Code")
bmf <- transform_foundation_code(bmf)

# Organization Code
log_transform_start("Organization Code")
bmf <- transform_organization_code(bmf)

# Status Code
log_transform_start("Status Code")
bmf <- transform_status_code(
  bmf,
  lookup = lookup_ls$status_code
)

save_checkpoint(bmf, "03_classification")

# ============================================================================
# PHASE 5: ACTIVITY TRANSFORMATIONS
# ============================================================================

log_phase_start("ACTIVITY TRANSFORMATIONS")

# Activity Code (dimension table pattern)
log_transform_start("Activity Code")
activity_dim_table <- create_activity_code_dim_table(
  bmf,
  lookup = activity_code_lookup
)
bmf <- transform_activity_code(bmf, activity_dim_table)

# NTEE Code (dimension table pattern)
log_transform_start("NTEE Code")
bmf <- transform_ntee_code(
  bmf,
  ntee_code_lookup = lookup_ls$ntee_code,
  ntee_major_group_lookup = lookup_ls$ntee_code_major_group,
  activity_code_lookup = lookup_ls$ntee_code_activity_type,
  input_ntee_col = "NTEE_CD",
  year = PROCESSING_YEAR,
  write_scd = FALSE
)

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
bmf <- transform_asset_code(bmf, lookup = lookup_ls$asset_code)

# Income Code
log_transform_start("Income Code")
bmf <- transform_income_code(bmf, lookup = lookup_ls$income_code)

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
bmf <- transform_code(
  dt = bmf,
  input_col = "FILING_REQ_CD",
  lookup_key = "filing_requirement_code",
  definition_col = "filing_requirement_code_definition",
  type_conversion_func = as.integer,
  lookup = lookup_ls$filing_requirement_code
)

# PF Filing Requirement Code
log_transform_start("PF Filing Requirement Code")
bmf <- transform_code(
  dt = bmf,
  input_col = "PF_FILING_REQ_CD",
  lookup_key = "pf_filing_requirement_code",
  definition_col = "pf_filing_requirement_code_definition",
  type_conversion_func = as.integer,
  lookup = lookup_ls$pf_filing_requirement_code
)

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

# ============================================================================
# PHASE 10: OUTPUT
# ============================================================================

log_phase_start("OUTPUT")

# Create output directory if needed
if (!dir.exists("data/processed")) {
  dir.create("data/processed", recursive = TRUE)
}

# Save final processed BMF
output_path <- sprintf("data/processed/bmf_%s_%s_processed.parquet", PROCESSING_YEAR, PROCESSING_MONTH)
arrow::write_parquet(bmf, output_path)
log_info(sprintf("Final BMF saved: %s", output_path))

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
# - Complement organization names with e-file data (NAME field often truncated)
# - Add SORT_NAME transformation
# - Migrate to DuckDB for larger-than-memory processing
# - Create additional metadata/dimension tables
# - Add geocoding to standardized addresses
