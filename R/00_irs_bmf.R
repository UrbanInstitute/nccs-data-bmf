# ============================================================================
# 00_irs_bmf.R
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

# Processing year
PROCESSING_YEAR <- "2025"

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

# ============================================================================
# Helper Functions
# ============================================================================

#' Save Checkpoint
#'
#' @description Saves intermediate data to parquet file for recovery
#' @param dt data.table to save
#' @param checkpoint_name character name of checkpoint
#' @noRd
save_checkpoint <- function(dt, checkpoint_name) {
  if (!ENABLE_CHECKPOINTS) return(invisible(NULL))

  if (!dir.exists(CHECKPOINT_DIR)) {
    dir.create(CHECKPOINT_DIR, recursive = TRUE)
  }

  path <- file.path(
    CHECKPOINT_DIR,
    sprintf("bmf_%s_%s.parquet", PROCESSING_YEAR, checkpoint_name)
  )

  arrow::write_parquet(dt, path)
  log_info(sprintf("Checkpoint saved: %s (%s rows)",
                   checkpoint_name,
                   format(nrow(dt), big.mark = ",")))
}

#' Load Checkpoint
#'
#' @description Loads data from a checkpoint file
#' @param checkpoint_name character name of checkpoint
#' @return data.table or NULL if checkpoint doesn't exist
#' @noRd
load_checkpoint <- function(checkpoint_name) {
  path <- file.path(
    CHECKPOINT_DIR,
    sprintf("bmf_%s_%s.parquet", PROCESSING_YEAR, checkpoint_name)
  )

  if (file.exists(path)) {
    dt <- data.table::as.data.table(arrow::read_parquet(path))
    log_info(sprintf("Checkpoint loaded: %s (%s rows)",
                     checkpoint_name,
                     format(nrow(dt), big.mark = ",")))
    return(dt)
  }

  return(NULL)
}

# ============================================================================
# PHASE 1: EXTRACTION
# ============================================================================

log_phase_start("EXTRACTION")

# IRS BMF source: https://www.irs.gov/charities-non-profits/exempt-organizations-business-master-file-extract-eo-bmf

# Download 4 regional BMF files
log_info("Downloading regional BMF files...")
regional_downloads <- purrr::imap(bmf_2025_url_ls, function(url, region) {
  dest_file <- paste0("data/raw/bmf_", PROCESSING_YEAR, "_", region, ".csv")
  log_info(sprintf("Downloading: %s", region))
  download.file(url, destfile = dest_file, quiet = TRUE)
  return(dest_file)
})

# Read and combine all regional files
log_info("Reading and combining regional files...")
regional_bmfs <- purrr::map(
  bmf_2025_raw_paths,
  data.table::fread,
  .progress = TRUE
)

bmf_raw <- data.table::rbindlist(regional_bmfs)
log_info(sprintf("Combined BMF: %s rows", format(nrow(bmf_raw), big.mark = ",")))

# Save raw combined file
data.table::fwrite(bmf_raw, sprintf("data/raw/bmf_%s.csv", PROCESSING_YEAR))
log_info("Raw BMF saved to data/raw/")

# ============================================================================
# PHASE 2: PRE-TRANSFORMATION VALIDATION
# ============================================================================

log_phase_start("PRE-TRANSFORMATION VALIDATION")

pre_check_results <- validate_raw_bmf_structure(
  bmf_raw,
  expected_rows = BMF_2025_EXPECTED_ROWS,
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
  sprintf("data/quality/bmf_%s_quality_report.json", PROCESSING_YEAR)
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
output_path <- sprintf("data/processed/bmf_%s_processed.parquet", PROCESSING_YEAR)
arrow::write_parquet(bmf, output_path)
log_info(sprintf("Final BMF saved: %s", output_path))

# Also save dimension tables for downstream use
if (exists("cl_code_dim_table")) {
  arrow::write_parquet(
    cl_code_dim_table,
    sprintf("data/processed/dim_classification_%s.parquet", PROCESSING_YEAR)
  )
}

if (exists("activity_dim_table")) {
  arrow::write_parquet(
    activity_dim_table,
    sprintf("data/processed/dim_activity_%s.parquet", PROCESSING_YEAR)
  )
}

log_info("Dimension tables saved")

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
# - Add address standardization and geocoding
