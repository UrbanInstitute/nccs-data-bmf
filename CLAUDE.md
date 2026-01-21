# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This repository contains code for harmonizing IRS Business Master File (BMF) data - extracts of nonprofit organizations exempt from federal income tax. The pipeline:
1. Downloads BMF from S3 bucket (Lambda ingests monthly from IRS)
2. Transforms raw fields through standardized cleaning functions
3. Creates lookup table joins for code definitions
4. Outputs a unified BMF consolidating ~1.9M nonprofit records

**Documentation**: https://urbaninstitute.github.io/nccs-data-bmf/docs/GUIDEBOOK/index.html

## Commands

### Run the Data Pipeline
```r
# In R/RStudio - runs the full BMF processing pipeline
# Downloads most recent BMF from S3 by default
source("R/run_pipeline.R")

# To process a specific month:
BMF_YEAR <- 2025
BMF_MONTH <- 1
source("R/run_pipeline.R")

# To list available BMF files in S3:
source("R/config.R")
list_available_bmf_files()
```

### Pipeline Configuration
Control flags in `run_pipeline.R`:
- `ENABLE_CHECKPOINTS` - Save intermediate states to parquet (default: TRUE)
- `STRICT_QUALITY_GATES` - Stop on validation failures (default: TRUE)
- `ENABLE_S3_UPLOAD` - Upload results to S3 (default: TRUE)
- `CHECKPOINT_DIR` - Directory for checkpoints (default: "data/checkpoints")

### Build Documentation
```bash
# Generate HTML guidebook from Quarto
cd docs && quarto render
```

## Architecture

### Data Flow
```
S3 (raw/bmf/YYYY-MM-BMF.csv) → Download → Transform → Validated BMF (parquet)
```

### Pipeline Phases
1. **Extraction** - Download BMF from S3
2. **Pre-validation** - Validate raw structure and required columns
3. **Identity** - Transform EIN, names, address, ruling date
4. **Classification** - Join lookup tables for affiliation, deductibility, foundation, organization, status codes
5. **Activity** - Unpivot activity codes and parse NTEE codes
6. **Temporal** - Parse tax period and accounting period dates
7. **Financial** - Process asset/income codes and amounts
8. **Filing** - Transform filing requirement codes
9. **Post-validation** - Generate quality report with completeness metrics
10. **Intermediate Output** - Save parquet with all columns to intermediate/ folder in S3
11. **Processed Output** - Save parquet without raw columns to processed/ folder in S3

### Key Files

**Core Infrastructure:**
- `R/run_pipeline.R` - Main orchestration (11 phases)
- `R/config.R` - S3 configuration, lookup table loading
- `R/checkpoints.R` - Save/load intermediate states
- `R/input_validation.R` - Shared validation functions
- `R/utils/logging.R` - Structured logging utilities
- `R/utils/transform_utils.R` - Reusable transformation helpers

**Quality Gates:**
- `R/quality/pre_checks.R` - Pre-transformation validation
- `R/quality/post_checks.R` - Post-transformation quality reporting

**Transforms by Category:**
- **Identity**: `ein.R`, `organization_name.R`, `dba_name.R`, `ico_name.R`, `address.R`, `ruling_date.R`, `group_exemption_number.R`
- **Classification**: `affiliation_code.R`, `deductibility_code.R`, `foundation_code.R`, `organization_code.R`, `status_code.R`, `accounting_period.R`, `transform_code.R`, `subsection_classification_codes.R`
- **Activity**: `activity_code.R`, `transform_ntee_code.R`
- **Temporal**: `transform_tax_period.R`
- **Financial**: `financial_codes.R`, `asset_amount.R`
- **Filing**: `filing_requirement_code.R`

### Transformation Pattern
Each transformation function:
1. Copies input data (avoids side effects)
2. Validates required columns exist
3. Type-converts and cleans values
4. Joins lookup tables via `data.table` keys for definitions
5. Returns transformed data.table

### Lookup Tables
- Master workbook: `data/lookup/bmf_code_lookup.xlsx` (multiple sheets)
- Individual CSVs: `data/lookup/*_lookup.csv`
- Accessed via `lookup_ls$<sheet_name>` after `config.R` loads them

### Dimension Tables
Multi-valued fields (activity codes, classification codes) create SCD Type 2 dimension tables with EIN as the grain, then aggregate back to main table.

### Checkpoints
Pipeline saves intermediate states for recovery and debugging. Checkpoint numbers are offset from phase numbers because Phase 1 (extraction) has no checkpoint:

| Checkpoint | Saved After Phase |
|------------|-------------------|
| `01_raw` | Phase 2: Pre-transformation validation |
| `02_identity` | Phase 3: Identity transformations |
| `03_classification` | Phase 4: Classification transformations |
| `04_activity` | Phase 5: Activity transformations |
| `05_temporal` | Phase 6: Temporal transformations |
| `06_financial` | Phase 7: Financial transformations |
| `07_filing` | Phase 8: Filing requirement transformations |

**Recovery functions:**
- `load_checkpoint("02_identity")` - Resume from checkpoint
- `list_checkpoints()` - View available checkpoints

## Output

**Local files:**
- `data/intermediate/bmf_YYYY_MM_intermediate.parquet` - All columns (raw + transformed)
- `data/processed/bmf_YYYY_MM_processed.parquet` - Transformed columns only
- `data/processed/bmf_YYYY_MM_data_dictionary.csv` - Column metadata and stats
- `data/quality/bmf_YYYY_MM_quality_report.json` - Quality metrics

**S3 upload (if enabled):**
- `intermediate/bmf/YYYY_MM/bmf_YYYY_MM_intermediate.parquet` - All columns
- `intermediate/bmf/YYYY_MM/bmf_YYYY_MM_quality_report.json`
- `processed/bmf/YYYY_MM/bmf_YYYY_MM_processed.parquet` - Transformed only
- `processed/bmf/YYYY_MM/bmf_YYYY_MM_data_dictionary.csv` - Column metadata
- `processed/bmf/YYYY_MM/bmf_YYYY_MM_quality_report.json`

## Key Dependencies

- `data.table` - Primary data manipulation (chosen for speed with ~2M rows)
- `arrow` - Parquet file I/O for checkpoints and output
- `aws.s3` - S3 bucket operations for downloading BMF
- `openxlsx` - Excel lookup file reading
- `here` - Project-relative paths
- `purrr` - Functional iteration
- `stringr` - String manipulation
- `lubridate` - Date parsing
- `jsonlite` - Quality report serialization

## Data Sources

BMF files are downloaded from S3 bucket `nccsdata`:
- Path: `raw/bmf/YYYY-MM-BMF.csv`
- A Lambda function ingests monthly BMF files from IRS and deposits them here
- Use `list_available_bmf_files()` to see available months

## Conventions

- Use `here::here()` for all file paths
- Transformation functions should be pure (copy input, don't modify in place)
- New field transforms go in dedicated `R/<field_name>.R` files
- Add corresponding lookup data to Excel workbook or CSV
- Call new transforms from `run_pipeline.R` in logical sequence
