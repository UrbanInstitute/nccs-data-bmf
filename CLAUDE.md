# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This repository contains code for harmonizing IRS Business Master File (BMF) data - extracts of nonprofit organizations exempt from federal income tax. The pipeline:
1. Downloads BMF from S3 bucket (Lambda ingests monthly from IRS)
2. Transforms raw fields through standardized cleaning functions
3. Creates lookup table joins for code definitions
4. Outputs a unified BMF consolidating ~1.9M nonprofit records

**Documentation**: https://urbaninstitute.github.io/nccs-data-bmf/index.html

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

### Run the Geocoding Workflow
```r
# Phase 1: Export address batches for geocoding
GEOCODING_MODE <- "export"
source("R/run_geocoding.R")

# ... upload to Urban Institute geocoder, download results ...

# Phase 2: Merge geocoded results back into BMF
GEOCODING_MODE <- "merge"
source("R/run_geocoding.R")
```

### Run the Legacy BMF Pipeline
For NCCS legacy 501CX-NONPROFIT-PX BMF files (1989–2016 vintages). These
files use NCCS-curated column names that differ from the current IRS BMF
schema; the legacy pipeline harmonizes them and runs the same transforms.

```r
# Download a specific vintage from S3 (s3://nccsdata/legacy/bmf/):
LEGACY_BMF_YEAR  <- 2013
LEGACY_BMF_MONTH <- 7
source("R/run_legacy_pipeline.R")

# Or download the most recent legacy file from S3:
source("R/run_legacy_pipeline.R")

# Or process a local file (skips S3 download):
LEGACY_BMF_FILE <- "data/raw/legacy/BMF-2013-07-501CX-NONPROFIT-PX.csv"
source("R/run_legacy_pipeline.R")

# To list available legacy BMF files in S3:
source("R/config.R")
list_available_legacy_bmf_files()
```

Legacy outputs use a `bmf_legacy_YYYY_MM_*` prefix and upload to
`s3://nccsdata/{intermediate,processed}/bmf-legacy/YYYY_MM/` (separate
from the monthly current-BMF outputs at `bmf/YYYY_MM/`). The Phase 11 processed CSV
contains a slim per-file schema — only columns whose underlying input
was actually populated in the legacy file. The intermediate parquet
keeps the full schema for audit. See `docs/09-legacy-harmonization.qmd`
for the full design.

### Batch-process all legacy vintages on EC2
For running the legacy pipeline across every vintage in
`s3://nccsdata/legacy/bmf/`, use the EC2 batch scripts:

```bash
# One-shot environment bootstrap (Ubuntu 22.04)
bash scripts/setup_ec2.sh

# Run every legacy vintage serially (one Rscript subprocess per vintage)
bash scripts/run_all_legacy.sh                  # oldest first
bash scripts/run_all_legacy.sh --newest-first
SKIP_EXISTING=1 bash scripts/run_all_legacy.sh  # resume after a failure
```

Per-vintage logs land in `logs/legacy/`; a roll-up status TSV is at
`logs/legacy/run_summary.tsv`. See `docs/10-ec2-batch-processing.qmd`
for the full EC2 setup walkthrough.

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
11. **Processed Output** - Save CSV without raw columns to processed/ folder in S3

### Key Files

**Core Infrastructure:**
- `R/run_pipeline.R` - Main orchestration (11 phases)
- `R/config.R` - S3 configuration, lookup table loading, upload functions
- `R/checkpoints.R` - Save/load intermediate states
- `R/input_validation.R` - Shared validation functions
- `R/utils/logging.R` - Structured logging utilities
- `R/utils/transform_utils.R` - Reusable transformation helpers

**Geocoding Workflow:**
- `R/run_geocoding.R` - Geocoding orchestrator (export/merge modes)
- `R/geocoding_export.R` - Prepare address batches for Urban geocoder
- `R/geocoding_merge.R` - Merge geocoded results back into processed BMF
- `R/quality/geocoding_checks.R` - Geocoding quality validation

**Quality Gates:**
- `R/quality/pre_checks.R` - Pre-transformation validation (defines `BMF_REQUIRED_COLUMNS` and `BMF_LEGACY_MIN_COLUMNS`)
- `R/quality/post_checks.R` - Post-transformation quality reporting
- `R/quality/legacy_pre_checks.R` - Relaxed pre-validation for legacy BMF mode

**Legacy BMF Harmonization (501CX-NONPROFIT-PX, 1989–2016):**
- `R/run_legacy_pipeline.R` - Legacy orchestrator (mirrors `run_pipeline.R` with harmonization at Phase 1.5 and slim Phase 11 output)
- `R/legacy_bmf_adapter.R` - `harmonize_legacy_bmf()`, `compute_legacy_output_columns()`, crosswalk loader
- `data/crosswalks/XWALK-BMF-V2.0.csv` - Legacy → current schema mapping (canonical)
- `data/crosswalks/legacy_column_inventory.csv` - Long-format inventory of all 47 columns observed across 73 scraped dictionaries
- `data/crosswalks/legacy_dictionaries_index.csv` - Index of fetched dictionaries (incl. unavailable pages)
- `data/crosswalks/legacy_dictionaries_raw/` - 73 per-dictionary parsed CSVs
- `scripts/scrape_legacy_dictionaries.py` - Reproducible scraper for the NCCS catalog

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
- `data/processed/bmf_YYYY_MM_processed.csv` - Transformed columns only
- `data/processed/bmf_YYYY_MM_data_dictionary.csv` - Column metadata and stats
- `data/quality/bmf_YYYY_MM_quality_report.json` - Quality metrics
- `data/geocoding/YYYY_MM/merged/bmf_YYYY_MM_geocoded.parquet` - Geocoded BMF

**S3 upload (if enabled):**
- `intermediate/bmf/YYYY_MM/bmf_YYYY_MM_intermediate.parquet` - All columns
- `intermediate/bmf/YYYY_MM/bmf_YYYY_MM_quality_report.json`
- `processed/bmf/YYYY_MM/bmf_YYYY_MM_processed.csv` - Transformed only
- `processed/bmf/YYYY_MM/bmf_YYYY_MM_data_dictionary.csv` - Column metadata
- `processed/bmf/YYYY_MM/bmf_YYYY_MM_quality_report.json`
- `geocoding/bmf/YYYY_MM/merged/bmf_YYYY_MM_geocoded.parquet` - Geocoded BMF
- `geocoding/bmf/YYYY_MM/merged/bmf_YYYY_MM_geocoded.csv`
- `README.md` - Bucket documentation (uploaded by `upload_s3_readme()`)

**S3 bucket documentation:**
- `data/s3-readme/README.md` - Version-controlled README uploaded to bucket root

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
