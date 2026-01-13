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

### Key Files
- `R/run_pipeline.R` - Main orchestration script that sequences all transformations (10 phases)
- `R/config.R` - S3 configuration, download functions, and lookup table initialization
- `R/checkpoints.R` - Save/load intermediate pipeline states to parquet
- `R/transform_*.R` and `R/*_code.R` - Individual field transformation functions
- `R/quality/pre_checks.R` and `R/quality/post_checks.R` - Validation quality gates

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
