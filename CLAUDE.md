# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This repository contains code for harmonizing IRS Business Master File (BMF) data - extracts of nonprofit organizations exempt from federal income tax. The pipeline:
1. Downloads and combines 4 regional BMF CSVs from the IRS
2. Transforms raw fields through standardized cleaning functions
3. Creates lookup table joins for code definitions
4. Outputs a unified BMF consolidating ~1.9M nonprofit records

**Documentation**: https://urbaninstitute.github.io/nccs-data-bmf/00-documentation/GUIDEBOOK/index.html

## Commands

### Run the Data Pipeline
```r
# In R/RStudio - sources config first, then runs extraction and transformation
source("R/config.R")
source("R/00_irs_bmf.R")
```

### Build Documentation
```bash
# Generate HTML guidebook from Quarto (run from 00-documentation/)
cd 00-documentation && quarto render
```

## Architecture

### Data Flow
```
IRS BMF (4 regional CSVs) → Download → Combine → Transform → Unified BMF
```

### Key Files
- `R/00_irs_bmf.R` - Main orchestration script that sequences all transformations
- `R/config.R` - IRS URLs, file paths, and lookup table initialization from `data/lookup/bmf_code_lookup.xlsx`
- `R/transform_*.R` and `R/*_code.R` - Individual field transformation functions

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
- `openxlsx` - Excel lookup file reading
- `here` - Project-relative paths
- `purrr` - Functional iteration
- `stringr` - String manipulation
- `lubridate` - Date parsing
- `validate` - Data validation rules (in `R/validate.R`)

## Data Sources

IRS BMF regional endpoints (configured in `config.R`):
- eo1.csv - Northeast
- eo2.csv - Mid-Atlantic
- eo3.csv - Gulf and Pacific Coast
- eo4.csv - International/PR

## Conventions

- Use `here::here()` for all file paths
- Transformation functions should be pure (copy input, don't modify in place)
- New field transforms go in dedicated `R/<field_name>.R` files
- Add corresponding lookup data to Excel workbook or CSV
- Call new transforms from `00_irs_bmf.R` in logical sequence
