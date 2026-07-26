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

The geocoder is Urban's **automated, S3-event-driven service**: submitting a
batch means putting a CSV (with a single-line `f_address` column) plus a form
JSON in `s3://geocoding-codestar-prod/data/input-data/` and `data/form-data/`;
a Lambda auto-starts the ArcGIS engine instance, results appear under
`data/output-data/` (FIFO), and the instance shuts itself down. No manual
activation anywhere. Full mechanics: `docs/reference/geocoder-service.md`.

```r
# Phase 1: Export address batches for geocoding
GEOCODING_MODE <- "export"
source("R/run_geocoding.R")

# ... submit batches to the geocoder service, retrieve outputs (see above) ...

# Phase 2: Merge geocoded results back into BMF
GEOCODING_MODE <- "merge"
source("R/run_geocoding.R")
```

### Run the Legacy BMF Pipeline
For NCCS legacy 501CX-NONPROFIT-PX BMF files (1989–2022 vintages). These
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

### Build the Master / Unified BMF
The Master BMF (being renamed the **Unified BMF**, ADR 0037) is a
single-row-per-EIN consolidation of every nonprofit ever observed across
both pipelines (current monthly + legacy). Each row carries the
most-recent vintage's contents plus `first_vintage_ym`, `last_vintage_ym`,
`first_year_in_bmf`, `last_year_in_bmf`, `bmf_vintages_observed`, and
`bmf_source`. Built via DuckDB `union_by_name` over the processed CSVs of
both pipelines; current wins on vintage_ym ties.

It also carries the additive coercion-safe EIN columns `ein_prefixed`
(`ein-XX-XXXXXXX`) and `EIN2` (`EIN-XX-XXXXXXX`) derived from the
unchanged canonical `ein` (ADR 0036; SQL mirror of `ein_to_prefixed()` /
`ein_to_ein2()` in `R/ein.R`), and emits a per-build ADR 0014
`_manifest.json` (commit, input hashes, row counts).

```r
source("R/run_master_pipeline.R")
```

Or on EC2:

```bash
bash scripts/run_master.sh
```

Outputs land in `data/master/` and upload to `s3://nccsdata/unified/`
(`UNIFIED_S3_PREFIX`, default stem `bmf_unified`), **superseding**
`s3://nccsdata/master/bmf/` which stays reachable for a 90-day window
then archives (ADR 0037; never a silent move). **The exact unified
path/filenames are pending nccs-contracts ratification** —
`run_master_pipeline.R` carries the proposed defaults. See
`docs/11-master-bmf.qmd` for the full design.

### Build the per-state Data Marts
Splits the geocoded Master BMF into one parquet partition + one CSV
per US state/territory so end users can pull only the rows they need.
Partition key is `org_addr_state`; rows with missing state are
bucketed into `ZZ`.

```r
source("R/run_master_state_marts.R")
```

Outputs land in `data/master/state_marts/{parquet,csv}/` and upload to
`s3://nccsdata/master/bmf/state_marts/`. See `docs/12-state-marts.qmd`
for the full design.

### Build the County FIPS Crosswalk
Maps the geocoder's dirty county labels (`geo_county`) to canonical
Census county identity (5-char FIPS GEOID + `NAMELSAD`) so consumers can
canonicalize names and filter by a collision-proof key (e.g. Baltimore
city `24510` vs Baltimore County `24005`). Published as a **separate**
artifact — FIPS columns are deliberately NOT added to the Master BMF
(ADR 0016; avoids pinning consumers to a TIGER vintage). `sf`/`tigris`
are isolated to these scripts, never in pipeline runtime.

```bash
# One-time single S3 read of the geocoded master -> local cache:
eval "$(aws configure export-credentials --profile thiya --format env)"
Rscript scripts/read_county_points.R
# Resolve via TIGER name match + org-mass point-in-polygon:
Rscript scripts/build_county_fips_crosswalk.R        # TIGER_YEAR=2023 default
```

Outputs `data/crosswalks/county_fips_crosswalk.parquet` (one row per
`(geo_state_abbr, geo_county_raw)`; ~3,635 rows) plus a
`*_audit.csv` of the genuinely ambiguous/unresolved labels (independent
city vs namesake county, CT planning-region change, wrong-state source
labels). Publish with `source("R/publish_county_fips_crosswalk.R")`.
See `docs/13-county-fips-crosswalk.qmd` for the full design.

### Build the CBSA Crosswalk
Derived from the county FIPS crosswalk: maps each resolved county GEOID
to its OMB Core-Based Statistical Area (metropolitan/micropolitan). Uses
the authoritative OMB July-2023 delineation (Census "List 1"), same
geography vintage as TIGER 2023 (incl. CT planning regions). Consumers
chain the two: raw label → `county_fips` → CBSA.

```bash
Rscript scripts/build_cbsa_crosswalk.R   # DELINEATION_YEAR=2023 default
```

Outputs `data/crosswalks/cbsa_crosswalk.parquet` (one row per resolved
county GEOID; CBSA columns NA for rural counties) + `*_audit.csv`.
Publish with `source("R/publish_cbsa_crosswalk.R")`. See
`docs/14-cbsa-crosswalk.qmd`. The universe also folds in the nine CT
planning-region GEOIDs from the CT companion (below), so the CT chain
completes and the audit lists zero delineation counties absent from BMF.

### Build the CT Planning-Region Crosswalk
Connecticut companion: Census retired CT's 8 historical counties for 9
planning regions (`09110`–`09190`) in 2022, but the geocoder still emits
old `<name> County` labels that each span multiple regions — so they cannot
be resolved at the `(state, county)` grain. The county crosswalk marks all 8
as `deferred_ct_planning_region`; this artifact resolves them by **coordinate**
instead. A dense 0.01° lookup grid over CT (built purely from TIGER 2023, no
S3 read, like CBSA derives from OMB); consumers round `geo_lat`/`geo_lon` to
0.01° and join on `(lat2, lon2)`, then chain CBSA. Keeps the master FIPS-free
(ADR 0016).

```bash
Rscript scripts/build_ct_planning_region_crosswalk.R   # TIGER_YEAR=2023 default
```

Outputs `data/crosswalks/ct_planning_region_crosswalk.parquet` (one row per
CT-land cell; `geo_county_fips`/`geo_county_canonical` mirror the county
crosswalk, plus `lat2`/`lon2` keys + `area_share` + `straddle` flag) +
`*_audit.csv` (straddle/boundary cells). Publish with
`source("R/publish_ct_planning_region_crosswalk.R")`. See
`docs/15-ct-planning-region-crosswalk.qmd`. Rebuilding this requires
rebuilding the CBSA crosswalk afterward (it folds these GEOIDs in).

### Build the NTEE-resolved crosswalk
Per-EIN lookup that recovers a usable NTEE classification for orgs whose
`NTEE_CD` is blank in the current BMF but was populated in an earlier vintage
(motivating case: Carnegie Mellon 25-0969449 — empty in current, `B43` in
legacy). Aggregates the **raw** NTEE code (vintage-invariant, so no legacy
reprocess) for each EIN across every vintage of both pipelines, cleans the
distinct codes once via the fixed `transform_ntee_code()`, and exposes four
views — `ntee_current` (may be NULL), `ntee_most_recent`, `ntee_modal`, and
`ntee_code_distribution` (JSON) — each with `_subsector`/`_nteev2` plus
`n_distinct_codes`/`n_vintages_with_ntee`/`ntee_agreement`. "Expose all, no
opinionated pick" (ADR 0034); a separate join layer, NOT columns on the
master (ADR 0016).

```bash
eval "$(aws configure export-credentials --profile <profile> --format env)"
Rscript scripts/build_ntee_resolved_crosswalk.R
```

Reads only `(ein, ntee_code_raw)` from the all-columns intermediate parquets
of both pipelines via DuckDB `httpfs` — column projection pulls a small slice
of the ~36 GB, so it runs locally (no EC2). The heavy `GROUP BY` spills to
disk (the script sets `temp_directory` + `preserve_insertion_order=false` +
env-overridable `DUCKDB_MEMORY_LIMIT`/`DUCKDB_THREADS`) and `INSTALL`/`LOAD`s
the DuckDB `aws` extension (the `credential_chain` S3 secret needs it).
Outputs `data/crosswalks/ntee_resolved_crosswalk.{parquet,csv}` (the 640 MB
CSV is gitignored — distributed via S3 only). Publish with
`source("R/publish_ntee_resolved_crosswalk.R"); publish_ntee_resolved_crosswalk()`.
See `docs/16-ntee-resolved-crosswalk.qmd`. Rebuild after each new monthly
current BMF so `ntee_current` tracks the newest vintage.

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
- `R/utils/render_quality_report_index.R` - Regenerates `docs/quality-reports/index.html` from files on disk; called by every pipeline after rendering its HTML quality report

**Geocoding Workflow:**
- `R/run_geocoding.R` - Per-month geocoding orchestrator (export/merge modes)
- `R/geocoding_export.R` - Prepare address batches for Urban geocoder
- `R/geocoding_merge.R` - Merge geocoded results back into processed BMF
- `R/run_master_geocoding.R` - Master BMF geocoding orchestrator (export/merge modes)
- `R/master_geocoding.R` - Address dedup + batch export and merge for the Master BMF
- `R/quality/geocoding_checks.R` - Geocoding quality validation

**Quality Gates:**
- `R/quality/pre_checks.R` - Pre-transformation validation (defines `BMF_REQUIRED_COLUMNS` and `BMF_LEGACY_MIN_COLUMNS`)
- `R/quality/post_checks.R` - Post-transformation quality reporting
- `R/quality/legacy_pre_checks.R` - Relaxed pre-validation for legacy BMF mode

**Legacy BMF Harmonization (501CX-NONPROFIT-PX, 1989–2022):**
- `R/run_legacy_pipeline.R` - Legacy orchestrator (mirrors `run_pipeline.R` with harmonization at Phase 1.5 and slim Phase 11 output)
- `R/legacy_bmf_adapter.R` - `harmonize_legacy_bmf()`, `compute_legacy_output_columns()`, crosswalk loader (incl. truncated-parse sanity check)
- `data/crosswalks/XWALK-BMF-V2.0.csv` - Legacy → current schema mapping (canonical; 78 rows)
- `data/crosswalks/legacy_column_inventory.csv` - Long-format inventory of all 47 columns observed across 73 scraped dictionaries
- `data/crosswalks/legacy_dictionaries_index.csv` - Index of fetched dictionaries (incl. unavailable pages)
- `data/crosswalks/legacy_dictionaries_raw/` - 73 per-dictionary parsed CSVs
- `data/lookup/ntee_legacy_5char_lookup.csv` - Vendored NCCS pre-2003 5-char NTEE → NTEEv2 crosswalk (1,597 rows)
- `scripts/scrape_legacy_dictionaries.py` - Reproducible scraper for the NCCS catalog
- `scripts/check_ntee_legacy_coverage.R` - Diagnostic for legacy 5-char NTEE crosswalk coverage

**Master BMF (one row per EIN, all vintages combined):**
- `R/run_master_pipeline.R` - DuckDB-based orchestrator
- `R/master_bmf_builder.R` - Discovery, stack via `union_by_name`, dedup with window function (current wins on vintage_ym ties)
- `R/quality/master_post_checks.R` - Master-specific quality report (EIN-uniqueness gate, source coverage, vintage histogram, completeness)

**Per-state Data Marts (geocoded master split by state):**
- `R/run_master_state_marts.R` - Orchestrator
- `R/master_state_marts.R` - `build_master_state_marts()`: Hive-partitioned parquet + per-state CSV writer

**County FIPS, CBSA & CT Planning-Region Crosswalks (geocoder county labels → Census geography):**
- `scripts/read_county_points.R` - Single S3 read of the geocoded master → local point cache
- `scripts/build_county_fips_crosswalk.R` - `sf`/`tigris` resolution (TIGER name match + org-mass point-in-polygon); writes the county crosswalk parquet/csv + audit. CT `<name> County` labels → `deferred_ct_planning_region` (resolved by coordinate via the CT companion, not by name)
- `scripts/build_cbsa_crosswalk.R` - Derives county→CBSA from the county crosswalk + OMB List 1 delineation; folds the CT companion's 9 planning-region GEOIDs into the universe
- `scripts/build_ct_planning_region_crosswalk.R` - Dense 0.01° CT lookup grid (TIGER 2023 only, no S3); coordinate → planning-region GEOID + `straddle` flag
- `R/publish_crosswalk.R` - Generic crosswalk publisher (parquet + csv + ADR 0014 manifest, idempotent)
- `R/publish_county_fips_crosswalk.R` / `R/publish_cbsa_crosswalk.R` / `R/publish_ct_planning_region_crosswalk.R` - Thin wrappers → `s3://nccsdata/crosswalks/{county-fips,cbsa,ct-planning-region}/`
- `data/crosswalks/county_fips_crosswalk.{csv,parquet}` + `_audit.csv` - County artifact (ambiguous/unresolved/deferred labels audited)
- `data/crosswalks/cbsa_crosswalk.{csv,parquet}` + `_audit.csv` - CBSA artifact (rural tally + delineation counties absent from BMF)
- `data/crosswalks/ct_planning_region_crosswalk.{csv,parquet}` + `_audit.csv` - CT companion (coordinate grid; straddle cells audited)

**NTEE-resolved crosswalk (per-EIN NTEE recovered across all vintages):**
- `scripts/build_ntee_resolved_crosswalk.R` - DuckDB aggregate of raw NTEE across both pipelines' intermediate parquets; cleans distinct codes via `transform_ntee_code()`; writes per-EIN current/most-recent/modal/distribution views. Runs locally (column projection + disk spill); no EC2
- `R/publish_ntee_resolved_crosswalk.R` - Thin wrapper over `R/publish_crosswalk.R` → `s3://nccsdata/crosswalks/ntee-resolved/`
- `data/crosswalks/ntee_resolved_crosswalk.parquet` - Local artifact (CSV sibling is ~640 MB, gitignored; both distributed via S3)

**EC2 batch scripts:**
- `scripts/setup_ec2.sh` - One-shot bootstrap (R, system libs, AWS CLI, Quarto, R packages)
- `scripts/run_all_legacy.sh` - Serial/parallel driver for every legacy vintage (`JOBS=N`, `SKIP_VINTAGES`, `SKIP_EXISTING`)
- `scripts/run_master.sh` - Driver for the master BMF build

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

## Published artifacts

Some artifacts in this repo are part of a stable contract with downstream
consumers. Treat their S3 paths as a public API: do not rename, do not move,
and re-publish whenever the local source changes.

**Contract-change guard (ADR 0022).** A PR that touches what/where this repo
publishes — or the schema/manifest shape — must acknowledge the
[`nccs-contracts`](https://github.com/UrbanInstitute/nccs-contracts) impact, or
CI fails. The `.github/workflows/contracts-guard.yml` caller (a thin wrapper over
the reusable guard in `nccs-contracts`) fires on PRs that change
`R/publish_*.R`, `R/run_*.R`, `R/master_*.R`, `R/config.R`, `R/manifest.R`, or
`scripts/build_*.R`. To pass: add an `ADR NNNN` breadcrumb to a commit message
or the PR body and queue the `nccs-contracts` reconcile, **or** add the
`contracts-ack` label if there is genuinely no contract impact. The guard checks
*acknowledgment, not correctness*. Keep the caller's `paths_regex` in sync with
`nccs-contracts/scripts/reconcile.sh`. See `nccs-contracts/CONTRIBUTING.md`.

### Geography crosswalks → S3

Path contract (each prefix holds `*.parquet` + `*.csv` + ADR 0014 `_manifest.json`):

- `s3://nccsdata/crosswalks/county-fips/` — geocoder county label →
  county FIPS GEOID + canonical `NAMELSAD` (TIGER 2023). Built by
  `scripts/build_county_fips_crosswalk.R` from the geocoded master.
- `s3://nccsdata/crosswalks/cbsa/` — county FIPS → CBSA (metro/micro).
  Derived from the county crosswalk + OMB 2023 delineation by
  `scripts/build_cbsa_crosswalk.R`.
- `s3://nccsdata/crosswalks/ct-planning-region/` — CT coordinate
  (`lat2`, `lon2`) → planning-region GEOID. Built from TIGER 2023 by
  `scripts/build_ct_planning_region_crosswalk.R`. Resolves CT, whose
  old-county labels are `deferred_ct_planning_region` in the county
  crosswalk. CT chain: raw coord → planning region → CBSA.

Consumers join these themselves (ADR 0016 consumer-composes); FIPS/CBSA
columns are deliberately NOT added to the Master BMF (avoids pinning a
TIGER vintage). All publish via `R/publish_crosswalk.R` (idempotent
sha256). **Maintenance rule**: re-run the matching
`R/publish_{county_fips,cbsa,ct_planning_region}_crosswalk.R` after
rebuilding any local artifact, and keep all three on the same geography
vintage (TIGER year ↔ OMB delineation year) so GEOIDs match. The three
are coupled: rebuilding the CT companion or the county crosswalk requires
rebuilding the CBSA crosswalk afterward (its universe folds in both).

### NTEE-resolved crosswalk → S3

Path contract (prefix holds `*.parquet` + `*.csv` + ADR 0014 `_manifest.json`):

- `s3://nccsdata/crosswalks/ntee-resolved/` — per-EIN NTEE recovered across
  every vintage of both pipelines. Built by
  `scripts/build_ntee_resolved_crosswalk.R` from the intermediate parquets;
  published via `R/publish_ntee_resolved_crosswalk.R`. Consumers join by
  `ein` (ADR 0016 consumer-composes; NTEE fields are deliberately NOT added
  to the Master BMF). Also carries the additive coercion-safe EIN columns
  `ein_prefixed` + `EIN2` (ADR 0036; from the unchanged canonical `ein` via
  `R/ein.R`). **Maintenance rule**: rebuild + re-publish after each
  new monthly current BMF so `ntee_current` tracks the newest vintage; the
  legacy half is static. The manifest records the input prefixes and the
  sha256 of `transform_ntee_code.R` + the legacy 5-char lookup, so a change
  to the cleaner or that lookup is visible in published provenance. See ADR
  0034 and `docs/16-ntee-resolved-crosswalk.qmd`.

### BMF lookup tables → S3

Path contract:

- `s3://nccsdata/lookups/bmf/{YYYY_MM}/{lookup_name}.csv` — vintage snapshot
- `s3://nccsdata/lookups/bmf/{YYYY_MM}/MANIFEST.json` — file list, row counts,
  column names, sha256, byte size
- `s3://nccsdata/lookups/bmf/latest/...` — mirror of the most recent vintage

The 17 published tables come from two on-disk sources, both of which feed
`lookup_ls` in `R/config.R` (the single source of truth):

| Source | Tables |
| --- | --- |
| `data/lookup/bmf_code_lookup.xlsx` (every sheet) | `status_code`, `asset_code`, `filing_requirement_code`, `income_code`, `pf_filing_requirement_code`, `ntee_code`, `ntee_common_code`, `ntee_code_major_group`, `parent_organization`, `subsection_classification_code`, `affiliation_code`, `organization_code`, `activity_code`, `deductibility_code`, `nteev2_subsector`, `foundation_code` |
| `data/lookup/ntee_legacy_5char_lookup.csv` | `ntee_legacy_5char` |

How it runs:

- As Phase 7 of `R/run_master_pipeline.R` (automatic when `ENABLE_S3_UPLOAD = TRUE`).
- Or standalone: `source("R/run_publish_lookups.R")` — supports
  `PUBLISH_DRY_RUN <- TRUE` and `PUBLISH_VINTAGE <- "YYYY_MM"`.

Implementation: `R/publish_lookups.R`. Idempotency is manifest-driven —
each file's sha256 is compared against the existing remote `MANIFEST.json`
and unchanged files are skipped (no `aws s3 sync`; all S3 traffic in this
repo uses `aws.s3::put_object` for consistency).

**Maintenance rule**: any edit to `data/lookup/bmf_code_lookup.xlsx` or
`data/lookup/ntee_legacy_5char_lookup.csv` requires re-running the publish
step (`source("R/run_publish_lookups.R")`) to refresh S3. Otherwise
downstream consumers will silently go stale against the new local copy.

**Downstream consumer**: the sibling `../nccsdata/` R package pulls these
via its own `data-raw/build_lookups.R` and bundles them as internal package
data. nccsdata does *not* import from this repo directly — S3 is the
contract surface.

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

## Shared workstream context

This is one of the NCCS core data repos. The shared architecture &
data-engineering doctrine (change management, the machinery-enforced quality
bar, canonical-mapping discipline, source/geography rules) is single-sourced in
the home Claude folder and imported here:

@~/.claude/nccs-architecture-context.md
