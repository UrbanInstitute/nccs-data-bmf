# nccs-data-bmf

Code and documentation for harmonizing IRS Business Master File (BMF) data —
extracts of the ~1.9 million nonprofit organizations exempt from federal income
tax. This repo ingests both the **current monthly IRS BMF** and **NCCS legacy
BMF** vintages, harmonizes them to a single schema, geocodes them, and
consolidates them into a **Master BMF** with one row per EIN.

**Documentation:** [BMF Research Guidebook](https://urbaninstitute.github.io/nccs-data-bmf/index.html)

## What this repo produces

The repository contains four pipelines plus a geocoding workflow. Each writes
to `data/` locally and (when `ENABLE_S3_UPLOAD = TRUE`) uploads to the
`nccsdata` S3 bucket.

| Pipeline | Orchestrator | What it does | S3 output prefix |
|----------|-------------|--------------|------------------|
| **Current BMF** | `R/run_pipeline.R` | Transforms a monthly IRS BMF extract through 11 phases into cleaned, validated columns | `processed/bmf/YYYY_MM/` |
| **Legacy BMF** | `R/run_legacy_pipeline.R` | Harmonizes an NCCS 501CX-NONPROFIT-PX vintage (1989–2022) to the current schema, then runs the same transforms | `processed/bmf-legacy/YYYY_MM/` |
| **Master BMF** | `R/run_master_pipeline.R` | Consolidates every processed vintage (current + legacy) into one row per EIN | `master/bmf/` |
| **State marts** | `R/run_master_state_marts.R` | Splits the geocoded Master BMF into one file per US state/territory | `master/bmf/state_marts/` |
| **Geocoding** | `R/run_geocoding.R`, `R/run_master_geocoding.R` | Two-phase (export → merge) workflow that appends lat/lon + FIPS via the Urban Institute geocoder | `geocoding/bmf/YYYY_MM/`, `geocoding/bmf-master/` |

## Data flow

```
                                  ┌─────────────────────────┐
  raw/bmf/  (monthly IRS) ──────► │  Current BMF pipeline    │ ──► processed/bmf/YYYY_MM/
                                  └─────────────────────────┘            │
                                                                         ▼
  legacy/bmf/ (NCCS 501CX) ─────► ┌─────────────────────────┐     ┌──────────────┐
                                  │  Legacy BMF pipeline     │ ──► │ Master BMF   │ ──► master/bmf/
                                  └─────────────────────────┘     │ (1 row/EIN)  │
  processed/bmf-legacy/YYYY_MM/ ───────────────────────────────► └──────────────┘
                                                                         │
                                                                geocode + split
                                                                         ▼
                                                              master/bmf/state_marts/
```

## Harmonized metadata fields

The transforms standardize raw IRS/NCCS columns into ~76 cleaned, validated
columns with human-readable code definitions joined from lookup tables. Notably:

- **`ein`** — normalized to the standard `XX-XXXXXXX` string format
  (see `R/ein.R`) so it survives CSV round-trips without being coerced to an
  integer.
- **`NTEEV2`** — derived from the reported NTEE code column (`NTEE_CD`) into the
  NTEEv2 classification, including subsector and organization-type components
  (see `R/transform_ntee_code.R`). Legacy pre-2003 5-character NTEE codes are
  crosswalked via `data/lookup/ntee_legacy_5char_lookup.csv`.

## The Master BMF

The Master BMF is the consolidated, one-row-per-EIN view of every nonprofit
ever observed across both the current monthly and legacy pipelines. It is built
with DuckDB (`union_by_name` stack + dedup) over the processed CSVs of both
pipelines; the most-recent vintage wins, with the current pipeline winning on
`vintage_ym` ties. Each row carries the latest vintage's contents plus
provenance markers:

`first_vintage_ym`, `last_vintage_ym`, `bmf_vintage_ym`, `first_year_in_bmf`,
`last_year_in_bmf`, `bmf_vintages_observed`, `bmf_source`.

The Master BMF carries **86 columns**: the `union_by_name` superset of the
transformed columns across all stacked vintages (slightly wider than the
~76 single-vintage schema), plus the 7 lineage columns above. It is rebuilt
from scratch each run (a single living artifact, overwritten in place), not
updated incrementally. See [docs/11-master-bmf.qmd](docs/11-master-bmf.qmd).

## Running the pipelines

AWS credentials must be configured (env vars or `~/.aws/credentials`). Run from
the project root in R/RStudio.

### Current monthly BMF

```r
# Most recent BMF in S3 (default):
source("R/run_pipeline.R")

# A specific month:
BMF_YEAR <- 2026
BMF_MONTH <- 3
source("R/run_pipeline.R")

# List available months:
source("R/config.R"); list_available_bmf_files()
```

Pipeline flags in `run_pipeline.R`: `ENABLE_CHECKPOINTS`, `STRICT_QUALITY_GATES`,
`ENABLE_S3_UPLOAD`, `CHECKPOINT_DIR`.

### Legacy BMF (1989–2022)

```r
# A specific vintage from S3:
LEGACY_BMF_YEAR <- 2013; LEGACY_BMF_MONTH <- 7
source("R/run_legacy_pipeline.R")

# Or a local file (skips S3 download):
LEGACY_BMF_FILE <- "data/raw/legacy/BMF-2013-07-501CX-NONPROFIT-PX.csv"
source("R/run_legacy_pipeline.R")
```

To batch every legacy vintage on EC2, see `scripts/run_all_legacy.sh` and
[docs/10-ec2-batch-processing.qmd](docs/10-ec2-batch-processing.qmd).

### Master BMF and state marts

```r
source("R/run_master_pipeline.R")       # build the Master BMF
source("R/run_master_state_marts.R")    # split geocoded master by state
```

Or on EC2: `bash scripts/run_master.sh`.

### Geocoding

```r
# Phase 1 — export address batches:
GEOCODING_MODE <- "export"; source("R/run_geocoding.R")
# ... upload to the Urban geocoder, download results ...
# Phase 2 — merge geocoded results back in:
GEOCODING_MODE <- "merge";  source("R/run_geocoding.R")
```

The Master BMF has its own geocoding orchestrator, `R/run_master_geocoding.R`,
following the same export/merge pattern.

## Outputs

Local outputs land under `data/` (`processed/`, `intermediate/`, `master/`,
`geocoding/`, `quality/`); the same artifacts upload to the `nccsdata` S3
bucket when `ENABLE_S3_UPLOAD = TRUE`. The bucket layout and "which dataset
should I use?" guidance live in the bucket's own README
([`data/s3-readme/README.md`](data/s3-readme/README.md)), which is published to
the bucket root.

## Published lookup tables (downstream contract)

The 17 BMF code-definition lookup tables are published to S3 as a stable,
versioned contract for downstream consumers (notably the sibling `nccsdata` R
package):

- `s3://nccsdata/lookups/bmf/{YYYY_MM}/...` — vintage snapshot + `MANIFEST.json`
- `s3://nccsdata/lookups/bmf/latest/...` — mirror of the most recent vintage

Publishing runs automatically as the final phase of `run_master_pipeline.R`, or
standalone via `source("R/run_publish_lookups.R")`. **Any edit to
`data/lookup/bmf_code_lookup.xlsx` or `data/lookup/ntee_legacy_5char_lookup.csv`
requires re-running the publish step**, or downstream consumers go stale. See
the "Published artifacts" section of `CLAUDE.md` for the full path contract.

## Documentation

- [BMF Research Guidebook](https://urbaninstitute.github.io/nccs-data-bmf/index.html) — full pipeline documentation (Quarto)
- [Latest Quality Report](https://urbaninstitute.github.io/nccs-data-bmf/quality-reports/index.html)
- [IRS BMF Extract source](https://www.irs.gov/charities-non-profits/exempt-organizations-business-master-file-extract-eo-bmf)

Build the guidebook locally: `cd docs && quarto render`.
