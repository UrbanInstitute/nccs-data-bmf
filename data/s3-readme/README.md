# NCCS Data S3 Bucket (`nccsdata`)

This bucket stores IRS Business Master File (BMF) data at various stages of processing.
The BMF contains records for ~1.9 million tax-exempt organizations.

## Data Flow

```
raw/bmf/          intermediate/bmf/       processed/bmf/         geocoding/bmf/
(IRS source) ---> (all columns,      ---> (transformed only, --> (with lat/lon,
                   parquet)                 CSV)                  parquet + CSV)
                  [BMF Pipeline]           [BMF Pipeline]        [Geocoding Workflow]
```

- **BMF Pipeline** (`R/run_pipeline.R`): Phases 1-11 transform raw IRS extracts into
  cleaned, validated data with lookup-table joins and quality reports.
- **Geocoding Workflow** (`R/run_geocoding.R`): A separate, manual workflow that adds
  geographic coordinates by running processed BMF through the Urban Institute geocoder.

## Folder Reference

| Prefix | Stage | Format | Description |
|--------|-------|--------|-------------|
| `raw/bmf/` | Source | CSV | Monthly IRS BMF extracts, ingested by Lambda |
| `intermediate/bmf/YYYY_MM/` | After transform | Parquet | All columns (raw + transformed) for auditing |
| `processed/bmf/YYYY_MM/` | Final output | CSV | Transformed columns only (~77 columns) |
| `geocoding/bmf/YYYY_MM/` | Enrichment | Parquet + CSV | Geocoded BMF with latitude/longitude |
| `legacy/bmf/` | Source (historical) | CSV | NCCS-curated 501CX-NONPROFIT-PX BMF, 1989-2022 |
| `intermediate/bmf-legacy/YYYY_MM/` | After transform | Parquet | Harmonized legacy BMF, full schema |
| `processed/bmf-legacy/YYYY_MM/` | Final output | CSV | Harmonized legacy BMF, slim per-vintage schema |
| `master/bmf/` | Consolidated | Parquet + CSV | One row per EIN across all current+legacy vintages |
| `master/bmf/state_marts/` | Distribution | Parquet + CSV | Geocoded master split into one file per state |

## Which Dataset Should I Use?

- **`processed/bmf/`** -- For most analysis of recent BMF. Contains ~77 cleaned and
  transformed columns with human-readable code definitions. CSV format.
- **`master/bmf/`** -- For "every nonprofit ever observed" workloads (historical
  geocoding, longitudinal coverage, EIN registry). One row per EIN, drawn from the
  most-recent vintage in which the EIN appears across both current and legacy
  pipelines. Includes `first_year_in_bmf` / `last_year_in_bmf` markers.
- **`master/bmf/state_marts/`** -- If you only need a single state or a handful of
  states, pull from here instead of downloading the full ~3 GB geocoded master.
  Same content as the geocoded master, partitioned on `org_addr_state`.
- **`processed/bmf-legacy/`** -- For historical analysis on a specific NCCS legacy
  vintage (1989-2022). Slim per-vintage schema -- only columns whose underlying
  input was populated in that file.
- **`geocoding/bmf/.../merged/`** -- If you need geographic coordinates (latitude,
  longitude, match quality). Builds on processed data.
- **`intermediate/bmf/`** -- Only if you need raw IRS column values alongside
  transformed columns, e.g., for auditing transformations. Parquet format.
- **`raw/bmf/`** -- Original IRS extracts. Use only if you need completely
  unprocessed data.

## Folder Details

### `raw/bmf/`

Monthly IRS BMF extracts deposited by a Lambda function.

- **File pattern:** `YYYY-MM-BMF.csv`
- **Example:** `raw/bmf/2026-03-BMF.csv`
- **Format:** CSV with ~30 raw IRS columns
- **Source:** IRS Exempt Organizations Business Master File

### `intermediate/bmf/YYYY_MM/`

Output of the BMF pipeline with all columns retained (raw IRS columns + transformed columns).
Useful for auditing how raw values were transformed.

- **Files per month:**
  - `bmf_YYYY_MM_intermediate.parquet` -- Full dataset (raw + transformed columns)
  - `bmf_YYYY_MM_quality_report.json` -- Validation metrics and completeness checks
- **Example:** `intermediate/bmf/2026_03/bmf_2026_03_intermediate.parquet`
- **Input:** `raw/bmf/YYYY-MM-BMF.csv`

### `processed/bmf/YYYY_MM/`

Final pipeline output with only transformed columns. This is the primary dataset for analysis.

- **Files per month:**
  - `bmf_YYYY_MM_processed.csv` -- Transformed columns only (~63 columns)
  - `bmf_YYYY_MM_data_dictionary.csv` -- Column metadata with descriptions and stats
  - `bmf_YYYY_MM_quality_report.json` -- Validation metrics and completeness checks
- **Example:** `processed/bmf/2026_03/bmf_2026_03_processed.csv`
- **Input:** `intermediate/bmf/YYYY_MM/` (same pipeline run)

### `geocoding/bmf/YYYY_MM/`

Geocoded BMF data enriched with geographic coordinates. Produced by a separate manual
workflow (`R/run_geocoding.R`) that sends addresses to the Urban Institute geocoder.

- **Sub-folders:**
  - `input/` -- Address batches exported for geocoding
  - `output/` -- Raw geocoder results (uploaded manually after geocoding)
  - `merged/` -- Final geocoded BMF (merged geocoder output back into processed BMF)
- **Files in `merged/`:**
  - `bmf_YYYY_MM_geocoded.parquet` -- Full geocoded dataset
  - `bmf_YYYY_MM_geocoded.csv` -- CSV version
  - `bmf_YYYY_MM_geocoding_quality_report.json` -- Match rates and quality metrics
  - `bmf_YYYY_MM_geocoding_data_dictionary.csv` -- Column metadata
- **Example:** `geocoding/bmf/2026_03/merged/bmf_2026_03_geocoded.parquet`
- **Input:** `processed/bmf/YYYY_MM/bmf_YYYY_MM_processed.csv`

### `legacy/bmf/`

NCCS-curated historical BMF snapshots covering 1989-2022. NCCS-curated column
schema (different from the current IRS BMF) -- the legacy pipeline harmonizes
these to the current schema before running the standard transforms.

- **File pattern:** `BMF-YYYY-MM-501CX-NONPROFIT-PX.csv`
- **Example:** `legacy/bmf/BMF-2010-07-501CX-NONPROFIT-PX.csv`
- **Format:** CSV with NCCS-curated columns

### `intermediate/bmf-legacy/YYYY_MM/` and `processed/bmf-legacy/YYYY_MM/`

Outputs of the legacy BMF pipeline (`R/run_legacy_pipeline.R`). Same structure
as the current `intermediate/bmf/` and `processed/bmf/` prefixes, with one
key difference: `processed/bmf-legacy/` uses a slim per-vintage schema that
includes only columns whose underlying input was populated in that specific
legacy file. The `intermediate/bmf-legacy/` parquet keeps the full schema
for audit.

- **Files per vintage:**
  - `bmf_legacy_YYYY_MM_intermediate.parquet`
  - `bmf_legacy_YYYY_MM_processed.csv`
  - `bmf_legacy_YYYY_MM_data_dictionary.csv`
  - `bmf_legacy_YYYY_MM_quality_report.json`
- **Example:** `processed/bmf-legacy/2010_07/bmf_legacy_2010_07_processed.csv`
- **Input:** `legacy/bmf/BMF-YYYY-MM-501CX-NONPROFIT-PX.csv`

### `master/bmf/`

Master BMF: one row per EIN across all current and legacy vintages. Each row
carries the most-recent vintage's contents plus first/last vintage markers
(`first_vintage_ym`, `last_vintage_ym`, `first_year_in_bmf`,
`last_year_in_bmf`, `bmf_vintages_observed`, `bmf_source`). Built by
`R/run_master_pipeline.R` via DuckDB stack + dedup over the
`processed/bmf/` and `processed/bmf-legacy/` CSVs. Current pipeline wins on
`vintage_ym` ties.

- **Files (single living artifact, overwritten on each rebuild):**
  - `bmf_master.parquet` -- Full schema, zstd-compressed
  - `bmf_master.csv` -- Same rows as CSV
  - `bmf_master_data_dictionary.csv` -- Column metadata
  - `bmf_master_quality_report.json` -- EIN-uniqueness gate, source coverage,
    vintage histogram, completeness
- **Example:** `master/bmf/bmf_master.parquet`
- **Inputs:** `processed/bmf/*/...` and `processed/bmf-legacy/*/...`

### `master/bmf/state_marts/`

Per-state data marts derived from the geocoded Master BMF
(`geocoding/master/merged/bmf_master_geocoded.parquet`). Built so end
users can pull only the state(s) they need instead of the full ~3 GB
unified file. Partition key is `org_addr_state` (cleaned mailing
state); rows with missing state are bucketed into `ZZ`. Built by
`R/run_master_state_marts.R`.

- **Files:**
  - `state_marts/parquet/state=XX/part-0.parquet` -- Hive-partitioned;
    query with `hive_partitioning = 1` in DuckDB / pandas / Athena
  - `state_marts/csv/bmf_master_XX.csv` -- One CSV per state for
    spreadsheet tools and single-file consumers
- **Coverage:** 50 states + DC, US territories (PR, VI, GU, AS, MP),
  APO/FPO codes (AA, AE, AP), Compact-of-Free-Association codes (FM,
  MH, PW), and a `ZZ` missing-state bucket
- **Example:** `master/bmf/state_marts/csv/bmf_master_NY.csv`
- **Input:** `geocoding/master/merged/bmf_master_geocoded.parquet`

## Documentation

Full pipeline documentation: https://urbaninstitute.github.io/nccs-data-bmf/index.html

Source repository: https://github.com/UrbanInstitute/nccs-data-bmf
