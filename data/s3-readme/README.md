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
| `processed/bmf/YYYY_MM/` | Final output | CSV | Transformed columns only (~63 columns) |
| `geocoding/bmf/YYYY_MM/` | Enrichment | Parquet + CSV | Geocoded BMF with latitude/longitude |

## Which Dataset Should I Use?

- **`processed/bmf/`** -- For most analysis. Contains ~63 cleaned and transformed columns
  with human-readable code definitions. CSV format.
- **`geocoding/bmf/.../merged/`** -- If you need geographic coordinates (latitude, longitude,
  match quality). Builds on processed data.
- **`intermediate/bmf/`** -- Only if you need raw IRS column values alongside transformed
  columns, e.g., for auditing transformations. Parquet format.
- **`raw/bmf/`** -- Original IRS extracts. Use only if you need completely unprocessed data.

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

## Documentation

Full pipeline documentation: https://urbaninstitute.github.io/nccs-data-bmf/index.html

Source repository: https://github.com/UrbanInstitute/nccs-data-bmf
