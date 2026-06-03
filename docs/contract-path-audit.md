# Contract path audit: `nccs-data-bmf` vs `nccs-contracts` (`bmf-*.yml`)

Contract sources (authoritative defaults):
- [`contracts/bmf-master.yml`](https://github.com/UrbanInstitute/nccs-contracts/blob/main/contracts/bmf-master.yml)
- [`contracts/bmf-master-geocoded.yml`](https://github.com/UrbanInstitute/nccs-contracts/blob/main/contracts/bmf-master-geocoded.yml)
- [`contracts/bmf-lookups.yml`](https://github.com/UrbanInstitute/nccs-contracts/blob/main/contracts/bmf-lookups.yml)
- [`contracts/bmf-legacy.yml`](https://github.com/UrbanInstitute/nccs-contracts/blob/main/contracts/bmf-legacy.yml)
- [`CLAUDE.md` (house rules)](https://github.com/UrbanInstitute/nccs-contracts/blob/main/CLAUDE.md)

Bucket check across all four contracts: **match** (`s3.bucket = nccsdata` aligns with `R/config.R:BMF_S3_BUCKET <- "nccsdata"`).

| contract | contract path (key_prefix / versioned / latest) | code location (file:symbol) | code path | match? | notes |
|---|---|---|---|---|---|
| bmf-master | `key_prefix = master/bmf/` | `R/run_master_pipeline.R:MASTER_S3_PREFIX` | `MASTER_S3_PREFIX <- "master/bmf/"`; uploads use `paste0(MASTER_S3_PREFIX, basename(...))` | yes | S3 path construction is in `run_master_pipeline.R`; `R/master_bmf_builder.R:write_master_outputs` controls filenames. |
| bmf-master | `versioned_template = null` | `R/run_master_pipeline.R:MASTER_S3_PREFIX` | Uploads write directly to `master/bmf/{file}` (no `{YYYY_MM}` subdir, no `latest/` mirror) | yes | Behavior matches contract's current unversioned setting. |
| bmf-master | `latest_template = master/bmf/bmf_master.parquet` | `R/master_bmf_builder.R:write_master_outputs` + `R/run_master_pipeline.R` S3 upload block | Local artifact `bmf_master.parquet` uploaded to `master/bmf/bmf_master.parquet` | yes | Direct overwrite-in-place target. |
| bmf-master-geocoded | `key_prefix = geocoding/bmf-master/merged/` | `R/config.R:BMF_S3_MASTER_GEOCODING_PREFIX`; `R/master_geocoding.R:merge_master_geocoded_results` | `paste0(BMF_S3_MASTER_GEOCODING_PREFIX, "merged/")` where base prefix is `geocoding/bmf-master/` | yes | Code composes the contracted prefix from base constant + `merged/`. |
| bmf-master-geocoded | `versioned_template = null` | `R/master_geocoding.R:merge_master_geocoded_results` | Uploads to `geocoding/bmf-master/merged/{file}` without vintage subdir | yes | Matches unversioned contract state. |
| bmf-master-geocoded | `latest_template = geocoding/bmf-master/merged/bmf_master_geocoded.parquet` | `R/master_geocoding.R:merge_master_geocoded_results` | `parquet_path` basename is uploaded to `.../merged/bmf_master_geocoded.parquet` | yes | Direct overwrite-in-place target. |
| bmf-master-geocoded | (state marts quirk documented in contract notes) | `R/master_state_marts.R:build_master_state_marts` | `s3_root <- "master/bmf/state_marts"` then uploads parquet+csv under that root | yes (expected) | Intentional quirk: contracted under **bmf-master-geocoded** even though prefix is `master/bmf/state_marts/`. |
| bmf-lookups | `key_prefix = lookups/bmf/` | `R/publish_lookups.R:publish_bmf_lookups` | default `s3_prefix = "lookups/bmf/"` | yes | Prefix is explicit function default. |
| bmf-lookups | `versioned_template = lookups/bmf/{vintage}/` | `R/publish_lookups.R:publish_bmf_lookups` | `vintage_prefix <- paste0(s3_prefix, vintage, "/")` with `vintage = format(Sys.Date(), "%Y_%m")` | yes | Contract vintage format (`YYYY_MM`) matches code. |
| bmf-lookups | `latest_template = lookups/bmf/latest/` | `R/publish_lookups.R:publish_bmf_lookups` | `latest_prefix <- paste0(s3_prefix, "latest/")` and mirror uploads to that prefix | yes | Contract and code both maintain `latest/` mirror. |
| bmf-legacy | `key_prefix = processed/bmf-legacy/` | `R/config.R:BMF_S3_LEGACY_PROCESSED_PREFIX`; `R/run_legacy_pipeline.R` phase 11 upload call | `prefix = BMF_S3_LEGACY_PROCESSED_PREFIX` in `upload_processed_bmf(...)` | yes | Contracted consumer-facing surface. |
| bmf-legacy | `versioned_template = processed/bmf-legacy/{vintage}/` | `R/config.R:upload_processed_bmf` | `s3_dir <- sprintf("%s%s_%s/", prefix, year, month)` | yes | `{vintage}` is implemented as `YYYY_MM`. |
| bmf-legacy | `latest_template = null` | `R/config.R:upload_processed_bmf` + `R/run_legacy_pipeline.R` | No `latest/` branch in legacy upload flow | yes | Also uploads `intermediate/bmf-legacy/{YYYY_MM}/...` via `upload_bmf_results`; contract marks intermediate as non-contracted build artifact. |

## Summary of divergences

No direct mismatches were found for `s3.bucket`, `s3.key_prefix`, `s3.versioned_template`, or `s3.latest_template` across the four audited contracts.

Notable contract-aware nuances to keep explicit:
1. **Intentional path quirk (expected):** state marts publish to `s3://nccsdata/master/bmf/state_marts/` and are correctly contracted under `bmf-master-geocoded`.
2. **Scope nuance for legacy:** the code writes both `processed/bmf-legacy/...` and `intermediate/bmf-legacy/...`; the contract intentionally scopes the public surface to `processed/bmf-legacy/...` and treats intermediate as build artifact.
