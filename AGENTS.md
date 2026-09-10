# AGENTS.md — nccs-data-bmf

Tool-neutral operating guidance for any coding agent working in this
repository. `CLAUDE.md` carries the full command reference and architecture
notes; this file is the short, shared contract for how to work here.

## Repository role

Producer. R pipeline (data.table + arrow + DuckDB) that harmonizes the IRS
Business Master File: current monthly vintages, 85 legacy vintages, the
Unified BMF (one row per EIN), geocoding, per-state marts, geography
crosswalks, the NTEE-resolved crosswalk, and published lookup tables.
Everything it publishes lands under `s3://nccsdata/` at paths described in
`nccs-contracts/contracts/{bmf-lookups,unified-bmf,unified-bmf-geocoded,
ntee-resolved-crosswalk,...}.yml`. Those YAMLs are authoritative.

## Governing documents

- The ADR named in the task (in `nccs-contracts/decisions/`). Read it and
  everything it `Relates:` to before editing.
- The contract YAML(s) for any artifact the change touches.
- `CLAUDE.md` here for pipeline phases, key files, and run commands.
- NTEE specifics: `R/transform_ntee_code.R`, the vendored lookups in
  `data/lookup/`, ADR 0032 and ADR 0048, and the public spec at
  `nccs/_resources/ntee.md`.

## Commands

```bash
# Tests (introduced by ADR 0048; keep green)
Rscript -e 'testthat::test_dir("tests/testthat")'

# Pipelines: see CLAUDE.md "Commands". Local runs read from S3 with
# profile thiya; nothing publishes unless the publish step is invoked.
eval "$(aws configure export-credentials --profile thiya --format env)"
```

## Data-integrity requirements

- Transforms are pure functions of validated inputs; a derived column has
  exactly one derivation path (ADR 0032 invariant). Do not add a parallel
  formula.
- Every pipeline emits a quality report and an ADR 0014 `_manifest.json`
  (git SHA, input hashes, row counts, sha256). A change that alters values
  must show before/after counts on the affected column(s) and per-column
  hashes proving the others are untouched.
- Row membership (`ein` set) never changes as a side effect of a value fix.
- Legacy vintages may carry pre-2003 5-char NTEE codes; the vendored
  `ntee_legacy_5char_lookup.csv` is the oracle for their V2 rendering.

## Change routing

- Column added/renamed/removed, path changed, or semantics of a published
  column changed: stop and route to `nccs-contracts` for an ADR unless one
  already authorizes it. Put `ADR NNNN` in the commit message or PR body
  (contracts-guard CI, ADR 0022).
- `:=` inside data.table shadows same-named function arguments; never name
  a parameter after a column.

## Production safety

- AWS profile `thiya`; read-only by default. Publishing (`v{YYYY_MM}/`
  then `latest/`, ADR 0042 / Z13 publisher) requires a distinct instruction
  naming bucket, prefix, vintage, and expected files. Never modify prior
  vintages.
- Merge-ready and publish-ready are separate gates.

## Evidence discipline

Label claims: verified / inferred / remembered / unverified / contradicted /
dependent on another agent's report. State the verification level of every
data check (code-only, local artifact, live S3 object by name).

## Completion criteria

Tests pass; quality report and manifest regenerated for any rebuilt
artifact; before/after evidence attached; contract YAML notes updated or a
reconcile item filed in `nccs-contracts`; ADR breadcrumb present; nothing
published without the separate authorization.
