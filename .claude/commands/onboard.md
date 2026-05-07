Run the standard onboard procedure (read `CLAUDE.md`, glob and read the docs, read the pipeline orchestrators, check `git log`/`status`, review auto-memory) and produce a session briefing.

In addition, treat these two BMF-specific reference docs as **must-reads** before producing the briefing — they're domain authority needed to reason about transformations and validation:

- `docs/reference/irm_25_007_001.md` — IRS IRM 25.7.1: authoritative BMF field definitions, code values, validation rules.
- `docs/reference/nccs_data_guide.md` — NCCS Data Guide: data quality caveats, NTEE classification, organizational categories, practical research context.

This repo has three pipeline orchestrators (current monthly, legacy 501CX, master), not one. Read all three when surveying the orchestration layer:

- `R/run_pipeline.R`
- `R/run_legacy_pipeline.R`
- `R/run_master_pipeline.R`
