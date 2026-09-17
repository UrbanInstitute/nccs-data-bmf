# Runbook: yearly refresh of the NTEE code list against the IRS

First written 2026-09-17 (BACKLOG Z18). Owner: whoever runs the BMF pipeline.
Cadence: once a year, in January, and any time a monthly quality report shows
raw NTEE codes the lookup does not recognize.

## Why this exists

The IRS assigns NTEE codes and occasionally adds new ones. Our pipeline only
accepts codes listed in the `ntee_code` sheet of `data/lookup/bmf_code_lookup.xlsx`.
Any raw code missing from that sheet is published as "unknown" (`Z99`) in every
downstream column. In 2026 we found 12 IRS-listed codes missing from the
lookup, about 5,300 organizations in one monthly file. Five were historical
gaps: four present in the data since 1989 (B29 charter schools, F31, M99, P83)
and P76 since 2008. The other seven were added by the IRS in 2021 and appear
in the published files from 2022 onward. Nothing checks this automatically, so
this runbook is the reminder.

## Source of truth

The IRS's own list: Appendix D of the Instructions for Form 1023
(<https://www.irs.gov/instructions/i1023>). It carries a revision date at the
top of the page. The same list appears as Appendix A of the Form 1024-A
instructions; the two were identical when checked in September 2026.

We do not use any third-party copy of the list as the source of truth.

## Steps

1. From the repo root, run:

   ```
   Rscript scripts/refresh_ntee_lookup_from_irs.R
   ```

   It downloads the IRS page, rewrites `data/lookup/irs_ntee_codes.csv` with the
   revision date stamped on every row, and prints two lists: IRS codes missing
   from our lookup, and lookup codes the IRS no longer lists.

2. If `git diff data/lookup/irs_ntee_codes.csv` is empty and both printed lists
   are empty, nothing changed. Commit nothing and stop.

3. For each IRS code missing from the lookup, add a row to the `ntee_code`
   sheet of `data/lookup/bmf_code_lookup.xlsx` with:
   - `ntee_code`: the code.
   - `naics_code`, in this order of preference:
     1. NCCS's own NTEE-to-NAICS crosswalk, `data-raw/NTEE-NAICS-XWALK.csv`
        in the `nccs` website repo. This is where the existing column came
        from (625 of 626 shared codes agree).
     2. If the code is not in that crosswalk: the **2022 NAICS edition** code
        whose title matches the IRS description. Many newer IRS codes are
        named after NAICS industries, so the title search at
        <https://www.census.gov/naics/> (edition selector at the top)
        usually gives an exact match. Keep using the 2022 edition until the
        lookup is deliberately migrated, because codes move between
        editions (pharmacies were 446110 in 2017 and are 456110 in 2022).
     3. Otherwise `UNDEFINED`, as 18 existing rows already are.
   - `ntee_code_definition`: the IRS description, word for word.
   - `effective_date`: today as YYYYMMDD.
   Keep the sheet sorted by code. Editing in Excel is fine; if editing with R,
   use `openxlsx::loadWorkbook()` and keep the sheet order unchanged.

4. Codes the IRS dropped stay in the lookup. Old monthly files still carry
   them, and removing them would turn those rows into "unknown".

5. Run the tests. The test file `tests/testthat/test-ntee-v2.R` checks that
   every code in `irs_ntee_codes.csv` is in the lookup, so it fails until step 3
   is complete.

   ```
   Rscript -e 'testthat::test_dir("tests/testthat")'
   ```

6. Measure how many organizations the new codes affect, per month, with
   `scripts/measure_ntee_lookup_gaps.R` (edit its `GAP_CODES` list first). This
   needs AWS credentials. Put the numbers in the pull request.

7. Open a pull request with the workbook, the CSV, and the numbers. Any change
   to the lookup alters published NTEE columns, so the PR needs an
   `ADR NNNN` line for the contract-change check (ADR 0022).

8. After merge, reprocess every monthly file (both pipelines), then rebuild the
   Unified BMF and the NTEE-resolved crosswalk. The full rerun procedure is in
   `docs/runbooks/2026-06-ntee-fix-rebuild.md` and the ADR 0048 rerun notes.

9. Tell the website maintainers: the "IRS version" NTEE list on nccs.urban.org
   (`data-raw/NTEE-NAICS-XWALK.csv` in the `nccs` repo) needs the same codes.

## Record of refreshes

| Date | IRS revision | Codes added | Codes retired by IRS | Notes |
|---|---|---|---|---|
| 2026-09-17 | Form 1023 instructions 12/2024 | B29, E6A, F31, K2A, K2B, K2C, L4A, L4B, M99, P76, P7A, P83 | A53, Q34, Q36, Q38, Q39, R21, R27, R29, R65 (kept) | First run. 645 IRS codes. |
