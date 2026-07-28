# Address-data invariants (rules for any address-related operation)

Codified 2026-07-26 after the address-log zip-format incident, extended
2026-07-28 after the leading-zero finding. These apply to the
address-resolved crosswalk, the geocoding export/merge, and any future
operation that joins or aggregates addresses across the two BMF pipelines.

## Vocabulary: what a "spell" is

A spell is one row of the address-resolved crosswalk: a single organization
observed at a single distinct address tuple. `spell_rank` orders an EIN's
addresses by recency, `0` = the most recent, `1` = the one before it, so
`spell_rank == 0` reproduces the one-row-per-EIN Unified BMF view and higher
ranks are the address history.

It is an address **tenure** aggregated over every observation of that tuple,
not a contiguity-checked survival spell: an organization that moved away and
later returned collapses into one row whose `first_vintage`/`last_vintage`
spans the gap. Consumers doing event-history work should treat the vintage
bounds as first-seen/last-seen, not as an unbroken occupancy interval.

## The incident, in two paragraphs

The address log's first build grouped spells on raw ZIP. Current-pipeline raw
ZIPs carry the ZIP+4 route add-on (`02138-1234`); legacy raw ZIPs do not.
Identical addresses therefore format-split into separate spells: ~1.78M
phantom rows, an inflated multi-address rate, and exactly ZERO spells matching
across pipelines, a mechanically impossible statistic that exposed the broken
key before publish. The fix truncated ZIP to its 5-digit base.

That fix was incomplete, and the follow-up is the more instructive half. Legacy
raw ZIPs also went through a numeric round-trip that **stripped leading zeros**
(`02138` stored as `2138`), which truncation leaves untouched. The first
published build therefore shipped 848,048 rows with a 3-to-4 character `zip5`
that joins to nothing, and 147,994 phantom spells, and every organization in a
0-prefix state kept format-splitting against its own current-pipeline rows: MA,
CT, RI, NJ, ME and NH each sat at **exactly 0.00% cross-source spells** against
a national 15.8%. The national gate passed at 14.32% the whole time. The lesson
is rule 3 below: an aggregate gate cannot see a stratified failure.

## Rules

1. **Never join or group raw address fields across pipelines without format
   normalization.** The two pipelines skew in opposite directions and both
   skews break the key: current carries a ZIP+4 add-on, legacy has lost its
   leading zeros. The normalization is digits-only, then first-5 if longer
   than 5, else left-pad to 5; strings upper-cased and trimmed with
   empty-to-NULL. Canonical implementations: `normalize_zip5_sql()` in
   `scripts/build_address_resolved_crosswalk.R` and its R mirror
   `normalize_zip5()` in `scripts/validate_address_crosswalk.R`. Change one,
   change the other. **Running the same transform over both pipelines does not
   make their output consistent**, which is the trap this rule exists for: the
   inputs are formatted differently, so identical code produces divergent
   results. `.clean_zip()` in `R/address.R` extracted `^\d{5}`, which a
   leading-zero-stripped legacy ZIP cannot match, so `org_addr_zip5`,
   `org_addr_zip` and `org_addr_full` came out NA/ZIP-less for 100% of legacy
   rows in the 0-prefix states (ME, NH, VT, MA, RI, CT, NJ, PR, VI). Those
   columns are published, and `org_addr_full` is what the geocoder receives.
   `.clean_zip()` now pads 3-4 digit values before extracting; **every vintage
   published before 2026-07-28 still carries the damage** until it is re-run.
2. **Every cross-pipeline address aggregation must assert a cross-source match
   floor.** Legacy vintages end `2022_08` and the current pipeline starts
   `2023_06`. Any organization that lived across that gap without moving writes
   the same normalized tuple on each side, so those rows must fold into a
   single `source = 'both'` spell. At ~3.7M EINs a near-zero share is
   mechanically impossible: it means the key is broken, not that the data is
   strange. The builder hard-fails below a 1% floor (the healthy value is
   ~14%).
3. **Assert that floor per state as well as nationally.** A national average
   hides a stratified failure: the leading-zero defect held six states at zero
   while the national share looked healthy and the global gate passed. The
   builder hard-fails if any state with >= 50,000 spells falls below the 1%
   floor.
4. **Format invariants are cheap, assert them**: `zip5` exactly 5 digits or
   absent (not merely `<= 5`, since that weaker form is what let the short legacy
   ZIPs through), spell ranks contiguous per EIN, EIN renderings bijective.
5. **Sort published artifacts on the full grain.** Rank/order keys must be
   total, not partial. DuckDB returns rows in an arbitrary order
   (`preserve_insertion_order=false`, many threads), so tied rows land
   differently on every rebuild, which silently changes `spell_rank` and the
   artifact's sha256, and ADR 0014 idempotency is sha256 comparison.
6. **Sample-match against source before publish**:
   `scripts/validate_address_crosswalk.R` verifies random spell-0 rows
   byte-match the Unified BMF's raw values and runs the full invariant suite;
   run it after every build, before every publish.
7. **A cleaner must never turn populated input into NA, and the pipeline must
   halt when it does.** This defect class is invisible in a completeness
   percentage (the column just looks emptier) and survives every downstream
   stage, so it needs its own gate rather than a metric.
   `assert_zip_integrity()` in `R/quality/post_checks.R` fails the run under
   `STRICT_QUALITY_GATES` when a raw ZIP holding 3+ digits cleans to NA or to a
   non-5-digit value, and reports the damage by state so a stratified failure
   is legible. Both pipelines call it before Phases 10-11 write and upload. Any
   future cleaner that can drop populated values deserves the same treatment.

The general principle (standing rule 14): when a slow-cost defect class is
found, leave behind a fast detector. Zero-cross-source was that detector for
join-key breakage; it is now a build-stopping gate, nationally and per state.
