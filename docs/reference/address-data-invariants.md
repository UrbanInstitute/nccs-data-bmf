# Address-data invariants (rules for any address-related operation)

Codified 2026-07-26 after the address-log zip-format incident. These apply
to the address-resolved crosswalk, the geocoding export/merge, and any
future operation that joins or aggregates addresses across the two BMF
pipelines.

## The incident, in one paragraph

The address log's first build grouped spells on raw ZIP. Current-pipeline
raw ZIPs are ZIP+4; legacy raw ZIPs are 5-digit. Identical addresses
therefore format-split into separate spells: ~1.78M phantom rows, an
inflated multi-address rate, and exactly ZERO spells matching across
pipelines: a mechanically impossible statistic that exposed the broken
key before publish.

## Rules

1. **Never join or group raw address fields across pipelines without
   format normalization.** Minimum: ZIP truncated to 5 digits; strings
   upper-cased and trimmed with empty-to-NULL. (The CLEANED columns,
   `org_addr_zip5` etc., are already consistent because both pipelines
   run the same transform: prefer them when raw fidelity is not
   required.)
2. **Every cross-pipeline address aggregation must assert a cross-source
   match floor.** At production scale, organizations alive across the
   2023 pipeline transition MUST produce matching tuples; a near-zero
   cross-source share means the key is broken, not that the data is
   strange. The builder hard-fails below a 1% floor.
3. **Format invariants are cheap, assert them**: zip5 length <= 5,
   spell ranks contiguous per EIN, EIN renderings bijective.
4. **Sample-match against source before publish**:
   `scripts/validate_address_crosswalk.R` verifies random spell-0 rows
   byte-match the master BMF's raw values and runs the full invariant
   suite; run it after every build, before every publish.

The general principle (standing rule 14): when a slow-cost defect class
is found, leave behind a fast detector. Zero-cross-source was that
detector for join-key breakage; it is now a build-stopping gate.
