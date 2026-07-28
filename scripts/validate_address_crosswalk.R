# ============================================================================
# validate_address_crosswalk.R
#
# Standing invariant suite for the address-resolved crosswalk (see
# docs/reference/address-data-invariants.md). Run after every build,
# before every publish:
#   Rscript scripts/validate_address_crosswalk.R
# Exits nonzero on any violation.
#
# Inputs (both are build outputs, read from disk, so this runs standalone in CI
# or a fresh shell with nothing left over in the R session):
#   ADDR_XWALK_PARQUET  the crosswalk being checked. Written by
#                       scripts/build_address_resolved_crosswalk.R to
#                       data/crosswalks/address_resolved_crosswalk.parquet.
#   UNIFIED_PARQUET     the Unified BMF (ADR 0037; still written to data/master/
#                       by R/run_master_pipeline.R until that rename finishes).
#                       It is the source of truth the crosswalk is checked
#                       against. Optional and unset by default, because it is a
#                       multi-GB file usually absent on a laptop. When it is
#                       missing that one check is skipped rather than failed, so
#                       everything else still runs locally.
# ============================================================================

suppressPackageStartupMessages({library(arrow); library(data.table); library(purrr)})
library(here)
source(here::here("R", "utils", "logging.R"))        # log_info()
source(here::here("R", "ein.R"))                     # ein_to_prefixed/ein_to_ein2 (ADR 0036)

crosswalk_parquet_path <- Sys.getenv(
  "ADDR_XWALK_PARQUET",
  here::here("data", "crosswalks", "address_resolved_crosswalk.parquet"))
unified_parquet_path <- Sys.getenv("UNIFIED_PARQUET", "")

# Some checks only make sense on a full build (~11M rows). A small test file
# skips them so the suite stays runnable against local fixtures.
PRODUCTION_SCALE_ROWS    <- 1e6
CROSS_SOURCE_SHARE_FLOOR <- 0.01
PER_STATE_MIN_SPELLS     <- 5e4
SAMPLE_MATCH_FLOOR       <- 0.99
SAMPLE_SIZE              <- 1000L

address_crosswalk <- data.table::setDT(arrow::read_parquet(crosswalk_parquet_path))

#' Reduce a raw ZIP to the 5-digit form the crosswalk keys on.
#'
#' The same rule the builder applies in SQL (normalize_zip5_sql): keep digits
#' only, take the first five (dropping the ZIP+4 add-on the current pipeline
#' carries), and put back any leading zero the legacy pipeline dropped. Change
#' one of the two and change the other.
normalize_zip5 <- function(raw_zip) {
  zip_base <- substr(gsub("[^0-9]", "", raw_zip), 1, 5)
  data.table::fifelse(zip_base == "", NA_character_,
                      stringr::str_pad(zip_base, width = 5, side = "left", pad = "0"))
}

# Every check is collected into one named vector, then reported together, so a
# run tells you everything that is wrong rather than stopping at the first
# problem. The name of each element is the message logged for it.
check_results <- c(
  # A ZIP is five digits or it is absent. A shorter one means a legacy ZIP kept
  # its missing leading zero; a longer one means a ZIP+4 add-on survived. Either
  # way the same address stops matching itself across the two pipelines.
  "zip5 is exactly 5 digits or absent" =
    address_crosswalk[!is.na(zip5) & nchar(zip5) != 5L, .N] == 0L,

  # spell_rank puts an organization's addresses in order, most recent first:
  # 0 is where it is now, 1 is where it was before that. So an organization
  # with 4 addresses must have ranks 0,1,2,3 and nothing missing or repeated.
  "spell ranks contiguous per EIN" =
    address_crosswalk[, max(spell_rank) + 1L == .N, by = ein][, all(V1)],

  # One row per organization per address is the whole point of the table, so
  # the same address must not appear twice for the same organization.
  "no duplicate spell tuples per EIN" =
    anyDuplicated(address_crosswalk[, .(ein, street, city, state, zip5)]) == 0L,

  # EIN2 and ein_prefixed are just the canonical EIN written differently
  # (ADR 0036), so rebuilding them from `ein` has to reproduce the published
  # columns exactly. If it does not, anyone joining on EIN2 gets the wrong
  # organization.
  "EIN2 consistent with canonical ein" =
    address_crosswalk[, all(EIN2 == ein_to_ein2(ein))],
  "ein_prefixed consistent with canonical ein" =
    address_crosswalk[, all(ein_prefixed == ein_to_prefixed(ein))]
)

# ---------------------------------------------------------------------------
# Is the address history actually joined up across the two pipelines?
#
# The purpose of the floor: this table is built by matching addresses from the
# legacy pipeline against addresses from the current one, and if that matching
# quietly stops working, the table still looks completely normal. It has the
# expected number of rows and no missing values. The only visible symptom is
# that organizations stop appearing in both pipelines at once, so `source`
# says 'legacy' or 'current' but almost never 'both'.
#
# That is a symptom we can put a number on. The legacy pipeline stops at
# 2022_08 and the current one starts at 2023_06, so any organization that was
# around on both sides of that gap and did not move writes down the same
# address twice, and the build folds those into a single 'both' row. Across
# 3.7M organizations that has to happen a lot: the real figure is about 14%.
# It cannot fall near zero for any innocent reason, so a floor of 1% is a
# tripwire that only a broken match can trip. Both times the address matching
# has broken, this is what caught it.
# ---------------------------------------------------------------------------
if (nrow(address_crosswalk) > PRODUCTION_SCALE_ROWS) {
  cross_source_share <- address_crosswalk[source == "both", .N] / nrow(address_crosswalk)
  check_results[sprintf("cross-source spell share %.2f%% >= %.0f%% floor",
                        100 * cross_source_share, 100 * CROSS_SOURCE_SHARE_FLOOR)] <-
    cross_source_share >= CROSS_SOURCE_SHARE_FLOOR

  # The same tripwire per state, because a national average hides a failure
  # confined to part of the country. The missing leading zeros broke every
  # address match in MA, CT, RI, NJ, ME and NH, and the national figure stayed
  # at a healthy 15.8% while those six states sat at zero.
  per_state_cross_source <- address_crosswalk[
    !is.na(state), .(spell_count = .N,
                     cross_source_share = sum(source == "both") / .N), by = state]
  states_below_floor <- per_state_cross_source[
    spell_count >= PER_STATE_MIN_SPELLS & cross_source_share < CROSS_SOURCE_SHARE_FLOOR]
  check_results[sprintf("no large state below the %.0f%% cross-source floor%s",
                        100 * CROSS_SOURCE_SHARE_FLOOR,
                        if (nrow(states_below_floor) > 0L) {
                          sprintf(" (offenders: %s)", paste(states_below_floor$state,
                                                            collapse = ", "))
                        } else "")] <- nrow(states_below_floor) == 0L
}

# ---------------------------------------------------------------------------
# Does the crosswalk agree with the Unified BMF about where organizations are?
#
# Yes: that is exactly what this checks. The crosswalk's most recent address
# for an organization should be the address the Unified BMF already publishes
# for it. Take 1,000 organizations at random, compare the two, and expect
# better than 99% agreement. It is the check that would notice the crosswalk
# drifting away from the published source, as opposed to the checks above,
# which only notice the crosswalk contradicting itself.
# ---------------------------------------------------------------------------
if (nzchar(unified_parquet_path) && file.exists(unified_parquet_path)) {
  unified_bmf <- data.table::setDT(arrow::read_parquet(unified_parquet_path, col_select = c(
    "EIN2", "org_addr_street_raw", "org_addr_city_raw", "org_addr_zip_raw", "bmf_source")))

  # Draw the sample from the rows that are actually comparable (current
  # pipeline, street present) and size it from that same pool. Sizing it off
  # the wider row count would ask for more organizations than the pool holds
  # and error out.
  eligible_ein2 <- unified_bmf[bmf_source == "current" & !is.na(org_addr_street_raw), EIN2]
  set.seed(42)
  sampled_ein2 <- sample(eligible_ein2, min(SAMPLE_SIZE, length(eligible_ein2)))

  # For the sampled organizations: crosswalk's most recent address on one side,
  # Unified BMF's address on the other, matched up by EIN2.
  joined_sample <- merge(
    address_crosswalk[spell_rank == 0L & EIN2 %in% sampled_ein2],
    unified_bmf[EIN2 %in% sampled_ein2], by = "EIN2")

  # The Unified BMF's values get the same tidying the crosswalk applied before
  # they are compared, otherwise the check would fail on capitalisation and ZIP
  # formatting rather than on a genuine disagreement about the address.
  sample_match_rate <- joined_sample[, mean(
    toupper(trimws(org_addr_street_raw)) == street &
    toupper(trimws(org_addr_city_raw))   == city &
    normalize_zip5(org_addr_zip_raw)     == zip5,
    na.rm = TRUE)]
  check_results[sprintf("spell-0 sample match vs Unified BMF: %.2f%% (n=%d)",
                        100 * sample_match_rate, nrow(joined_sample))] <-
    sample_match_rate > SAMPLE_MATCH_FLOOR
} else {
  log_info("SKIP: Unified BMF not provided (set UNIFIED_PARQUET for the sample-match check)")
}

# Report every check, then fail the run if any of them came back FALSE.
purrr::iwalk(check_results,
             function(passed, description) {
               log_info(paste(if (isTRUE(passed)) "PASS:" else "FAIL:", description))
             })

failed_checks <- names(check_results)[!vapply(check_results, isTRUE, logical(1))]
if (length(failed_checks) > 0L) {
  stop(sprintf("Address crosswalk validation: %d failure(s)", length(failed_checks)))
}
log_info("Address crosswalk validation: all checks passed")
