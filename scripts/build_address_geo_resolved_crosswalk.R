# ============================================================================
# build_address_geo_resolved_crosswalk.R
#
# Builds the address-geo-resolved crosswalk (nccs-contracts ADR 0051): census
# geography for every address in the address history. One row per spell of
# the address-resolved crosswalk, joined to it on spell_id, carrying the
# census block (2020 and 2010 boundaries), the 2020 ZIP Code Tabulation Area,
# the congressional district, the coordinates, and a plain-language match
# level. Spells without a street are listed with empty geography.
#
# WHERE TO RUN IT
#   A full build runs on EC2, never on a laptop. It downloads the Census
#   block boundary files for every state twice (2020 and 2010 boundaries)
#   plus two national files, roughly 15 to 20 GB in all, holds the largest
#   states' boundaries in memory, and writes an 11-million-row table.
#   Starting size: 64 GB of memory, 100 GB of disk, 8 virtual CPUs (for
#   example r6i.2xlarge). The work runs on one core, so more CPUs do not
#   make it faster. This size is an estimate from the size of the inputs and
#   has NOT yet been measured on a full run: watch memory on the first full
#   run and record the measured peak here (operating rule 7).
#   A trial run limited to a few small states (see CENSUS_GEO_STATES below)
#   is fine on a laptop.
#
# Inputs
#   data/crosswalks/address_resolved_crosswalk.parquet        the address history (with spell_id)
#   data/geocoding/address_history/output/*_geocoded.csv     geocoder results, one file per batch plus
#                                                            the carryover file (R/address_history_geocoding.R)
#   data/geocoding/address_history/input/address_history_geocoder_addr_lookup.parquet
#   data/geocoding/unified/merged/bmf_unified_geocoded.parquet the geocoded Unified BMF (which rank-0
#                                                            spells match the current address)
#   data/crosswalks/census_geo_resolved_crosswalk.parquet     the published census table (ADR 0045),
#                                                            for the current-address comparison
#   TIGER/Line via tigris (cached): blocks 2020 and 2010 per state, ZCTA 2020, districts (TIGER 2024)
#
# Outputs (data/crosswalks/)
#   address_geo_resolved_crosswalk.parquet / .csv
#   address_geo_resolved_crosswalk_data_dictionary.csv
#   address_geo_resolved_crosswalk_sample.csv        10,000 rows for reviewers
#   address_geo_resolved_crosswalk_audit.csv         state-prefix disagreements
#   address_geo_resolved_crosswalk_summary.json      coverage by level, checks, input months
#
# Run from the repo root after the geocoder run has been retrieved:
#   Rscript scripts/build_address_geo_resolved_crosswalk.R
# Optional, for a trial run: CENSUS_GEO_STATES="DE,RI" limits the block
# assignment and the current-address comparison to those states (the table
# is still written for every spell); ADDRESS_HISTORY_GEOCODING_DIR points at
# a different run folder.
#
# A note on `ein`. Every table read here carries the EIN in its cleaned,
# published form: the `ein` column, "XX-XXXXXXX", nine digits with leading
# zeros restored (R/ein.R). The uncleaned source value lives in a separate
# column, `ein_raw`, which this script never reads. Counting and joining on
# `ein` is therefore counting and joining on the cleaned EIN.
# ============================================================================

options(tigris_use_cache = TRUE)
sf::sf_use_s2(FALSE)   # planar predicates are what block shapefiles expect

source(here::here("R", "config.R"))
source(here::here("R", "utils", "logging.R"))
source(here::here("R", "ein.R"))
source(here::here("R", "address.R"))                       # .detect_po_box(), .build_full_address()
source(here::here("R", "address_normalize.R"))
source(here::here("R", "address_spell_id.R"))
source(here::here("R", "quality", "geocoding_checks.R"))   # GEOCODER_COLUMN_MAP
source(here::here("R", "census_geo_resolved.R"))
source(here::here("R", "census_geo_assign.R"))
source(here::here("R", "address_history_geocoding.R"))

ADDRESS_CROSSWALK_PATH <- here::here("data", "crosswalks", "address_resolved_crosswalk.parquet")
GEOCODED_UNIFIED_PATH  <- here::here("data", "geocoding", "unified", "merged", "bmf_unified_geocoded.parquet")
CENSUS_CROSSWALK_PATH  <- here::here("data", "crosswalks", "census_geo_resolved_crosswalk.parquet")
GEOCODING_DIR          <- Sys.getenv("ADDRESS_HISTORY_GEOCODING_DIR", ADDRESS_HISTORY_GEOCODING_DIR)
OUT_DIR                <- here::here("data", "crosswalks")
OUT_STEM               <- file.path(OUT_DIR, "address_geo_resolved_crosswalk")

TIGER_YEAR_2020 <- 2020L
TIGER_YEAR_2010 <- 2010L
CD_TIGER_YEAR   <- 2024L   # TIGER 2024 carries the 119th Congress districts
STATE_SUBSET    <- Sys.getenv("CENSUS_GEO_STATES", "")
IS_TRIAL_RUN    <- nzchar(STATE_SUBSET)

STATE_MISMATCH_MAX_SHARE <- 0.01   # ADR 0051 Acceptance: at least 99% of placed rows agree
SAMPLE_ROWS              <- 10000L

# ---------------------------------------------------------------------------
# 1. Read the address history and the geocoder results
# ---------------------------------------------------------------------------

log_info("Reading the address history")

address_spells <- arrow::read_parquet(ADDRESS_CROSSWALK_PATH) |>
  tibble::as_tibble()

stopifnot(
  "address history lacks spell_id (rebuild it first, ADR 0051)" = "spell_id" %in% names(address_spells)
)

# Organizations are counted on `ein`, the cleaned EIN (see the header note).
# The counts come from the table just read, not from its manifest: the
# manifest records the row count but not the number of organizations, and
# counting the rows in hand describes the file this build is actually using.
log_info(sprintf(
  "Spells: %s across %s organizations",
  format(nrow(address_spells), big.mark = ","),
  format(dplyr::n_distinct(address_spells$ein), big.mark = ",")
))

# Month check (also run before the geocoder submission): the address history
# and the geocoded Unified BMF must end on the same month of BMF data, or
# organizations that moved or registered in between would be missing from
# one of them. Both months are read from the data, because the manifests
# record the month the build ran, not the newest month inside the file.
unified_bmf_vintages <- arrow::read_parquet(GEOCODED_UNIFIED_PATH, col_select = "last_vintage_ym")

input_vintages <- address_history_stop_unless_same_vintage(
  address_history_vintages = address_spells$last_vintage,
  unified_bmf_vintages     = unified_bmf_vintages$last_vintage_ym
)

rm(unified_bmf_vintages)

log_info(sprintf("Both inputs end on BMF month %s", input_vintages$address_history))

distinct_addresses <- arrow::read_parquet(file.path(GEOCODING_DIR, "input", ADDRESS_HISTORY_ADDRESS_LOOKUP_FILE)) |>
  tibble::as_tibble()

geocodes <- read_address_history_geocodes(GEOCODING_DIR)

# As above, counted from the files just read. The run manifest holds the
# number of distinct addresses but not how many came back with a result.
log_info(sprintf(
  "Distinct addresses: %s; with a geocoder result: %s",
  format(nrow(distinct_addresses), big.mark = ","),
  format(nrow(geocodes), big.mark = ",")
))

# Every batch retrieved and every submitted address present in the outputs,
# or stop: a partial retrieval must never be published as "no match".
run_completeness <- address_history_stop_unless_complete(GEOCODING_DIR, geocodes)

log_info(sprintf(
  "Geocoder run complete: %d batch(es), %s submitted addresses all accounted for",
  run_completeness$batches,
  format(run_completeness$submitted, big.mark = ",")
))

# The distinct-address file has to describe the same address history, or a
# spell could silently miss its geocode.
addresses_in_history <- address_history_distinct_addresses(address_spells)

stopifnot(
  "distinct-address file does not match the address history" =
    nrow(addresses_in_history) == nrow(distinct_addresses) &&
    setequal(addresses_in_history$f_address, distinct_addresses$f_address)
)

# One row per distinct address with its geocode (NA columns when no result).
#
# ADR 0051 §5: only "address" rows are placed in a block. A PO box matched to
# a point keeps its coordinates (the post office's) but no block, ZCTA or
# district; point_level marks the rows that are placed.
address_geocodes <- distinct_addresses |>
  dplyr::left_join(geocodes, by = "f_address") |>
  dplyr::mutate(
    org_addr_is_po_box = .detect_po_box(street),
    geo_match_level    = address_geo_match_level(
      has_street    = TRUE,
      is_po_box     = org_addr_is_po_box,
      geo_addr_type = geo_addr_type,
      geo_lat       = geo_lat,
      geo_lon       = geo_lon
    ),
    point_level        = geo_match_level == "address"
  )

match_level_counts <- dplyr::count(address_geocodes, geo_match_level)

match_level_text <- paste(
  sprintf(
    "%s=%s",
    match_level_counts$geo_match_level,
    format(match_level_counts$n, big.mark = ",", trim = TRUE)
  ),
  collapse = ", "
)

log_info(sprintf("Match levels over distinct addresses: %s", match_level_text))

# ---------------------------------------------------------------------------
# 2. Point-in-polygon for the address-level points (geo_match_level "address")
# ---------------------------------------------------------------------------

# Named vector: state abbreviation -> 2-digit state FIPS code.
state_fips_by_abbr <- tigris::fips_codes |>
  dplyr::distinct(state, state_code) |>
  tibble::deframe()

points <- address_geocodes |>
  dplyr::filter(point_level) |>
  dplyr::mutate(
    state_fips = unname(state_fips_by_abbr[geo_state_abbr])
  ) |>
  dplyr::filter(!is.na(state_fips)) |>
  dplyr::select(f_address, geo_lon, geo_lat, state_fips)

states_to_build <- sort(unique(points$state_fips))

if (IS_TRIAL_RUN) {

  requested_abbreviations <- stringr::str_trim(stringr::str_split_1(STATE_SUBSET, ","))
  requested_fips          <- unname(state_fips_by_abbr[requested_abbreviations])

  states_to_build <- intersect(states_to_build, requested_fips)
  points          <- dplyr::filter(points, state_fips %in% states_to_build)

  log_info(sprintf("Trial run limited to %d state(s)", length(states_to_build)))

}

log_info(sprintf(
  "Address-level points to place: %s in %d states",
  format(nrow(points), big.mark = ","),
  length(states_to_build)
))

# --- Census blocks: one boundary file per state, so one state at a time ------

#' Census blocks (2020 and 2010 boundaries) for the points of one state.
#'
#' @param state_fips Character: the state's 2-digit FIPS code. The state's
#'   points are taken from `points` (a tibble with f_address, geo_lon,
#'   geo_lat and state_fips, defined above).
#' @return Tibble, one row per point of the state: f_address,
#'   block_geoid_2020 and block_geoid_2010 (NA where no block contains the
#'   point).
assign_blocks_for_one_state <- function(state_fips) {

  state_points <- dplyr::filter(points, state_fips == !!state_fips)

  log_info(sprintf("State %s: %s points", state_fips, format(nrow(state_points), big.mark = ",")))

  state_blocks <- tibble::tibble(
    f_address        = state_points$f_address,
    block_geoid_2020 = assign_blocks_for_state(state_points, state_fips, TIGER_YEAR_2020),
    block_geoid_2010 = assign_blocks_for_state(state_points, state_fips, TIGER_YEAR_2010)
  )

  return(state_blocks)

}

blocks_by_address <- purrr::map(states_to_build, assign_blocks_for_one_state) |>
  purrr::list_rbind()

# --- ZIP Code Tabulation Areas and congressional districts: national files ---

# Blocks are published per state, so the block step above turns each state's
# points into map points inside assign_blocks_for_state(), one state at a
# time. ZIP Code Tabulation Areas and congressional districts are each
# published as ONE national file, so every point is turned into a map point
# here, once, and the same set is used for both lookups.
points_sf <- sf::st_as_sf(
  points,
  coords = c("geo_lon", "geo_lat"),
  crs    = 4326,
  remove = FALSE
)

log_info("ZCTA 2020 (national)")

zcta_by_address <- tibble::tibble(
  f_address = points$f_address,
  zcta_2020 = assign_zcta_2020(points_sf)
)

# Congressional district GEOIDs are assigned at the national level too: one
# file covers every state, and the GEOID it returns already begins with the
# state's 2-digit FIPS code.
log_info(sprintf("Congressional districts (TIGER %d)", CD_TIGER_YEAR))

district_assignment <- assign_congressional_districts(points_sf, CD_TIGER_YEAR)

district_by_address <- tibble::tibble(
  f_address                  = points$f_address,
  congressional_district_119 = district_assignment$district
)

stopifnot(
  "TIGER districts are not the 119th Congress" = district_assignment$congress_session == 119L
)

rm(points_sf)
invisible(gc())

address_geography <- address_geocodes |>
  dplyr::left_join(blocks_by_address,   by = "f_address") |>
  dplyr::left_join(zcta_by_address,     by = "f_address") |>
  dplyr::left_join(district_by_address, by = "f_address")

# ---------------------------------------------------------------------------
# 3. State check (ADR 0051 Acceptance): the block's state must be the spell's
# ---------------------------------------------------------------------------

placed <- address_geography |>
  dplyr::filter(!is.na(block_geoid_2020)) |>
  dplyr::mutate(
    block_state_fips = census_geo_state_from_block(block_geoid_2020),
    spell_state_fips = unname(state_fips_by_abbr[state])
  )

state_comparable <- dplyr::filter(placed, !is.na(spell_state_fips))
state_mismatches <- dplyr::filter(state_comparable, block_state_fips != spell_state_fips)

state_mismatch_share <- if (nrow(state_comparable) > 0) {
  nrow(state_mismatches) / nrow(state_comparable)
} else {
  0
}

log_info(sprintf(
  "State check: %s comparable, %s disagree (%.3f%%), limit %.1f%%",
  format(nrow(state_comparable), big.mark = ","),
  format(nrow(state_mismatches), big.mark = ","),
  100 * state_mismatch_share,
  100 * STATE_MISMATCH_MAX_SHARE
))

state_mismatches |>
  dplyr::select(
    f_address,
    state,
    spell_state_fips,
    block_state_fips,
    block_geoid_2020,
    geo_match_addr,
    geo_addr_type,
    geo_score,
    geo_lat,
    geo_lon
  ) |>
  data.table::fwrite(paste0(OUT_STEM, "_audit.csv"))

if (state_mismatch_share > STATE_MISMATCH_MAX_SHARE) {

  stop(sprintf(
    "State check failed: %.3f%% of placed addresses sit in a different state than the spell says (limit %.1f%%). See %s_audit.csv",
    100 * state_mismatch_share,
    100 * STATE_MISMATCH_MAX_SHARE,
    OUT_STEM
  ))

}

# ---------------------------------------------------------------------------
# 4. One row per spell
# ---------------------------------------------------------------------------

geography_columns <- c(
  "block_geoid_2020",
  "block_geoid_2010",
  "zcta_2020",
  "congressional_district_119",
  "latitude",
  "longitude",
  "geo_match_level",
  "geo_addr_type",
  "geo_score",
  "geo_match_addr",
  "org_addr_is_po_box"
)

# The geography of each distinct address, under the published column names.
# geo_state_abbr is kept for the trial-run filter in section 5 only; it is
# not published.
geography_by_address <- address_geography |>
  dplyr::select(
    street,
    city,
    state,
    zip5,
    block_geoid_2020,
    block_geoid_2010,
    zcta_2020,
    congressional_district_119,
    latitude  = geo_lat,
    longitude = geo_lon,
    geo_match_level,
    geo_addr_type,
    geo_score,
    geo_match_addr,
    org_addr_is_po_box,
    geo_state_abbr
  )

crosswalk <- address_spells |>
  dplyr::select(spell_id, EIN2, ein, ein_prefixed, spell_rank, street, city, state, zip5) |>
  dplyr::left_join(
    geography_by_address,
    by = c("street", "city", "state", "zip5")
  ) |>
  dplyr::mutate(
    geo_match_level    = dplyr::if_else(is.na(street), "not_geocoded", geo_match_level),
    org_addr_is_po_box = dplyr::if_else(is.na(street), NA, org_addr_is_po_box)
  ) |>
  dplyr::arrange(ein, spell_rank)

# ---------------------------------------------------------------------------
# 5. Current addresses come from the published census table (ADR 0051 §6,
#    operating rule 6). For every rank-0 spell whose address is the one the
#    Unified BMF holds for that organization, the census table's geography
#    is copied in. The values computed above for the same rows must agree
#    with the copy (they come from the same coordinates), and the §5 rule
#    still applies to the copy: only "address" rows keep a block. Rank-0
#    spells whose address is not the Unified BMF's keep the computed values;
#    they must be rare and are listed in an audit file.
# ---------------------------------------------------------------------------

log_info("Copying current-address geography from the census-geo-resolved crosswalk")

# The Unified BMF's current address per organization, normalized the same way
# the address history is. `ein` is the cleaned EIN in both tables (see the
# header note), so it matches across them without further cleaning.
unified_current_addresses <- arrow::read_parquet(
  GEOCODED_UNIFIED_PATH,
  col_select = c(
    "ein",
    "org_addr_street_raw",
    "org_addr_city_raw",
    "org_addr_state_raw",
    "org_addr_zip_raw"
  )
) |>
  dplyr::mutate(
    ein    = ein,
    street = address_normalize_text(org_addr_street_raw),
    city   = address_normalize_text(org_addr_city_raw),
    state  = address_normalize_text(org_addr_state_raw),
    zip5   = address_normalize_zip5(org_addr_zip_raw),
    .keep  = "none"
  )

census_crosswalk <- arrow::read_parquet(
  CENSUS_CROSSWALK_PATH,
  col_select = c(
    "ein",
    "block_geoid_2020",
    "block_geoid_2010",
    "zcta_2020",
    "congressional_district"
  )
)

# A trial run assigns blocks only in the requested states, so it compares only
# addresses the geocoder placed in those states (an organization can list one
# state and sit in another; the state check above counts those). A full run
# compares every state.
if (IS_TRIAL_RUN) {

  is_state_built  <- state_fips_by_abbr %in% states_to_build
  states_compared <- names(state_fips_by_abbr)[is_state_built]

} else {

  states_compared <- unique(crosswalk$geo_state_abbr)

}

current_rows <- crosswalk |>
  dplyr::filter(
    spell_rank == 0L,
    !is.na(street),
    geo_state_abbr %in% states_compared
  )

# Coverage: which current rows are the Unified BMF's address, and of those,
# which have a census row. Every matched row must have one. The match is on
# the organization (`ein`, the cleaned EIN) and the normalized address.
current_address_key <- c("ein", "street", "city", "state", "zip5")

current_matched <- current_rows |>
  dplyr::semi_join(unified_current_addresses, by = current_address_key)

current_unmatched <- current_rows |>
  dplyr::anti_join(unified_current_addresses, by = current_address_key)

current_copied <- current_matched |>
  dplyr::inner_join(census_crosswalk, by = "ein", suffix = c("", "_census"))

stopifnot(
  "a current address matched to the Unified BMF has no row in the census-geo-resolved crosswalk" =
    nrow(current_copied) == nrow(current_matched)
)

unmatched_share <- if (nrow(current_rows) > 0) {
  nrow(current_unmatched) / nrow(current_rows)
} else {
  0
}

log_info(sprintf(
  "Current addresses: %s; the Unified BMF's address: %s; not the Unified BMF's address: %s (%.3f%%)",
  format(nrow(current_rows), big.mark = ","),
  format(nrow(current_matched), big.mark = ","),
  format(nrow(current_unmatched), big.mark = ","),
  100 * unmatched_share
))

current_unmatched |>
  dplyr::select(spell_id, ein, street, city, state, zip5, geo_match_level) |>
  data.table::fwrite(paste0(OUT_STEM, "_current_addresses_not_in_census_table.csv"))

if (unmatched_share > STATE_MISMATCH_MAX_SHARE) {

  stop(sprintf(
    "%.3f%% of current addresses are not the Unified BMF's address (limit %.1f%%): the address history and the census table are out of step. Rebuild both from the same vintage.",
    100 * unmatched_share,
    100 * STATE_MISMATCH_MAX_SHARE
  ))

}

#' TRUE where a computed value and a copied value agree.
#'
#' Two values agree when they are equal, or when both are missing. A plain
#' `==` returns NA when either side is missing, which would hide a real
#' disagreement (one side filled, the other empty) and miscount two empty
#' values as unknown.
#'
#' @param computed Vector: the value this build computed.
#' @param copied   Vector of the same length and type: the value copied from
#'   the census-geo-resolved crosswalk.
#' @return Logical vector, never NA: TRUE where the two agree.
same_value <- function(computed, copied) {

  both_missing <- is.na(computed) & is.na(copied)
  equal_values <- computed == copied   # NA where either side is missing

  # Where the comparison is NA, the answer is whether both were missing.
  values_agree <- dplyr::coalesce(equal_values, both_missing)

  return(values_agree)

}

# The copy, with the §5 rule applied: a block only where the match level is "address".
current_copied <- current_copied |>
  dplyr::mutate(
    keep_block                      = geo_match_level == "address",
    block_geoid_2020_copy           = dplyr::if_else(keep_block, block_geoid_2020_census, NA_character_),
    block_geoid_2010_copy           = dplyr::if_else(keep_block, block_geoid_2010_census, NA_character_),
    zcta_2020_copy                  = dplyr::if_else(keep_block, zcta_2020_census, NA_character_),
    congressional_district_119_copy = dplyr::if_else(keep_block, congressional_district, NA_character_),
    agrees_block_2020               = same_value(block_geoid_2020, block_geoid_2020_copy),
    agrees_block_2010               = same_value(block_geoid_2010, block_geoid_2010_copy),
    agrees_zcta                     = same_value(zcta_2020, zcta_2020_copy),
    agrees_district                 = same_value(congressional_district_119, congressional_district_119_copy),
    agrees                          = agrees_block_2020 & agrees_block_2010 & agrees_zcta & agrees_district
  )

current_disagreements <- dplyr::filter(current_copied, !agrees)

log_info(sprintf(
  "Current addresses copied from the census table: %s; computed values that differ from the copy: %s",
  format(nrow(current_copied), big.mark = ","),
  format(nrow(current_disagreements), big.mark = ",")
))

if (nrow(current_disagreements) > 0L) {

  data.table::fwrite(current_disagreements, paste0(OUT_STEM, "_current_address_disagreements.csv"))

  stop(sprintf(
    "%d current addresses carry different geography than the census-geo-resolved crosswalk; see %s_current_address_disagreements.csv",
    nrow(current_disagreements),
    OUT_STEM
  ))

}

# Write the copied values into the table (identical to the computed ones,
# as just checked; the census table is the source of record for them).
copied_geography <- current_copied |>
  dplyr::select(
    spell_id,
    block_geoid_2020           = block_geoid_2020_copy,
    block_geoid_2010           = block_geoid_2010_copy,
    zcta_2020                  = zcta_2020_copy,
    congressional_district_119 = congressional_district_119_copy
  )

crosswalk <- crosswalk |>
  dplyr::rows_update(copied_geography, by = "spell_id")

# ---------------------------------------------------------------------------
# 6. Checks before anything is written (ADR 0051 Acceptance)
# ---------------------------------------------------------------------------

no_geography_levels    <- c("no_match", "not_geocoded")
rows_without_geography <- dplyr::filter(crosswalk, geo_match_level %in% no_geography_levels)

rows_without_geography_are_empty <- all(
  is.na(rows_without_geography$block_geoid_2020) &
    is.na(rows_without_geography$zcta_2020) &
    is.na(rows_without_geography$latitude)
)

has_street   <- !is.na(crosswalk$street)
has_block    <- !is.na(crosswalk$block_geoid_2020)
has_zcta     <- !is.na(crosswalk$zcta_2020)
has_district <- !is.na(crosswalk$congressional_district_119)

stopifnot(
  "row count differs from the address history" =
    nrow(crosswalk) == nrow(address_spells),
  "spell_id repeats" =
    !anyDuplicated(crosswalk$spell_id),
  "spell_id set differs from the address history" =
    setequal(crosswalk$spell_id, address_spells$spell_id),
  "geo_match_level has empty values" =
    !anyNA(crosswalk$geo_match_level),
  "a spell without a street is not not_geocoded" =
    all(crosswalk$geo_match_level[!has_street] == "not_geocoded"),
  "a spell with a street is marked not_geocoded" =
    !any(crosswalk$geo_match_level[has_street] == "not_geocoded"),
  "no_match or not_geocoded rows carry geography" =
    rows_without_geography_are_empty,
  "a block is assigned outside the address level" =
    all(crosswalk$geo_match_level[has_block] == "address"),
  "a ZCTA or district is assigned outside the address level" =
    all(crosswalk$geo_match_level[has_zcta | has_district] == "address")
)

# ---------------------------------------------------------------------------
# 7. Write the table, the sample, the dictionary and the summary
# ---------------------------------------------------------------------------

crosswalk_out <- dplyr::select(
  crosswalk,
  spell_id,
  EIN2,
  ein,
  ein_prefixed,
  dplyr::all_of(geography_columns)
)

arrow::write_parquet(crosswalk_out, paste0(OUT_STEM, ".parquet"), compression = "zstd")
data.table::fwrite(crosswalk_out, paste0(OUT_STEM, ".csv"))

set.seed(2051)

crosswalk_out |>
  dplyr::slice_sample(n = SAMPLE_ROWS) |>
  data.table::fwrite(paste0(OUT_STEM, "_sample.csv"))

dictionary <- tibble::tribble(
  ~column, ~description,
  "spell_id",                   "Stable identifier of one organization at one address. Join key to the address-resolved crosswalk (same column there). It does not change between builds; spell_rank does.",
  "EIN2",                       "Employer Identification Number, EIN-XX-XXXXXXX (ADR 0036).",
  "ein",                        "Employer Identification Number, XX-XXXXXXX.",
  "ein_prefixed",               "Coercion-safe EIN key, ein-XX-XXXXXXX (ADR 0036).",
  "block_geoid_2020",           "15-digit census block GEOID on 2020 boundaries (TIGER/Line 2020). Tract = first 11 digits, block group = first 12, county = first 5, state = first 2. Empty unless geo_match_level is address.",
  "block_geoid_2010",           "15-digit census block GEOID on 2010 boundaries (TIGER/Line 2010), for joins to pre-2020 census products. Same prefix rules. Empty as above.",
  "zcta_2020",                  "5-digit ZIP Code Tabulation Area, 2020 boundaries. A ZCTA is an area; a ZIP code is a delivery route. Empty as above.",
  "congressional_district_119", "4-digit district GEOID (2-digit state FIPS + 2-digit district number) for the 119th Congress, from TIGER/Line 2024. Empty as above.",
  "latitude",                   "Latitude returned by the geocoder (WGS 84). Empty when no_match or not_geocoded.",
  "longitude",                  "Longitude returned by the geocoder (WGS 84). Empty when no_match or not_geocoded.",
  "geo_match_level",            "How the address was placed. not_geocoded: the spell has no street (pre-2009 records) and was not sent. no_match: sent, no coordinates came back. po_box: the street is a post office box; the coordinates are the post office's, and no block is assigned. address: matched to a specific address (PointAddress, Subaddress, StreetAddress, StreetAddressExt, StreetInt). zip: ZIP code centre only (Postal, PostalExt, PostalLoc). city: city or place only (Locality). other: any other tier with coordinates (street name, point of interest). Only address rows carry a block, ZCTA and district.",
  "geo_addr_type",              "Geocoder precision tier as returned.",
  "geo_score",                  "Geocoder match confidence, 0 to 100.",
  "geo_match_addr",             "The address the geocoder matched, as it matched it. Lets a user see what the coordinates stand for. Past addresses are placed on today's street network.",
  "org_addr_is_po_box",         "TRUE when the street is a post office box. Empty when the spell has no street."
)

data.table::fwrite(dictionary, paste0(OUT_STEM, "_data_dictionary.csv"))

summary <- list(
  address_history_rows                = nrow(address_spells),
  rows                                = nrow(crosswalk_out),
  distinct_addresses                  = nrow(distinct_addresses),
  distinct_addresses_geocoded         = sum(!is.na(address_geocodes$geo_lat)),
  by_match_level                      = as.list(table(crosswalk_out$geo_match_level)),
  assigned_block_2020                 = sum(!is.na(crosswalk_out$block_geoid_2020)),
  assigned_block_2010                 = sum(!is.na(crosswalk_out$block_geoid_2010)),
  assigned_zcta_2020                  = sum(!is.na(crosswalk_out$zcta_2020)),
  assigned_congressional_district_119 = sum(!is.na(crosswalk_out$congressional_district_119)),
  input_vintages                      = list(
    address_history = input_vintages$address_history,
    unified_bmf     = input_vintages$unified_bmf
  ),
  state_check                         = list(
    n_comparable   = nrow(state_comparable),
    n_mismatch     = nrow(state_mismatches),
    mismatch_share = state_mismatch_share,
    limit          = STATE_MISMATCH_MAX_SHARE
  ),
  current_address_copy                = list(
    n_current             = nrow(current_rows),
    n_copied              = nrow(current_copied),
    n_not_in_census_table = nrow(current_unmatched),
    n_disagree            = nrow(current_disagreements)
  ),
  tiger                               = list(
    blocks_2020             = TIGER_YEAR_2020,
    blocks_2010             = TIGER_YEAR_2010,
    zcta                    = TIGER_YEAR_2020,
    congressional_districts = CD_TIGER_YEAR,
    congress_session        = 119L
  ),
  states_built                        = states_to_build,
  built_at                            = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
)

jsonlite::write_json(summary, paste0(OUT_STEM, "_summary.json"), auto_unbox = TRUE, pretty = TRUE)

log_info(sprintf(
  "Wrote %s.{parquet,csv} (%s rows), sample, dictionary, audit and summary",
  OUT_STEM,
  format(nrow(crosswalk_out), big.mark = ",")
))

# ---------------------------------------------------------------------------
# 8. Persist the address cache so the next round sends only new addresses
#    (ADR 0051 §7). Skipped on a trial run.
# ---------------------------------------------------------------------------

if (IS_TRIAL_RUN) {

  log_info("Trial run: address cache not written")

} else {

  address_history_write_cache(address_geocodes, geocoding_dir = GEOCODING_DIR)

}
