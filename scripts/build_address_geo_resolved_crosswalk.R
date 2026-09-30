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
# Inputs
#   data/crosswalks/address_resolved_crosswalk.parquet        the address history (with spell_id)
#   data/geocoding/address_history/output/*_geocoded.csv     geocoder results, one file per batch plus
#                                                            the carryover file (R/address_history_geocoding.R)
#   data/geocoding/address_history/input/address_history_geocoder_addr_lookup.parquet
#   data/geocoding/master/merged/bmf_unified_geocoded.parquet the geocoded Unified BMF (which rank-0
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
#   address_geo_resolved_crosswalk_summary.json      coverage by level, checks
#
# Run from the repo root after the geocoder run has been retrieved:
#   Rscript scripts/build_address_geo_resolved_crosswalk.R
# Optional, for a trial run: CENSUS_GEO_STATES="DE,RI" limits the block
# assignment and the current-address comparison to those states (the table
# is still written for every spell); ADDRESS_HISTORY_GEOCODING_DIR points at
# a different run folder.
# ============================================================================

suppressPackageStartupMessages({
  library(dplyr)
})
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
GEOCODED_UNIFIED_PATH  <- here::here("data", "geocoding", "master", "merged", "bmf_unified_geocoded.parquet")
CENSUS_CROSSWALK_PATH  <- here::here("data", "crosswalks", "census_geo_resolved_crosswalk.parquet")
GEOCODING_DIR          <- Sys.getenv("ADDRESS_HISTORY_GEOCODING_DIR", ADDRESS_HISTORY_GEOCODING_DIR)
OUT_DIR                <- here::here("data", "crosswalks")
OUT_STEM               <- file.path(OUT_DIR, "address_geo_resolved_crosswalk")

TIGER_YEAR_2020 <- 2020L
TIGER_YEAR_2010 <- 2010L
CD_TIGER_YEAR   <- 2024L   # TIGER 2024 carries the 119th Congress districts
STATE_SUBSET    <- Sys.getenv("CENSUS_GEO_STATES", "")

STATE_MISMATCH_MAX_SHARE <- 0.01   # ADR 0051 Acceptance: at least 99% of placed rows agree
SAMPLE_ROWS              <- 10000L

# ---------------------------------------------------------------------------
# 1. Read the address history and the geocoder results
# ---------------------------------------------------------------------------

log_info("Reading the address history")
address_spells <- arrow::read_parquet(ADDRESS_CROSSWALK_PATH) |>
  tibble::as_tibble()
stopifnot("address history lacks spell_id (rebuild it first, ADR 0051)" = "spell_id" %in% names(address_spells))

log_info(sprintf("Spells: %s across %s organizations",
                 format(nrow(address_spells), big.mark = ","),
                 format(dplyr::n_distinct(address_spells$ein), big.mark = ",")))

distinct_addresses <- arrow::read_parquet(file.path(GEOCODING_DIR, "input", ADDRESS_HISTORY_ADDRESS_LOOKUP_FILE)) |>
  tibble::as_tibble()

geocodes <- read_address_history_geocodes(GEOCODING_DIR)
log_info(sprintf("Distinct addresses: %s; with a geocoder result: %s",
                 format(nrow(distinct_addresses), big.mark = ","),
                 format(nrow(geocodes), big.mark = ",")))

# The distinct-address file has to describe the same address history, or a
# spell could silently miss its geocode.
addresses_in_history <- address_history_distinct_addresses(address_spells)
stopifnot(
  "distinct-address file does not match the address history" =
    nrow(addresses_in_history) == nrow(distinct_addresses) &&
    setequal(addresses_in_history$f_address, distinct_addresses$f_address)
)

# One row per distinct address with its geocode (NA columns when no result).
address_geocodes <- distinct_addresses |>
  left_join(geocodes, by = "f_address") |>
  mutate(
    org_addr_is_po_box = .detect_po_box(street),
    geo_match_level    = address_geo_match_level(
      has_street = TRUE, is_po_box = org_addr_is_po_box,
      geo_addr_type = geo_addr_type, geo_lat = geo_lat, geo_lon = geo_lon),
    point_level        = census_geo_is_point_level(geo_addr_type, geo_lat, geo_lon)
  )

log_info(sprintf("Match levels over distinct addresses: %s",
                 paste(sprintf("%s=%s", names(table(address_geocodes$geo_match_level)),
                               format(as.integer(table(address_geocodes$geo_match_level)), big.mark = ",")),
                       collapse = ", ")))

# ---------------------------------------------------------------------------
# 2. Point-in-polygon for the address-level points
#    Blocks go to every point-level geocode, PO boxes included (the block of
#    the post office, as in the census table, ADR 0045), so the two tables
#    agree row for row on current addresses. The match level says which is
#    which.
# ---------------------------------------------------------------------------

state_fips_by_abbr <- tigris::fips_codes |>
  distinct(state, state_code) |>
  tibble::deframe()

points <- address_geocodes |>
  filter(point_level) |>
  mutate(state_fips = unname(state_fips_by_abbr[geo_state_abbr])) |>
  filter(!is.na(state_fips)) |>
  select(f_address, geo_lon, geo_lat, state_fips)

states_to_build <- sort(unique(points$state_fips))
if (nzchar(STATE_SUBSET)) {
  requested_abbreviations <- trimws(strsplit(STATE_SUBSET, ",")[[1]])
  states_to_build         <- intersect(states_to_build, unname(state_fips_by_abbr[requested_abbreviations]))
  points                  <- filter(points, state_fips %in% states_to_build)
  log_info(sprintf("Trial run limited to %d state(s)", length(states_to_build)))
}
log_info(sprintf("Address-level points to place: %s in %d states",
                 format(nrow(points), big.mark = ","), length(states_to_build)))

assign_blocks_for_one_state <- function(state_fips) {
  state_points <- filter(points, state_fips == !!state_fips)
  log_info(sprintf("State %s: %s points", state_fips, format(nrow(state_points), big.mark = ",")))

  tibble::tibble(
    f_address        = state_points$f_address,
    block_geoid_2020 = assign_blocks_for_state(state_points, state_fips, TIGER_YEAR_2020),
    block_geoid_2010 = assign_blocks_for_state(state_points, state_fips, TIGER_YEAR_2010)
  )
}

blocks_by_address <- purrr::map(states_to_build, assign_blocks_for_one_state) |>
  purrr::list_rbind()

points_sf <- sf::st_as_sf(points, coords = c("geo_lon", "geo_lat"), crs = 4326, remove = FALSE)

log_info("ZCTA 2020 (national)")
zcta_by_address <- tibble::tibble(f_address = points$f_address, zcta_2020 = assign_zcta_2020(points_sf))

log_info(sprintf("Congressional districts (TIGER %d)", CD_TIGER_YEAR))
district_assignment <- assign_congressional_districts(points_sf, CD_TIGER_YEAR)
district_by_address <- tibble::tibble(f_address = points$f_address,
                                      congressional_district_119 = district_assignment$district)
stopifnot("TIGER districts are not the 119th Congress" = district_assignment$congress_session == 119L)
rm(points_sf)
invisible(gc())

address_geography <- address_geocodes |>
  left_join(blocks_by_address,   by = "f_address") |>
  left_join(zcta_by_address,     by = "f_address") |>
  left_join(district_by_address, by = "f_address")

# ---------------------------------------------------------------------------
# 3. State check (ADR 0051 Acceptance): the block's state must be the spell's
# ---------------------------------------------------------------------------

placed <- address_geography |>
  filter(!is.na(block_geoid_2020)) |>
  mutate(block_state_fips = census_geo_state_from_block(block_geoid_2020),
         spell_state_fips = unname(state_fips_by_abbr[state]))

state_comparable <- filter(placed, !is.na(spell_state_fips))
state_mismatches <- filter(state_comparable, block_state_fips != spell_state_fips)
state_mismatch_share <- if (nrow(state_comparable) > 0) nrow(state_mismatches) / nrow(state_comparable) else 0

log_info(sprintf("State check: %s comparable, %s disagree (%.3f%%), limit %.1f%%",
                 format(nrow(state_comparable), big.mark = ","), format(nrow(state_mismatches), big.mark = ","),
                 100 * state_mismatch_share, 100 * STATE_MISMATCH_MAX_SHARE))

state_mismatches |>
  select(f_address, state, spell_state_fips, block_state_fips, block_geoid_2020,
         geo_match_addr, geo_addr_type, geo_score, geo_lat, geo_lon) |>
  data.table::fwrite(paste0(OUT_STEM, "_audit.csv"))

if (state_mismatch_share > STATE_MISMATCH_MAX_SHARE) {
  stop(sprintf("State check failed: %.3f%% of placed addresses sit in a different state than the spell says (limit %.1f%%). See %s_audit.csv",
               100 * state_mismatch_share, 100 * STATE_MISMATCH_MAX_SHARE, OUT_STEM))
}

# ---------------------------------------------------------------------------
# 4. One row per spell
# ---------------------------------------------------------------------------

geography_columns <- c("block_geoid_2020", "block_geoid_2010", "zcta_2020", "congressional_district_119",
                       "latitude", "longitude", "geo_match_level", "geo_addr_type", "geo_score",
                       "geo_match_addr", "org_addr_is_po_box")

crosswalk <- address_spells |>
  select(spell_id, EIN2, ein, ein_prefixed, spell_rank, street, city, state, zip5) |>
  left_join(
    address_geography |>
      transmute(street, city, state, zip5,
                block_geoid_2020, block_geoid_2010, zcta_2020, congressional_district_119,
                latitude = geo_lat, longitude = geo_lon,
                geo_match_level, geo_addr_type, geo_score, geo_match_addr, org_addr_is_po_box,
                geo_state_abbr),   # for the trial-run filter below only; not published
    by = c("street", "city", "state", "zip5")
  ) |>
  mutate(
    geo_match_level    = if_else(is.na(street), "not_geocoded", geo_match_level),
    org_addr_is_po_box = if_else(is.na(street), NA, org_addr_is_po_box)
  ) |>
  arrange(ein, spell_rank)

# ---------------------------------------------------------------------------
# 5. Current addresses agree with the published census table (ADR 0051 §6)
#    Compared where the rank-0 spell is the same address the Unified BMF
#    holds for that organization; rows where the address history is newer
#    than the census table are counted, not compared.
# ---------------------------------------------------------------------------

log_info("Comparing current addresses with the census-geo-resolved crosswalk")
unified_current_addresses <- arrow::read_parquet(
  GEOCODED_UNIFIED_PATH,
  col_select = c("ein", "org_addr_street_raw", "org_addr_city_raw", "org_addr_state_raw", "org_addr_zip_raw")
) |>
  transmute(ein,
            street = address_normalize_text(org_addr_street_raw),
            city   = address_normalize_text(org_addr_city_raw),
            state  = address_normalize_text(org_addr_state_raw),
            zip5   = address_normalize_zip5(org_addr_zip_raw))

census_crosswalk <- arrow::read_parquet(
  CENSUS_CROSSWALK_PATH,
  col_select = c("ein", "block_geoid_2020", "block_geoid_2010", "zcta_2020", "congressional_district")
)

# A trial run assigns blocks only in the requested states, so it compares only
# addresses the geocoder placed in those states (an organization can list one
# state and sit in another; the state check above counts those).
states_compared <- if (nzchar(STATE_SUBSET)) names(state_fips_by_abbr)[state_fips_by_abbr %in% states_to_build] else unique(crosswalk$geo_state_abbr)

current_comparison <- crosswalk |>
  filter(spell_rank == 0L, !is.na(street), geo_state_abbr %in% states_compared) |>
  semi_join(unified_current_addresses, by = c("ein", "street", "city", "state", "zip5")) |>
  inner_join(census_crosswalk, by = "ein", suffix = c("", "_census")) |>
  mutate(agrees = coalesce(block_geoid_2020 == block_geoid_2020_census, is.na(block_geoid_2020) & is.na(block_geoid_2020_census)) &
                  coalesce(block_geoid_2010 == block_geoid_2010_census, is.na(block_geoid_2010) & is.na(block_geoid_2010_census)) &
                  coalesce(zcta_2020 == zcta_2020_census,               is.na(zcta_2020) & is.na(zcta_2020_census)) &
                  coalesce(congressional_district_119 == congressional_district, is.na(congressional_district_119) & is.na(congressional_district)))

current_disagreements <- filter(current_comparison, !agrees)
log_info(sprintf("Current addresses compared: %s; disagreements: %s",
                 format(nrow(current_comparison), big.mark = ","), format(nrow(current_disagreements), big.mark = ",")))

if (nrow(current_disagreements) > 0L) {
  data.table::fwrite(current_disagreements, paste0(OUT_STEM, "_current_address_disagreements.csv"))
  stop(sprintf("%d current addresses carry different geography than the census-geo-resolved crosswalk; see %s_current_address_disagreements.csv",
               nrow(current_disagreements), OUT_STEM))
}

# ---------------------------------------------------------------------------
# 6. Checks before anything is written (ADR 0051 Acceptance)
# ---------------------------------------------------------------------------

no_geography_levels <- c("no_match", "not_geocoded")
rows_without_geography <- filter(crosswalk, geo_match_level %in% no_geography_levels)

stopifnot(
  "row count differs from the address history"           = nrow(crosswalk) == nrow(address_spells),
  "spell_id repeats"                                       = !anyDuplicated(crosswalk$spell_id),
  "spell_id set differs from the address history"          = setequal(crosswalk$spell_id, address_spells$spell_id),
  "geo_match_level has empty values"                       = !anyNA(crosswalk$geo_match_level),
  "a spell without a street is not not_geocoded"           = all(crosswalk$geo_match_level[is.na(crosswalk$street)] == "not_geocoded"),
  "a spell with a street is marked not_geocoded"           = !any(crosswalk$geo_match_level[!is.na(crosswalk$street)] == "not_geocoded"),
  "no_match or not_geocoded rows carry geography"          = all(is.na(rows_without_geography$block_geoid_2020) & is.na(rows_without_geography$zcta_2020) &
                                                                 is.na(rows_without_geography$latitude)),
  "a block is assigned outside the point-level tiers"      = all(crosswalk$geo_addr_type[!is.na(crosswalk$block_geoid_2020)] %in% CENSUS_GEO_POINT_LEVEL_TYPES)
)

# ---------------------------------------------------------------------------
# 7. Write the table, the sample, the dictionary and the summary
# ---------------------------------------------------------------------------

crosswalk_out <- select(crosswalk, spell_id, EIN2, ein, ein_prefixed, all_of(geography_columns))

arrow::write_parquet(crosswalk_out, paste0(OUT_STEM, ".parquet"), compression = "zstd")
data.table::fwrite(crosswalk_out, paste0(OUT_STEM, ".csv"))

set.seed(2051)
crosswalk_out |>
  slice_sample(n = SAMPLE_ROWS) |>
  data.table::fwrite(paste0(OUT_STEM, "_sample.csv"))

dictionary <- tibble::tribble(
  ~column, ~description,
  "spell_id",                   "Stable identifier of one organization at one address. Join key to the address-resolved crosswalk (same column there). It does not change between builds; spell_rank does.",
  "EIN2",                       "Employer Identification Number, EIN-XX-XXXXXXX (ADR 0036).",
  "ein",                        "Employer Identification Number, XX-XXXXXXX.",
  "ein_prefixed",               "Coercion-safe EIN key, ein-XX-XXXXXXX (ADR 0036).",
  "block_geoid_2020",           "15-digit census block GEOID on 2020 boundaries (TIGER/Line 2020). Tract = first 11 digits, block group = first 12, county = first 5, state = first 2. Empty unless the address was geocoded to a specific address (geo_match_level address or po_box).",
  "block_geoid_2010",           "15-digit census block GEOID on 2010 boundaries (TIGER/Line 2010), for joins to pre-2020 census products. Same prefix rules. Empty as above.",
  "zcta_2020",                  "5-digit ZIP Code Tabulation Area, 2020 boundaries. A ZCTA is an area; a ZIP code is a delivery route. Empty as above.",
  "congressional_district_119", "4-digit district GEOID (2-digit state FIPS + 2-digit district number) for the 119th Congress, from TIGER/Line 2024. Empty as above.",
  "latitude",                   "Latitude returned by the geocoder (WGS 84). Empty when no_match or not_geocoded.",
  "longitude",                  "Longitude returned by the geocoder (WGS 84). Empty when no_match or not_geocoded.",
  "geo_match_level",            "How the address was placed. not_geocoded: the spell has no street (pre-2009 records) and was not sent. no_match: sent, no coordinates came back. po_box: the street is a post office box, so the point and block are the post office's, not the organization's. address: matched to a specific address (PointAddress, Subaddress, StreetAddress, StreetAddressExt, StreetInt). zip: ZIP code centre only (Postal, PostalExt, PostalLoc). city: city or place only (Locality). other: any other tier with coordinates (street name, point of interest). Only address and po_box rows carry a block, ZCTA and district.",
  "geo_addr_type",              "Geocoder precision tier as returned.",
  "geo_score",                  "Geocoder match confidence, 0 to 100.",
  "geo_match_addr",             "The address the geocoder matched, as it matched it. Lets a user see what the coordinates stand for. Past addresses are placed on today's street network.",
  "org_addr_is_po_box",         "TRUE when the street is a post office box. Empty when the spell has no street."
)
data.table::fwrite(dictionary, paste0(OUT_STEM, "_data_dictionary.csv"))

summary <- list(
  address_history_rows        = nrow(address_spells),
  rows                        = nrow(crosswalk_out),
  distinct_addresses          = nrow(distinct_addresses),
  distinct_addresses_geocoded = sum(!is.na(address_geocodes$geo_lat)),
  by_match_level              = as.list(table(crosswalk_out$geo_match_level)),
  assigned_block_2020         = sum(!is.na(crosswalk_out$block_geoid_2020)),
  assigned_block_2010         = sum(!is.na(crosswalk_out$block_geoid_2010)),
  assigned_zcta_2020          = sum(!is.na(crosswalk_out$zcta_2020)),
  assigned_congressional_district_119 = sum(!is.na(crosswalk_out$congressional_district_119)),
  state_check                 = list(n_comparable = nrow(state_comparable), n_mismatch = nrow(state_mismatches),
                                     mismatch_share = state_mismatch_share, limit = STATE_MISMATCH_MAX_SHARE),
  current_address_comparison  = list(n_compared = nrow(current_comparison), n_disagree = nrow(current_disagreements)),
  tiger                       = list(blocks_2020 = TIGER_YEAR_2020, blocks_2010 = TIGER_YEAR_2010, zcta = TIGER_YEAR_2020,
                                     congressional_districts = CD_TIGER_YEAR, congress_session = 119L),
  states_built                = states_to_build,
  built_at                    = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
)
jsonlite::write_json(summary, paste0(OUT_STEM, "_summary.json"), auto_unbox = TRUE, pretty = TRUE)

log_info(sprintf("Wrote %s.{parquet,csv} (%s rows), sample, dictionary, audit and summary",
                 OUT_STEM, format(nrow(crosswalk_out), big.mark = ",")))
