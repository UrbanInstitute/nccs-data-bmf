# ============================================================================
# build_census_geo_resolved_crosswalk.R
#
# Builds the census-geo-resolved crosswalk (nccs-contracts ADR 0045): one row
# per EIN from the geocoded Unified BMF with the census block the
# organization's address falls in, for the 2020 and the 2010 boundaries,
# plus the 2020 ZIP Code Tabulation Area and the congressional district.
# Tract, block group, county and state are prefixes of the block GEOID and
# are derived by the consumer, not stored (see R/census_geo_resolved.R).
#
# Only address-level geocodes get a block (ADR 0045 §3); the rest get NA.
# A county-consistency gate compares each block's county with the county
# the geocoder named and stops the build if they disagree too often.
#
# Inputs
#   data/geocoding/master/merged/bmf_unified_geocoded.parquet (+ _manifest.json)
#   data/county_fips_crosswalk.parquet   (published county-fips crosswalk, for the gate)
#   TIGER/Line via tigris (cached): blocks per state for 2020 and 2010,
#   ZCTAs 2020 (national), congressional districts (119th Congress).
#
# Outputs (data/crosswalks/)
#   census_geo_resolved_crosswalk.parquet / .csv
#   census_geo_resolved_crosswalk_data_dictionary.csv
#   census_geo_resolved_crosswalk_audit.csv     (county mismatches)
#   census_geo_resolved_crosswalk_summary.json  (coverage by tier, gate result)
#
# Run from the repo root (a laptop-scale job; state by state):
#   Rscript scripts/build_census_geo_resolved_crosswalk.R
# Optional: CENSUS_GEO_STATES="DE,RI" to build a subset for a trial run.
# ============================================================================

suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
})
options(tigris_use_cache = TRUE)
sf::sf_use_s2(FALSE)   # planar predicates are what block shapefiles expect

source(here::here("R", "census_geo_resolved.R"))
source(here::here("R", "ein.R"))

GEOCODED_PATH     <- here::here("data", "geocoding", "master", "merged", "bmf_unified_geocoded.parquet")
GEOCODED_MANIFEST <- here::here("data", "geocoding", "master", "merged", "_manifest.json")
COUNTY_XWALK_PATH <- here::here("data", "county_fips_crosswalk.parquet")
OUT_DIR           <- here::here("data", "crosswalks")
OUT_STEM          <- file.path(OUT_DIR, "census_geo_resolved_crosswalk")

TIGER_YEAR_2020   <- 2020L
TIGER_YEAR_2010   <- 2010L
CD_TIGER_YEAR     <- 2024L   # TIGER 2024 carries the 119th Congress districts
STATE_SUBSET      <- Sys.getenv("CENSUS_GEO_STATES", "")

if (!dir.exists(OUT_DIR)) dir.create(OUT_DIR, recursive = TRUE)
log_line <- function(...) message(sprintf("[%s] %s", format(Sys.time(), "%H:%M:%S"), sprintf(...)))

# ---------------------------------------------------------------------------
# 1. Read the geocoded Unified BMF and choose the rows that get a block
# ---------------------------------------------------------------------------

log_line("Reading the geocoded Unified BMF")
unified <- arrow::read_parquet(
  GEOCODED_PATH,
  col_select = c("ein", "geo_lat", "geo_lon", "geo_addr_type", "geo_score",
                 "geo_state_abbr", "geo_county", "org_addr_is_po_box")
) |>
  tibble::as_tibble() |>
  mutate(
    geo_score          = suppressWarnings(as.numeric(geo_score)),
    org_addr_is_po_box = org_addr_is_po_box %in% c("TRUE", TRUE),
    point_level        = census_geo_is_point_level(geo_addr_type, geo_lat, geo_lon)
  )

source_vintage <- jsonlite::fromJSON(GEOCODED_MANIFEST)$vintage
log_line("Rows: %s; geocoded: %s; address-level (get a block): %s",
         format(nrow(unified), big.mark = ","),
         format(sum(!is.na(unified$geo_lat)), big.mark = ","),
         format(sum(unified$point_level), big.mark = ","))

# State FIPS per USPS abbreviation (50 states, DC, territories).
state_fips_by_abbr <- tigris::fips_codes |>
  distinct(state, state_code) |>
  tibble::deframe()

points <- unified |>
  filter(point_level) |>
  mutate(state_fips = unname(state_fips_by_abbr[geo_state_abbr])) |>
  filter(!is.na(state_fips))

states_to_build <- sort(unique(points$state_fips))
if (nzchar(STATE_SUBSET)) {
  wanted <- unname(state_fips_by_abbr[trimws(strsplit(STATE_SUBSET, ",")[[1]])])
  states_to_build <- intersect(states_to_build, wanted)
  points <- filter(points, state_fips %in% states_to_build)
  log_line("Trial run limited to %d state(s)", length(states_to_build))
}

# ---------------------------------------------------------------------------
# 2. Point-in-polygon per state and boundary vintage
# ---------------------------------------------------------------------------

assign_blocks_for_state <- function(state_pts, state_fips, tiger_year) {
  geoid_column <- if (tiger_year == 2020L) "GEOID20" else "GEOID10"
  blocks <- tigris::blocks(state = state_fips, year = tiger_year, progress_bar = FALSE) |>
    st_transform(4326) |>
    select(all_of(geoid_column))
  pts_sf <- st_as_sf(state_pts, coords = c("geo_lon", "geo_lat"), crs = 4326, remove = FALSE)
  hit <- suppressMessages(st_within(pts_sf, blocks))   # spatially indexed; planar message silenced (sf_use_s2 is FALSE on purpose)
  first_hit <- vapply(hit, function(i) if (length(i) > 0) i[[1]] else NA_integer_, integer(1))
  blocks[[geoid_column]][first_hit]
}

block_2020 <- rep(NA_character_, nrow(points))
block_2010 <- rep(NA_character_, nrow(points))

for (state_fips in states_to_build) {
  rows <- which(points$state_fips == state_fips)
  state_pts <- points[rows, c("ein", "geo_lon", "geo_lat")]
  log_line("State %s: %s points", state_fips, format(length(rows), big.mark = ","))
  block_2020[rows] <- assign_blocks_for_state(state_pts, state_fips, TIGER_YEAR_2020)
  block_2010[rows] <- assign_blocks_for_state(state_pts, state_fips, TIGER_YEAR_2010)
  log_line("  assigned 2020: %s | 2010: %s",
           format(sum(!is.na(block_2020[rows])), big.mark = ","),
           format(sum(!is.na(block_2010[rows])), big.mark = ","))
}
points$block_geoid_2020 <- block_2020
points$block_geoid_2010 <- block_2010

# ---------------------------------------------------------------------------
# 3. ZCTA (2020) and congressional district (119th Congress), national files
# ---------------------------------------------------------------------------

pts_sf <- st_as_sf(points, coords = c("geo_lon", "geo_lat"), crs = 4326, remove = FALSE)

log_line("ZCTA 2020 (national)")
zcta <- tigris::zctas(year = TIGER_YEAR_2020, progress_bar = FALSE) |>
  st_transform(4326) |> select(ZCTA5CE20)
zcta_hit <- suppressMessages(st_within(pts_sf, zcta))
points$zcta_2020 <- zcta$ZCTA5CE20[vapply(zcta_hit, function(i) if (length(i) > 0) i[[1]] else NA_integer_, integer(1))]
rm(zcta, zcta_hit); invisible(gc())

log_line("Congressional districts (TIGER %d)", CD_TIGER_YEAR)
districts <- tigris::congressional_districts(year = CD_TIGER_YEAR, progress_bar = FALSE) |>
  st_transform(4326)
cd_session <- unique(districts$CDSESSN)[[1]]
districts <- select(districts, GEOID)
cd_hit <- suppressMessages(st_within(pts_sf, districts))
points$congressional_district <- districts$GEOID[vapply(cd_hit, function(i) if (length(i) > 0) i[[1]] else NA_integer_, integer(1))]
rm(districts, cd_hit, pts_sf); invisible(gc())

# ---------------------------------------------------------------------------
# 4. County-consistency gate (ADR 0045 §4)
# ---------------------------------------------------------------------------

county_xwalk <- arrow::read_parquet(COUNTY_XWALK_PATH) |>
  filter(resolution == "resolved") |>
  select(geo_state_abbr, geo_county_raw, geo_county_fips)

points <- points |>
  left_join(county_xwalk, by = c("geo_state_abbr", "geo_county" = "geo_county_raw"))

gate <- census_geo_county_gate(points$block_geoid_2020, points$geo_county_fips)
log_line("County gate: %s comparable, %s mismatches (%.3f%%), limit %.1f%%",
         format(gate$n_comparable, big.mark = ","), format(gate$n_mismatch, big.mark = ","),
         100 * gate$mismatch_share, 100 * CENSUS_GEO_COUNTY_MISMATCH_MAX_SHARE)

audit <- points[gate$mismatch_rows, ] |>
  transmute(ein, geo_state_abbr, geo_county, geocoder_county_fips = geo_county_fips,
            block_county_fips = census_geo_county_from_block(block_geoid_2020),
            block_geoid_2020, geo_addr_type, geo_score, geo_lat, geo_lon)
data.table::fwrite(audit, paste0(OUT_STEM, "_audit.csv"))

if (gate$mismatch_share > CENSUS_GEO_COUNTY_MISMATCH_MAX_SHARE) {
  stop(sprintf("County gate failed: %.3f%% of blocks sit in a different county than the geocoder named (limit %.1f%%). See %s_audit.csv",
               100 * gate$mismatch_share, 100 * CENSUS_GEO_COUNTY_MISMATCH_MAX_SHARE, OUT_STEM))
}

# ---------------------------------------------------------------------------
# 5. Assemble one row per EIN (every EIN in the Unified BMF; NA where not assigned)
# ---------------------------------------------------------------------------

crosswalk <- unified |>
  select(ein, geo_addr_type, geo_score, org_addr_is_po_box) |>
  left_join(
    points |> select(ein, block_geoid_2020, block_geoid_2010, zcta_2020, congressional_district),
    by = "ein"
  ) |>
  mutate(
    ein_prefixed          = ein_to_prefixed(ein),
    EIN2                  = ein_to_ein2(ein),
    tiger_year_2020       = TIGER_YEAR_2020,
    tiger_year_2010       = TIGER_YEAR_2010,
    congress_session      = as.integer(cd_session),
    source_vintage        = source_vintage
  ) |>
  select(ein, ein_prefixed, EIN2,
         block_geoid_2020, block_geoid_2010, zcta_2020, congressional_district,
         geo_addr_type, geo_score, org_addr_is_po_box,
         tiger_year_2020, tiger_year_2010, congress_session, source_vintage) |>
  arrange(ein)

arrow::write_parquet(crosswalk, paste0(OUT_STEM, ".parquet"), compression = "zstd")
data.table::fwrite(crosswalk, paste0(OUT_STEM, ".csv"))

dictionary <- tibble::tribble(
  ~column, ~description,
  "ein",                    "Employer Identification Number, XX-XXXXXXX. One row per EIN in the geocoded Unified BMF.",
  "ein_prefixed",           "Coercion-safe EIN key, ein-XX-XXXXXXX (ADR 0036).",
  "EIN2",                   "Legacy-compatibility EIN key, EIN-XX-XXXXXXX (ADR 0036).",
  "block_geoid_2020",       "15-digit census block GEOID on 2020 boundaries (TIGER/Line 2020). Tract = first 11 digits, block group = first 12, county = first 5, state = first 2. NA when the address was not geocoded to address level.",
  "block_geoid_2010",       "15-digit census block GEOID on 2010 boundaries (TIGER/Line 2010), for joins to pre-2020 census products. Same prefix rules. NA as above.",
  "zcta_2020",              "5-digit ZIP Code Tabulation Area, 2020 boundaries. A ZCTA is an area; a ZIP code is a delivery route. NA as above.",
  "congressional_district", "4-digit district GEOID (2-digit state FIPS + 2-digit district number) for the Congress in congress_session, from TIGER/Line 2024. Redistricting changes these on a different clock from the decennial census. NA as above.",
  "geo_addr_type",          "Geocoder precision tier. Blocks are assigned only for PointAddress, Subaddress, StreetAddress, StreetAddressExt and StreetInt; Postal, PostalExt, PostalLoc, StreetName, POI, Locality and DistanceMarker matches get NA.",
  "geo_score",              "Geocoder match confidence, 0 to 100.",
  "org_addr_is_po_box",     "TRUE when the mailing address is a PO box: the geocode then locates the post office, not the organization.",
  "tiger_year_2020",        "TIGER/Line vintage used for block_geoid_2020 (2020).",
  "tiger_year_2010",        "TIGER/Line vintage used for block_geoid_2010 (2010).",
  "congress_session",       "Congress whose districts congressional_district refers to (119).",
  "source_vintage",         "Vintage of the geocoded Unified BMF the coordinates came from."
)
data.table::fwrite(dictionary, paste0(OUT_STEM, "_data_dictionary.csv"))

summary <- list(
  source_vintage = source_vintage,
  rows = nrow(crosswalk),
  geocoded = sum(!is.na(unified$geo_lat)),
  address_level = sum(unified$point_level),
  assigned_block_2020 = sum(!is.na(crosswalk$block_geoid_2020)),
  assigned_block_2010 = sum(!is.na(crosswalk$block_geoid_2010)),
  assigned_zcta_2020 = sum(!is.na(crosswalk$zcta_2020)),
  assigned_congressional_district = sum(!is.na(crosswalk$congressional_district)),
  by_addr_type = as.list(table(unified$geo_addr_type[!is.na(unified$geo_lat)])),
  county_gate = gate[c("n_comparable", "n_mismatch", "mismatch_share")],
  county_gate_limit = CENSUS_GEO_COUNTY_MISMATCH_MAX_SHARE,
  tiger = list(blocks_2020 = TIGER_YEAR_2020, blocks_2010 = TIGER_YEAR_2010, zcta = TIGER_YEAR_2020,
               congressional_districts = CD_TIGER_YEAR, congress_session = as.integer(cd_session)),
  states_built = states_to_build,
  built_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
)
jsonlite::write_json(summary, paste0(OUT_STEM, "_summary.json"), auto_unbox = TRUE, pretty = TRUE)
log_line("Wrote %s.{parquet,csv} (%s rows), dictionary, audit and summary", OUT_STEM, format(nrow(crosswalk), big.mark = ","))
