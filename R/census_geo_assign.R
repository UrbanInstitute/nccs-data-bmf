# ============================================================================
# census_geo_assign.R
#
# Point-in-polygon assignment of census geography to coordinates, shared by
# scripts/build_census_geo_resolved_crosswalk.R (ADR 0045, one point per
# organization) and scripts/build_address_geo_resolved_crosswalk.R (ADR
# 0051, one point per distinct historical address). Needs sf and tigris;
# TIGER/Line files are downloaded through tigris and cached.
#
# Coordinates are longitude and latitude, and the Census draws block
# boundaries as straight lines between such vertices, so the predicates run
# in planar mode (sf_use_s2(FALSE), set by the calling script). st_within()
# uses a spatial index, so each point is tested only against nearby polygons.
# ============================================================================

# st_within() returns, for each point, the indices of the polygons containing
# it (usually one, none for points outside every polygon). Keep the first.
first_polygon_index <- function(hits) {
  purrr::map_int(hits, function(indices) if (length(indices) > 0) indices[[1]] else NA_integer_)
}

# Block GEOID for each point of one state under one boundary vintage.
# `state_points` needs geo_lon and geo_lat; the return is aligned to its rows.
assign_blocks_for_state <- function(state_points, state_fips, tiger_year) {
  geoid_column <- if (tiger_year == 2020L) "GEOID20" else "GEOID10"

  blocks <- tigris::blocks(state = state_fips, year = tiger_year, progress_bar = FALSE) |>
    sf::st_transform(4326) |>
    dplyr::select(dplyr::all_of(geoid_column))

  points_sf <- sf::st_as_sf(state_points, coords = c("geo_lon", "geo_lat"), crs = 4326, remove = FALSE)

  # The "assumes that they are planar" message is silenced on purpose: planar
  # is the intended mode here (see the header).
  hits <- suppressMessages(sf::st_within(points_sf, blocks))

  blocks[[geoid_column]][first_polygon_index(hits)]
}

# 2020 ZIP Code Tabulation Area for each point (national file).
assign_zcta_2020 <- function(points_sf) {
  zcta <- tigris::zctas(year = 2020L, progress_bar = FALSE) |>
    sf::st_transform(4326) |>
    dplyr::select(ZCTA5CE20)

  hits <- suppressMessages(sf::st_within(points_sf, zcta))

  zcta$ZCTA5CE20[first_polygon_index(hits)]
}

# Congressional district GEOID for each point, plus the Congress the TIGER
# file says the districts belong to (119 for TIGER 2024).
assign_congressional_districts <- function(points_sf, cd_tiger_year) {
  districts <- tigris::congressional_districts(year = cd_tiger_year, progress_bar = FALSE) |>
    sf::st_transform(4326)

  congress_session <- unique(districts$CDSESSN)[[1]]
  districts        <- dplyr::select(districts, GEOID)

  hits <- suppressMessages(sf::st_within(points_sf, districts))

  list(
    district         = districts$GEOID[first_polygon_index(hits)],
    congress_session = as.integer(congress_session)
  )
}
