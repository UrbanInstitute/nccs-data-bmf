# ============================================================================
# census_geo_assign.R
#
# Point-in-polygon assignment of census geography to coordinates: given the
# longitude and latitude of an address, find the census block, ZIP Code
# Tabulation Area or congressional district whose boundary contains it. Used
# by scripts/build_address_geo_resolved_crosswalk.R (ADR 0051, one point per
# distinct historical address). scripts/build_census_geo_resolved_crosswalk.R
# (ADR 0045, one point per organization) carries its own earlier copy of the
# same steps. Needs sf and tigris; TIGER/Line boundary files are downloaded
# through tigris and cached.
#
# Blocks are published one file per state, so block assignment runs state by
# state. ZIP Code Tabulation Areas and congressional districts are each
# published as one national file, so those two run once over every point.
#
# Coordinates are longitude and latitude, and the Census draws block
# boundaries as straight lines between such vertices, so the containment
# tests run in planar (flat) mode: sf_use_s2(FALSE), set by the calling
# script. sf::st_within() uses a spatial index, so each point is tested only
# against nearby polygons.
# ============================================================================

#' Index of the polygon containing each point.
#'
#' sf::st_within() returns, for each point, the indices of every polygon that
#' contains it: usually one, none for a point outside every polygon, and
#' occasionally two for a point exactly on a shared boundary. This keeps the
#' first.
#'
#' @param hits List of integer vectors, one per point: the output of
#'   sf::st_within(points, polygons).
#' @return Integer vector, one per point: the row number of the containing
#'   polygon, or NA when no polygon contains the point.
first_polygon_index <- function(hits) {

  first_index_or_missing <- function(polygon_indices) {

    if (length(polygon_indices) == 0L) {

      return(NA_integer_)

    }

    return(polygon_indices[[1]])

  }

  polygon_index <- purrr::map_int(hits, first_index_or_missing)

  return(polygon_index)

}

#' Census block for each point of one state, under one boundary year.
#'
#' Downloads (or reads from the tigris cache) the state's block boundaries
#' for the given year and finds the block containing each point.
#'
#' @param state_points Tibble or data frame, one row per point, all in one
#'   state, with numeric columns geo_lon and geo_lat (WGS 84 degrees). Other
#'   columns are ignored.
#' @param state_fips Character: the state's 2-digit FIPS code, for example
#'   "11" for the District of Columbia.
#' @param tiger_year Integer: the TIGER/Line boundary year, 2020L or 2010L.
#' @return Character vector aligned to the rows of `state_points`: the
#'   15-digit block GEOID, or NA for a point outside every block of the state.
#' @details The "assumes that they are planar" message from sf is silenced on
#'   purpose: planar is the intended mode here (see the file header).
assign_blocks_for_state <- function(state_points, state_fips, tiger_year) {

  geoid_column <- if (tiger_year == 2020L) "GEOID20" else "GEOID10"

  blocks <- tigris::blocks(state = state_fips, year = tiger_year, progress_bar = FALSE) |>
    sf::st_transform(4326) |>
    dplyr::select(dplyr::all_of(geoid_column))

  points_sf <- sf::st_as_sf(
    state_points,
    coords = c("geo_lon", "geo_lat"),
    crs    = 4326,
    remove = FALSE
  )

  hits <- suppressMessages(sf::st_within(points_sf, blocks))

  block_geoids <- blocks[[geoid_column]][first_polygon_index(hits)]

  return(block_geoids)

}

#' 2020 ZIP Code Tabulation Area for each point.
#'
#' The Census publishes ZIP Code Tabulation Areas as one national file, so
#' this runs once over every point (unlike blocks, which are per state).
#'
#' @param points_sf sf object of points (WGS 84, EPSG 4326), one row per
#'   point: the output of sf::st_as_sf() on the longitude and latitude.
#' @return Character vector aligned to the rows of `points_sf`: the 5-digit
#'   ZIP Code Tabulation Area, or NA for a point outside every area.
assign_zcta_2020 <- function(points_sf) {

  zcta <- tigris::zctas(year = 2020L, progress_bar = FALSE) |>
    sf::st_transform(4326) |>
    dplyr::select(ZCTA5CE20)

  hits <- suppressMessages(sf::st_within(points_sf, zcta))

  zcta_codes <- zcta$ZCTA5CE20[first_polygon_index(hits)]

  return(zcta_codes)

}

#' Congressional district for each point.
#'
#' The Census publishes congressional districts as one national file, so this
#' runs once over every point (unlike blocks, which are per state). The file
#' also says which Congress its districts belong to (119 for TIGER 2024),
#' which is returned so the caller can check it.
#'
#' @param points_sf sf object of points (WGS 84, EPSG 4326), one row per
#'   point: the output of sf::st_as_sf() on the longitude and latitude.
#' @param cd_tiger_year Integer: the TIGER/Line year of the district file,
#'   for example 2024L.
#' @return list(district = character vector aligned to the rows of
#'   `points_sf`, the 4-digit district GEOID (2-digit state FIPS + 2-digit
#'   district number) or NA; congress_session = integer, the Congress the
#'   districts belong to).
assign_congressional_districts <- function(points_sf, cd_tiger_year) {

  districts <- tigris::congressional_districts(year = cd_tiger_year, progress_bar = FALSE) |>
    sf::st_transform(4326)

  congress_session <- as.integer(unique(districts$CDSESSN)[[1]])

  district_geoids <- dplyr::select(districts, GEOID)

  hits <- suppressMessages(sf::st_within(points_sf, district_geoids))

  district_for_point <- district_geoids$GEOID[first_polygon_index(hits)]

  return(list(
    district         = district_for_point,
    congress_session = congress_session
  ))

}
