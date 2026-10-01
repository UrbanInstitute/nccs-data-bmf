# ============================================================================
# census_geo_resolved.R
#
# Pure helpers for the census-geo-resolved crosswalk (nccs-contracts ADR
# 0045): which geocodes qualify for a block assignment, how a block GEOID
# unfolds into its parents, and the county-consistency gate. No spatial
# libraries here, so these can be unit-tested without TIGER downloads. The
# point-in-polygon work lives in scripts/build_census_geo_resolved_crosswalk.R.
# ============================================================================

# Geocoder precision tiers that place an organization at (or interpolated
# along) its street address. Only these get a block. ZIP-centroid, street-
# name, place and point-of-interest matches get NA: a plausible-but-wrong
# block is worse than a missing one (ADR 0045 §3).
CENSUS_GEO_POINT_LEVEL_TYPES <- c(
  "PointAddress", "Subaddress", "StreetAddress", "StreetAddressExt", "StreetInt"
)

# Share of gated rows whose block's county may disagree with the county the
# geocoder named before the build stops. Border addresses legitimately
# disagree now and then; a rate above this points at a geocoding defect.
CENSUS_GEO_COUNTY_MISMATCH_MAX_SHARE <- 0.01

# TRUE for rows whose geocode is precise enough for a block assignment.
census_geo_is_point_level <- function(geo_addr_type, geo_lat, geo_lon) {
  !is.na(geo_lat) & !is.na(geo_lon) & geo_addr_type %in% CENSUS_GEO_POINT_LEVEL_TYPES
}

# Geocoder tiers that place a point at a ZIP code centre or at a city, used by
# the address-history match level below (ADR 0051 §4).
CENSUS_GEO_ZIP_LEVEL_TYPES  <- c("Postal", "PostalExt", "PostalLoc")
CENSUS_GEO_CITY_LEVEL_TYPES <- c("Locality")

#' Plain-language match level for the address-geo-resolved crosswalk.
#'
#' Says how each address was placed (ADR 0051 §4). One value per spell,
#' never NA. The first rule that applies wins, in this order:
#'   not_geocoded  the spell has no street and was never sent
#'   no_match      sent to the geocoder, no coordinates came back
#'   po_box        the street is a post office box: the point is the post
#'                 office, not the organization (flagged whatever the tier)
#'   address       matched to a specific address (the point-level tiers)
#'   zip           matched to a ZIP code centre only
#'   city          matched to a city or place only
#'   other         any other tier with coordinates (street name, point of interest)
#'
#' @param has_street    Logical vector: TRUE when the spell has a street.
#' @param is_po_box     Logical vector: TRUE when the street is a post office
#'   box; NA counts as not a post office box.
#' @param geo_addr_type Character vector: the geocoder's precision tier, for
#'   example "PointAddress" or "Postal".
#' @param geo_lat       Numeric vector: latitude from the geocoder, NA when
#'   none came back.
#' @param geo_lon       Numeric vector: longitude from the geocoder, NA when
#'   none came back.
#' @return Character vector, one value per input row, from the list above.
address_geo_match_level <- function(has_street, is_po_box, geo_addr_type, geo_lat, geo_lon) {

  has_coordinates <- !is.na(geo_lat) & !is.na(geo_lon)

  match_level <- dplyr::case_when(
    !has_street                                     ~ "not_geocoded",
    !has_coordinates                                ~ "no_match",
    is_po_box %in% TRUE                             ~ "po_box",
    geo_addr_type %in% CENSUS_GEO_POINT_LEVEL_TYPES ~ "address",
    geo_addr_type %in% CENSUS_GEO_ZIP_LEVEL_TYPES   ~ "zip",
    geo_addr_type %in% CENSUS_GEO_CITY_LEVEL_TYPES  ~ "city",
    TRUE                                            ~ "other"
  )

  return(match_level)

}

# Census GEOIDs nest by prefix: block (15) > block group (12) > tract (11) >
# county (5) > state (2). Consumers derive these; the crosswalk stores only
# the block. These helpers document the derivation and serve the gate.
census_geo_tract_from_block <- function(block_geoid)  substr(block_geoid, 1, 11)
census_geo_county_from_block <- function(block_geoid) substr(block_geoid, 1, 5)
census_geo_state_from_block <- function(block_geoid)  substr(block_geoid, 1, 2)

# County-consistency gate: compare the county implied by each assigned
# block with the county the geocoder named (resolved to FIPS through the
# county-fips crosswalk). Returns a list with the comparison table and the
# mismatch share over rows where both sides are known.
census_geo_county_gate <- function(block_geoid, geocoder_county_fips) {
  block_county <- census_geo_county_from_block(block_geoid)
  comparable   <- !is.na(block_county) & !is.na(geocoder_county_fips)
  mismatch     <- comparable & block_county != geocoder_county_fips
  list(
    n_comparable   = sum(comparable),
    n_mismatch     = sum(mismatch),
    mismatch_share = if (sum(comparable) > 0) sum(mismatch) / sum(comparable) else 0,
    mismatch_rows  = which(mismatch)
  )
}
