# ============================================================================
# build_county_fips_crosswalk.R
#
# Build a small, reusable crosswalk mapping the geocoder's *dirty* county
# labels (geo_county) to the canonical Census county identity
# (FIPS GEOID + NAMELSAD), so downstream consumers can canonicalize names
# and filter by stable, collision-proof FIPS codes (e.g. Baltimore city
# 24510 vs Baltimore County 24005).
#
# This script is intentionally ISOLATED from the pipeline runtime: sf and
# tigris live here only. The pipeline never pins consumers to a TIGER vintage
# (ADR 0016); county boundaries are stable enough to be the safe exception.
#
# Inputs
#   data/crosswalks/_county_points_cache.parquet
#     Distinct (geo_state_abbr, geo_county) + a ~1km-gridded spread of
#     representative geocoded points, cached from
#     s3://nccsdata/geocoding/bmf-master/merged/bmf_master_geocoded.parquet
#     (single bucket read; see scripts/read_county_points.R).
#   tigris::counties(cb = TRUE, year = TIGER_YEAR)  [downloaded from Census]
#
# Outputs
#   data/crosswalks/county_fips_crosswalk.parquet  (one row per state+raw county)
#     geo_state_abbr        chr  USPS state/territory abbreviation
#     geo_county_raw        chr  dirty geocoder county label (join key)
#     geo_county_fips       chr  5-char county GEOID, leading zeros (NA if unresolved)
#     state_fips            chr  2-char state FIPS, leading zeros
#     geo_county_canonical  chr  Census NAMELSAD (NA if unresolved/ambiguous)
#     resolution            chr  "resolved" | "ambiguous" | "unresolved"
#                                | "deferred_ct_planning_region" (CT old-county
#                                  labels -> coordinate-keyed companion)
#     tiger_year            int  TIGER/Census vintage of the boundaries
#   data/crosswalks/county_fips_crosswalk_audit.csv  (ambiguous + unresolved only)
#
# Run:  Rscript scripts/build_county_fips_crosswalk.R
#       TIGER_YEAR=2023 Rscript scripts/build_county_fips_crosswalk.R
# ============================================================================

suppressMessages({
  library(sf)
  library(tigris)
  library(dplyr)
  library(arrow)
  library(stringi)
})
options(tigris_use_cache = TRUE)

TIGER_YEAR <- as.integer(Sys.getenv("TIGER_YEAR", "2023"))
here_root  <- tryCatch(here::here(), error = function(e) getwd())

cache_path <- file.path(here_root, "data", "crosswalks", "_county_points_cache.parquet")
out_path   <- file.path(here_root, "data", "crosswalks", "county_fips_crosswalk.parquet")
out_csv    <- file.path(here_root, "data", "crosswalks", "county_fips_crosswalk.csv")
audit_path <- file.path(here_root, "data", "crosswalks", "county_fips_crosswalk_audit.csv")

stopifnot(file.exists(cache_path))

message(sprintf("[1/5] Loading cached points: %s", cache_path))
pts <- read_parquet(cache_path)
n_groups0 <- nrow(distinct(pts, geo_state_abbr, geo_county))
message(sprintf("      %d gridded points across %d (state, raw county) groups",
                nrow(pts), n_groups0))

message(sprintf("[2/5] Downloading TIGER counties (cb=TRUE, year=%d)", TIGER_YEAR))
co <- counties(cb = TRUE, year = TIGER_YEAR, progress_bar = FALSE)
co <- co[, c("GEOID", "STATEFP", "STUSPS", "NAMELSAD")]
co <- st_transform(co, 4326)  # cb files are NAD83 (4269); WGS84 diff is <1m
message(sprintf("      %d county polygons (%d states/territories)",
                nrow(co), length(unique(co$STUSPS))))

# --- Name gazetteer for name-based resolution ------------------------------
# The geocoder's geo_county IS a county name; matching it to the TIGER
# gazetteer is the authoritative signal for canonicalization. Point-in-polygon
# mass corroborates and is the fallback when the name does not match (e.g.
# TIGER renamed the unit). Normalize: lowercase, de-accent, drop punctuation,
# fold "City of X" -> "x city" and "Saint"->"st", then strip the trailing
# county-equivalent TYPE token (county/parish/borough/municipio/census area)
# but KEEP "city" so independent cities stay distinct from their namesake county.
norm_name <- function(x) {
  x <- stri_trans_general(tolower(trimws(x)), "Latin-ASCII")
  x <- gsub("[^a-z0-9 ]", " ", x)
  x <- gsub("\\s+", " ", trimws(x))
  x <- gsub("^city of (.+)$", "\\1 city", x)            # "City of Alexandria" -> "alexandria city"
  x <- gsub("\\bsainte?\\b", "st", x)                   # Saint/Sainte -> st
  x <- gsub(" (county|parish|borough|municipio|municipality|census area|city and borough|district)$",
            "", x)
  trimws(x)
}
co_attr <- sf::st_drop_geometry(co)
co_attr$nm <- norm_name(co_attr$NAMELSAD)

# Twin ambiguity: a base name present BOTH as an independent "<base> city" AND
# as a "<base>" county-equivalent in the same state (e.g. VA Richmond city +
# Richmond County, MD Baltimore city + Baltimore County). Only a BARE label
# (no explicit type token) is ambiguous between the two; an explicit
# "<base> County" / "<base> city" disambiguates itself.
is_city_nm <- grepl(" city$", tolower(co_attr$NAMELSAD))
city_base  <- norm_name(sub(" [Cc]ity$", "", co_attr$NAMELSAD[is_city_nm]))
city_twins <- intersect(
  unique(paste(co_attr$STUSPS[is_city_nm], city_base)),
  unique(paste(co_attr$STUSPS[!is_city_nm], co_attr$nm[!is_city_nm])))

# Matchable name table: the normalized NAMELSAD for every unit, PLUS a bare-base
# alias ("<base>" -> the city GEOID) for independent cities with no namesake
# county, so a bare "Portsmouth"/"Alexandria" resolves to its city by name
# rather than depending on point-mass. Twin bases are excluded (left to the
# twin guard). Uniqueness/collision falls out of the group-by below.
match_tbl <- data.frame(STUSPS = co_attr$STUSPS, GEOID = co_attr$GEOID,
                        nm = co_attr$nm, stringsAsFactors = FALSE)
alias <- data.frame(STUSPS = co_attr$STUSPS[is_city_nm],
                    GEOID = co_attr$GEOID[is_city_nm],
                    nm = city_base, stringsAsFactors = FALSE)
alias <- alias[!(paste(alias$STUSPS, alias$nm) %in% city_twins), ]
match_tbl <- rbind(match_tbl, alias)

name_idx <- match_tbl %>%
  group_by(STUSPS, nm) %>%
  summarise(geoids = list(unique(GEOID)), .groups = "drop")
name_lookup <- setNames(name_idx$geoids, paste(name_idx$STUSPS, name_idx$nm))
geoid_meta <- setNames(
  lapply(seq_len(nrow(co_attr)),
         function(i) list(statefp = co_attr$STATEFP[i], namelsad = co_attr$NAMELSAD[i])),
  co_attr$GEOID)

message("[3/5] Point-in-polygon (st_within)")
sf_pts <- st_as_sf(pts, coords = c("lon2", "lat2"), crs = 4326, remove = FALSE)
idx    <- st_within(sf_pts, co)                 # spatial-indexed
hit    <- vapply(idx, function(x) if (length(x)) x[[1]] else NA_integer_, integer(1))

resolved_pts <- pts %>%
  mutate(
    poly_geoid    = co$GEOID[hit],
    poly_statefp  = co$STATEFP[hit],
    poly_stusps   = co$STUSPS[hit],
    poly_namelsad = co$NAMELSAD[hit],
    state_match   = !is.na(poly_stusps) & poly_stusps == geo_state_abbr
  )

n_no_poly   <- sum(is.na(resolved_pts$poly_geoid))
n_xstate    <- sum(!is.na(resolved_pts$poly_geoid) & !resolved_pts$state_match)
message(sprintf("      points: %d total | %d no-polygon | %d cross-state strays",
                nrow(resolved_pts), n_no_poly, n_xstate))

message("[4/5] Resolving per (state, raw county): name match + org-mass corroboration")
# Resolution precedence:
#   1. Genuine ambiguity guards FIRST: a label that names 2+ in-state counties
#      (collision) or a bare base that has an independent-"<base> city" sibling
#      cannot be canonicalized -> ambiguous, regardless of mass.
#   2. Unique in-state NAME match -> resolve to it (authoritative for the
#      canonical name). If org mass overwhelmingly (>=90%) disagrees with the
#      name match, that is a conflict worth review -> ambiguous.
#   3. No name match -> fall back to org-mass: dominant >=90% resolves
#      (e.g. labels TIGER has renamed but whose orgs sit squarely in one unit);
#      otherwise ambiguous (CT old-county->planning-region splits, abolished
#      census areas, etc.).
# Mass is weighted by n_orgs per cell (where the orgs that carry a label sit),
# never by distinct cells, so a thin sliver of border spillover cannot outvote
# the core.
DOMINANT_THRESHOLD <- 0.90

# Connecticut: Census retired the 8 historical counties in 2022 for 9 planning
# regions. The geocoder still emits old "<name> County" labels, but a single
# old county genuinely splits across multiple planning regions, so it cannot be
# canonicalized at the (state, county) grain -- and mass-fallback would silently
# collapse the ones that happen to clear DOMINANT_THRESHOLD onto a single region
# (mislabeling the minority slivers). Defer every old-CT-county label to the
# coordinate-keyed companion (scripts/build_ct_planning_region_crosswalk.R).
CT_OLD_COUNTIES <- c("fairfield", "hartford", "litchfield", "middlesex",
                     "new haven", "new london", "tolland", "windham")

classify_group <- function(g, key) {
  state    <- key$geo_state_abbr
  raw      <- key$geo_county
  rawnm    <- norm_name(raw)
  in_state <- g[g$state_match, ]
  total    <- if (nrow(in_state)) sum(in_state$n_orgs) else 0
  emit <- function(fips, sfp, canon, res, share, cands)
    tibble(geo_county_fips = fips, state_fips = sfp, geo_county_canonical = canon,
           resolution = res, n_orgs = sum(g$n_orgs), n_orgs_in_state = total,
           top_share = share, candidates = cands)

  mass <- if (total > 0) sort(tapply(in_state$n_orgs, in_state$poly_geoid, sum),
                              decreasing = TRUE) else numeric(0)
  share <- if (length(mass)) mass / sum(mass) else numeric(0)
  top   <- if (length(mass)) names(mass)[1] else NA_character_
  cands <- if (length(mass))
    paste(sprintf("%s(%.0f%%)", names(mass), 100 * share), collapse = "; ") else NA_character_

  # 0. Connecticut old-county labels -> defer to the planning-region companion.
  #    Only an explicit "<name> County" is deferred; a bare town label (e.g.
  #    "Hartford") lacks the county token and still resolves normally below.
  if (state == "CT" && grepl("county$", tolower(trimws(raw))) && rawnm %in% CT_OLD_COUNTIES)
    return(emit(NA_character_, "09", NA_character_, "deferred_ct_planning_region",
                if (length(share)) share[[1]] else NA_real_, cands))

  name_hits <- name_lookup[[paste(state, rawnm)]]
  is_bare   <- !grepl("(county|parish|borough|municipio|municipality|census area|district)$|^city of | city$",
                      tolower(trimws(raw)))
  has_twin  <- is_bare && (paste(state, rawnm) %in% city_twins)

  # 1. Genuine ambiguity: a same-name collision, or a bare label that could mean
  #    either an independent city or its namesake county. Cannot be canonicalized.
  if ((length(name_hits) >= 2L) || has_twin)
    return(emit(NA_character_, if (!is.na(top)) substr(top, 1, 2) else NA_character_,
                NA_character_, "ambiguous", if (length(share)) share[[1]] else NA_real_, cands))

  # 2. Unique name match -> authoritative for canonicalization. (The per-point
  #    polygon hit is noisier than the curated label for tiny groups, so the
  #    name wins; the mass share is recorded for transparency.)
  if (length(name_hits) == 1L) {
    g_id <- name_hits[[1]]
    meta <- geoid_meta[[g_id]]
    return(emit(g_id, meta$statefp, meta$namelsad, "resolved",
                if (length(share)) share[[1]] else NA_real_, cands))
  }

  # 3. No name match -> mass fallback
  if (total == 0)
    return(emit(NA_character_, NA_character_, NA_character_, "unresolved", NA_real_, cands))
  if (share[[1]] >= DOMINANT_THRESHOLD) {
    meta <- geoid_meta[[top]]
    return(emit(top, meta$statefp, meta$namelsad, "resolved", share[[1]], cands))
  }
  emit(NA_character_, substr(top, 1, 2), NA_character_, "ambiguous", share[[1]], cands)
}

xwalk <- resolved_pts %>%
  group_by(geo_state_abbr, geo_county) %>%
  group_modify(~ classify_group(.x, .y)) %>%
  ungroup() %>%
  rename(geo_county_raw = geo_county) %>%
  mutate(tiger_year = TIGER_YEAR) %>%
  arrange(geo_state_abbr, geo_county_raw)

tally <- table(xwalk$resolution)
n_res <- function(k) { v <- tally[k]; if (is.na(v)) 0L else as.integer(v) }
message(sprintf("      groups: %d resolved | %d ambiguous | %d unresolved",
                n_res("resolved"), n_res("ambiguous"), n_res("unresolved")))

message("[5/5] Writing outputs")
clean <- xwalk %>%
  select(geo_state_abbr, geo_county_raw, geo_county_fips, state_fips,
         geo_county_canonical, resolution, tiger_year)
write_parquet(clean, out_path)
message(sprintf("      -> %s (%d rows)", out_path, nrow(clean)))
data.table::fwrite(clean, out_csv)   # CSV mirror for the web catalog / spreadsheet users
message(sprintf("      -> %s", out_csv))

audit <- xwalk %>%
  filter(resolution != "resolved") %>%
  select(geo_state_abbr, geo_county_raw, resolution, top_share,
         n_orgs, n_orgs_in_state, candidates, state_fips, tiger_year)
data.table::fwrite(audit, audit_path)
message(sprintf("      -> %s (%d rows)", audit_path, nrow(audit)))

message("Done.")
