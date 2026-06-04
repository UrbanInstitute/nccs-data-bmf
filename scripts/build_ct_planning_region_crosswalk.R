# ============================================================================
# build_ct_planning_region_crosswalk.R
#
# Companion to the county FIPS crosswalk, resolving Connecticut to its 2022
# OMB/Census *planning regions* (county-equivalent GEOIDs 091xx) by
# COORDINATE rather than by county name.
#
# Why a separate, coordinate-keyed artifact:
#   In 2022 Census retired CT's 8 historical counties and replaced them with
#   9 planning regions. The geocoder still emits the old county labels
#   ("Fairfield County", ...). A single old county genuinely SPLITS across
#   several planning regions, so there is no correct FIPS for it at the
#   (state, county) grain -- the county crosswalk rightly defers every CT
#   "<name> County" label here (resolution = "deferred_ct_planning_region").
#   Each individual org point, however, falls in exactly one planning region.
#   This script bakes that point-in-polygon answer into a lookup grid so a
#   consumer can recover the region from the geocoded coordinates it already
#   carries -- without doing any GIS itself, and without us putting a FIPS in
#   the master (ADR 0016).
#
# Self-contained from TIGER 2023 (no BMF point cache, no S3 read), exactly as
# the CBSA crosswalk derives purely from the OMB delineation. sf/tigris stay
# isolated to scripts/, never the pipeline runtime.
#
# Output: a dense 0.01-degree grid over CT land. To use it, round the
# geocoded org's (geo_lat, geo_lon) to 2 decimals and join on (lat2, lon2),
# valid where geo_state_abbr == "CT".
#
#   data/crosswalks/ct_planning_region_crosswalk.parquet
#     geo_state_abbr        chr  always "CT" (scopes the grid)
#     lat2                  dbl  geo_lat rounded to 0.01 deg  (join key)
#     lon2                  dbl  geo_lon rounded to 0.01 deg  (join key)
#     geo_county_fips       chr  5-char planning-region GEOID (091xx)
#     state_fips            chr  "09"
#     geo_county_canonical  chr  Census NAMELSAD ("<name> Planning Region")
#     area_share            dbl  share of the cell's sub-samples in that region
#     straddle              lgl  TRUE when area_share < STRADDLE_THRESHOLD
#                                (cell sits across a planning-region boundary)
#     tiger_year            int  TIGER/Census vintage of the boundaries
#   data/crosswalks/ct_planning_region_crosswalk_audit.csv  (straddle cells)
#
# Run:  Rscript scripts/build_ct_planning_region_crosswalk.R
#       TIGER_YEAR=2023 Rscript scripts/build_ct_planning_region_crosswalk.R
# ============================================================================

suppressMessages({
  library(sf)
  library(tigris)
  library(dplyr)
  library(arrow)
  library(data.table)
})
options(tigris_use_cache = TRUE)

TIGER_YEAR         <- as.integer(Sys.getenv("TIGER_YEAR", "2023"))
STEP               <- 0.01        # grid resolution = round(coord, 2)
SUBN               <- 5L          # SUBN x SUBN sub-samples per cell
STRADDLE_THRESHOLD <- 0.95        # flag a cell when its top region < this share
CT_STATEFP         <- "09"

here_root  <- tryCatch(here::here(), error = function(e) getwd())
out_path   <- file.path(here_root, "data", "crosswalks", "ct_planning_region_crosswalk.parquet")
out_csv    <- file.path(here_root, "data", "crosswalks", "ct_planning_region_crosswalk.csv")
audit_path <- file.path(here_root, "data", "crosswalks", "ct_planning_region_crosswalk_audit.csv")

# --- 1. CT planning-region polygons (county-equivalents in TIGER 2023) -------
# Fetch the national cb county file and filter to STATEFP 09 (reuses the tigris
# cache the county-crosswalk build already populated; avoids relying on cb
# state-filter support).
message(sprintf("[1/5] TIGER counties (cb=TRUE, year=%d) -> filter CT", TIGER_YEAR))
co <- counties(cb = TRUE, year = TIGER_YEAR, progress_bar = FALSE)
pr <- co[co$STATEFP == CT_STATEFP, c("GEOID", "STATEFP", "NAMELSAD")]
pr <- st_transform(pr, 4326)  # cb files are NAD83 (4269); WGS84 diff < 1m
message(sprintf("      %d CT planning regions: %s", nrow(pr),
                paste(sort(pr$GEOID), collapse = ", ")))
stopifnot(nrow(pr) > 0)

# --- 2. Candidate 0.01-deg grid over the CT bounding box ---------------------
# Keys are exact 0.01 multiples so a consumer's round(coord, 2) lands on a cell.
bb      <- st_bbox(pr)
lon_seq <- round(seq(floor(bb["xmin"] * 100) / 100,
                     ceiling(bb["xmax"] * 100) / 100, by = STEP), 2)
lat_seq <- round(seq(floor(bb["ymin"] * 100) / 100,
                     ceiling(bb["ymax"] * 100) / 100, by = STEP), 2)
grid <- CJ(lon2 = lon_seq, lat2 = lat_seq)
grid[, cell_id := .I]
message(sprintf("      %d candidate cells in CT bbox (%.2f..%.2f lon, %.2f..%.2f lat)",
                nrow(grid), min(lon_seq), max(lon_seq), min(lat_seq), max(lat_seq)))

# --- 3. Sub-sample each cell and assign region by point-in-polygon -----------
# A round(x, 2) cell spans [v - 0.005, v + 0.005]. Sample a SUBN x SUBN lattice
# inside each cell; the region holding the most sub-points wins, its fraction of
# in-region sub-points is the area_share, and a cell whose share is below the
# threshold straddles a boundary. st_within is spatial-indexed, so even
# ~500k sub-points resolve quickly.
message(sprintf("[2/5] Point-in-polygon over %d-pt sub-lattice per cell", SUBN * SUBN))
off  <- ((seq_len(SUBN) - 0.5) / SUBN - 0.5) * STEP   # symmetric offsets within a cell
nsub <- SUBN * SUBN
suboff <- CJ(dlon = off, dlat = off)

pts <- grid[rep(seq_len(.N), each = nsub)]
pts[, `:=`(plon = lon2 + rep(suboff$dlon, times = nrow(grid)),
           plat = lat2 + rep(suboff$dlat, times = nrow(grid)))]

sf_pts <- st_as_sf(pts[, .(plon, plat)], coords = c("plon", "plat"), crs = 4326)
idx    <- st_within(sf_pts, pr)
pts[, geoid := vapply(idx, function(x) if (length(x)) pr$GEOID[x[[1]]] else NA_character_,
                      character(1))]

# --- 4. Reduce to one winning region per cell --------------------------------
message("[3/5] Reducing to one planning region per cell")
hits <- pts[!is.na(geoid)]
per  <- hits[, .N, by = .(cell_id, lat2, lon2, geoid)]
per[, share := N / sum(N), by = cell_id]
win  <- per[order(cell_id, -N), .SD[1L], by = cell_id]   # top region per cell

meta <- as.data.table(st_drop_geometry(pr))[
  , .(geoid = GEOID, geo_county_canonical = NAMELSAD, state_fips = STATEFP)]
win  <- merge(win, meta, by = "geoid", all.x = TRUE)

out <- win[, .(
  geo_state_abbr       = "CT",
  lat2, lon2,
  geo_county_fips      = geoid,
  state_fips,
  geo_county_canonical,
  area_share           = round(share, 4),
  straddle             = share < STRADDLE_THRESHOLD,
  tiger_year           = TIGER_YEAR
)][order(lat2, lon2)]

n_straddle <- sum(out$straddle)
message(sprintf("      %d CT-land cells | %d planning regions covered | %d straddle cells (%.1f%%)",
                nrow(out), length(unique(out$geo_county_fips)),
                n_straddle, 100 * n_straddle / nrow(out)))

# --- 5. Write outputs --------------------------------------------------------
message("[4/5] Writing grid")
write_parquet(out, out_path)
message(sprintf("      -> %s (%d rows)", out_path, nrow(out)))
fwrite(out, out_csv)
message(sprintf("      -> %s", out_csv))

message("[5/5] Writing straddle audit")
audit <- out[straddle == TRUE][order(-area_share == FALSE, area_share)]
fwrite(audit, audit_path)
message(sprintf("      -> %s (%d rows)", audit_path, nrow(audit)))

# Per-region cell tally for a quick sanity read.
tally <- out[, .(cells = .N,
                 straddle = sum(straddle)), by = .(geo_county_fips, geo_county_canonical)][order(geo_county_fips)]
message("Per-region cell counts:")
print(tally)
message("Done.")
