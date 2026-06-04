# ============================================================================
# read_county_points.R
#
# One-time (single) S3 read feeding scripts/build_county_fips_crosswalk.R.
# Pulls distinct (geo_state_abbr, geo_county) plus a ~1km-gridded spread of
# representative geocoded points, and caches them locally so the geometry
# build never re-hits the bucket.
#
# Reads:  s3://nccsdata/geocoding/bmf-master/merged/bmf_master_geocoded.parquet
# Writes: data/crosswalks/_county_points_cache.parquet
#
# Run (creds must be exported, e.g. aws sso login --profile thiya):
#   eval "$(aws configure export-credentials --profile thiya --format env)"
#   Rscript scripts/read_county_points.R
# ============================================================================
suppressMessages({library(arrow); library(dplyr)})

src <- "s3://nccsdata/geocoding/bmf-master/merged/bmf_master_geocoded.parquet?region=us-east-1"
ds  <- open_dataset(src)

pts <- ds %>%
  select(geo_state_abbr, geo_county, geo_lat, geo_lon) %>%
  filter(!is.na(geo_state_abbr), !is.na(geo_county),
         !is.na(geo_lat), !is.na(geo_lon),
         geo_county != "", geo_state_abbr != "") %>%
  mutate(lat2 = round(geo_lat, 2), lon2 = round(geo_lon, 2)) %>%
  count(geo_state_abbr, geo_county, lat2, lon2, name = "n_orgs") %>%
  collect()

cat(sprintf("distinct gridded points: %d\n", nrow(pts)))
cat(sprintf("distinct (state, raw county) groups: %d\n",
            nrow(distinct(pts, geo_state_abbr, geo_county))))

out <- here::here("data", "crosswalks", "_county_points_cache.parquet")
write_parquet(pts, out)
cat(sprintf("cached -> %s\n", out))
