# ============================================================================
# build_cbsa_crosswalk.R
#
# Build a county -> CBSA (Core-Based Statistical Area) crosswalk DERIVED from
# the county FIPS crosswalk. CBSAs are OMB-defined unions of whole counties,
# so this maps each county GEOID in the county crosswalk to its metropolitan /
# micropolitan statistical area. Consumers chain the two:
#
#   raw geocoder label --(county_fips_crosswalk)--> county_fips
#                      --(this crosswalk)----------> CBSA
#
# Authoritative source: the OMB July-2023 delineation (Census "List 1"),
# which uses the SAME geography vintage as TIGER 2023 (incl. the Connecticut
# planning regions), so every county GEOID joins cleanly.
#
# Inputs
#   data/crosswalks/county_fips_crosswalk.parquet   (the county universe + names)
#   List 1 delineation xlsx (downloaded from census.gov; URL below)
#
# Outputs
#   data/crosswalks/cbsa_crosswalk.parquet  (one row per county_fips in the
#     county crosswalk; CBSA columns NA for rural counties not in any CBSA)
#     county_fips        chr  5-char county GEOID (join key -> county xwalk geo_county_fips)
#     county_name        chr  Census NAMELSAD, from the county crosswalk
#     cbsa_code          chr  5-char CBSA code (NA if rural)
#     cbsa_title         chr  e.g. "Hartford-West Hartford-East Hartford, CT"
#     cbsa_type          chr  "Metropolitan Statistical Area" | "Micropolitan Statistical Area"
#     central_outlying   chr  "Central" | "Outlying"
#     csa_code           chr  3-char Combined Statistical Area code (NA if none)
#     csa_title          chr  CSA title (NA if none)
#     delineation_year   int  OMB delineation vintage (2023)
#   data/crosswalks/cbsa_crosswalk.csv      (web / spreadsheet mirror)
#
# Run:  Rscript scripts/build_cbsa_crosswalk.R
# ============================================================================

suppressMessages({
  library(dplyr)
  library(arrow)
  library(openxlsx)
})

DELINEATION_YEAR <- as.integer(Sys.getenv("DELINEATION_YEAR", "2023"))
DELINEATION_URL  <- sprintf(
  "https://www2.census.gov/programs-surveys/metro-micro/geographies/reference-files/%d/delineation-files/list1_%d.xlsx",
  DELINEATION_YEAR, DELINEATION_YEAR)

here_root <- tryCatch(here::here(), error = function(e) getwd())
county_path <- file.path(here_root, "data", "crosswalks", "county_fips_crosswalk.parquet")
out_path    <- file.path(here_root, "data", "crosswalks", "cbsa_crosswalk.parquet")
out_csv     <- file.path(here_root, "data", "crosswalks", "cbsa_crosswalk.csv")
audit_path  <- file.path(here_root, "data", "crosswalks", "cbsa_crosswalk_audit.csv")

stopifnot(file.exists(county_path))

message("[1/4] County universe from the county FIPS crosswalk + CT companion")
cty <- read_parquet(county_path)
universe <- cty %>%
  filter(resolution == "resolved", !is.na(geo_county_fips)) %>%
  distinct(county_fips = geo_county_fips, county_name = geo_county_canonical)

# CT planning regions are deferred (coordinate-keyed) in the county crosswalk,
# so fold the companion's distinct GEOIDs in here -- otherwise the chain
# raw -> CT companion -> region -> CBSA would dead-end and the planning regions
# would (wrongly) show as "absent_from_bmf".
ct_path <- file.path(here_root, "data", "crosswalks", "ct_planning_region_crosswalk.parquet")
if (file.exists(ct_path)) {
  ct_universe <- read_parquet(ct_path) %>%
    distinct(county_fips = geo_county_fips, county_name = geo_county_canonical)
  universe <- universe %>%
    bind_rows(ct_universe) %>%
    distinct(county_fips, .keep_all = TRUE)
  message(sprintf("      +%d CT planning-region GEOIDs from the companion", nrow(ct_universe)))
} else {
  message("      (CT companion not found; planning regions will be absent)")
}
universe <- arrange(universe, county_fips)
message(sprintf("      %d distinct resolved county GEOIDs", nrow(universe)))

message(sprintf("[2/4] Downloading OMB %d delineation (List 1)", DELINEATION_YEAR))
tmp <- tempfile(fileext = ".xlsx")
utils::download.file(DELINEATION_URL, tmp, mode = "wb", quiet = TRUE)
raw <- read.xlsx(tmp, sheet = 1, startRow = 3, colNames = TRUE)
names(raw) <- c("cbsa_code", "metdiv_code", "csa_code", "cbsa_title", "cbsa_type",
                "metdiv_title", "csa_title", "county_name_omb", "state_name",
                "fips_state", "fips_county", "central_outlying")
raw <- raw[!is.na(raw$cbsa_code) & !is.na(raw$fips_county), ]   # drop footer notes

pad <- function(x, w) formatC(as.integer(x), width = w, flag = "0")
deline <- raw %>%
  transmute(
    county_fips      = paste0(pad(fips_state, 2), pad(fips_county, 3)),
    cbsa_code        = pad(cbsa_code, 5),
    cbsa_title, cbsa_type, central_outlying,
    csa_code         = ifelse(is.na(csa_code), NA_character_, pad(csa_code, 3)),
    csa_title)
message(sprintf("      %d county memberships across %d CBSAs",
                nrow(deline), dplyr::n_distinct(deline$cbsa_code)))
stopifnot(!anyDuplicated(deline$county_fips))   # a county belongs to <=1 CBSA

message("[3/4] Joining CBSA membership onto the county universe")
xwalk <- universe %>%
  left_join(deline, by = "county_fips") %>%
  mutate(delineation_year = DELINEATION_YEAR) %>%
  arrange(county_fips)

n_cbsa  <- sum(!is.na(xwalk$cbsa_code))
n_rural <- sum(is.na(xwalk$cbsa_code))
n_metro <- sum(xwalk$cbsa_type == "Metropolitan Statistical Area", na.rm = TRUE)
n_micro <- sum(xwalk$cbsa_type == "Micropolitan Statistical Area", na.rm = TRUE)
message(sprintf("      %d in a CBSA (%d metro, %d micro) across %d distinct CBSAs | %d rural (no CBSA)",
                n_cbsa, n_metro, n_micro,
                dplyr::n_distinct(xwalk$cbsa_code[!is.na(xwalk$cbsa_code)]), n_rural))

# Invariants
stopifnot(
  all(nchar(xwalk$county_fips) == 5),
  all(is.na(xwalk$cbsa_code) | nchar(xwalk$cbsa_code) == 5),
  !anyDuplicated(xwalk$county_fips))

message("[4/4] Writing outputs")
write_parquet(xwalk, out_path)
data.table::fwrite(xwalk, out_csv)
message(sprintf("      -> %s (%d rows)", out_path, nrow(xwalk)))
message(sprintf("      -> %s", out_csv))

# Audit: delineation counties that never appear in the BMF county universe
# (counties with no observed nonprofits at this vintage) — informational, not
# an error. Plus the rural-county count.
missing <- deline %>% filter(!county_fips %in% universe$county_fips)
audit <- bind_rows(
  missing %>% transmute(issue = "delineation_county_absent_from_bmf",
                        county_fips, cbsa_title),
  tibble(issue = "rural_counties_no_cbsa",
         county_fips = sprintf("(%d counties)", n_rural), cbsa_title = NA_character_))
data.table::fwrite(audit, audit_path)
message(sprintf("      -> %s (%d delineation counties absent from BMF + rural tally)",
                audit_path, nrow(missing)))
message("Done.")
