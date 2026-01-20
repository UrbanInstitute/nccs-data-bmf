# Script Header #

# Add state and county fips to the BMF for filtering with API
# Update with the most recent financial data from the most recent BMF
# Update the BMF with the latest BMF

# Packages
library(tigris)
library(usdata)

# Load BMF data
unified_bmf <- data.table::fread(
  "~/Urban/NCCS/remote_data/bmf/BMF_UNIFIED_V1.1.csv"
)

# Update BMF state data
unified_bmf_1.2 <- unified_bmf |>
  dplyr::mutate(CENSUS_STATE_ABBR = ifelse(CENSUS_STATE_ABBR == "", 
                                           "UK", 
                                           CENSUS_STATE_ABBR)) |>
  dplyr::mutate(CENSUS_STATE_NAME = dplyr::case_when(
    CENSUS_STATE_ABBR == "UK" ~ "Unknown",
    CENSUS_STATE_ABBR %in% usdata::state_stats$abbr ~ usdata::abbr2state(CENSUS_STATE_ABBR),
    CENSUS_STATE_ABBR == "PR" ~ "Puerto Rico",
    CENSUS_STATE_ABBR == "VI" ~ "Virgin Islands",
    CENSUS_STATE_ABBR == "MP" ~ "Northern Mariana Islands",
    CENSUS_STATE_ABBR == "AS" ~ "American Samoa",
    CENSUS_STATE_ABBR == "GU" ~ "Guam"
  ))


# Get state and county fips and merge
census_states <- tigris::states()
census_county <- tigris::counties()
  
state_merge <- census_states |>
  dplyr::select(
    NAME,
    GEOID
  ) |>
  dplyr::mutate(
    CENSUS_STATE_ABBR = usdata::state2abbr(NAME)
  ) |>
  dplyr::mutate(
    CENSUS_STATE_ABBR = dplyr::case_when(
      NAME == "Commonwealth of the Northern Mariana Islands" ~ "MP",
      NAME == "American Samoa" ~ "AS",
      NAME == "Guam" ~ "GU",
      NAME == "Puerto Rico" ~ "PR",
      NAME == "United States Virgin Islands" ~ "VI",
      .default = CENSUS_STATE_ABBR
    )
  ) |>
  dplyr::rename(CENSUS_STATE_FIPS = GEOID) |>
  dplyr::select(! NAME)

# ADD UNKNOWN FIPS AS 00

# rename county fips to same column names as bmf and select relevant columns for merge

