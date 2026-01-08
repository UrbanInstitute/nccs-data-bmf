# Library
library(data.table)

# Helper scripts
source(here::here("R", "config.R"))
source(here::here("R", "ein.R"))
source(here::here("R", "transform_ntee_code.R"))
source(here::here("R", "affiliation_code.R"))
source(here::here("R", "input_validation.R"))

bmf_2025_raw <- data.table::fread("data/raw/bmf_2025.csv")

bmf_2025_raw[, ein := clean_and_format_ein(EIN)]

bmf_2025_preprocessed <- transform_affiliation_code(bmf_2025_raw, 
                                                    lookup = affiliation_code_lookup)