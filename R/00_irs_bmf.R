# Script to process raw bmf from the IRS

# Helper scripts
source(here::here("R", "config.R"))
source(here::here("R", "ein.R"))


# Extraction Layer

# IRS site: https://www.irs.gov/charities-non-profits/exempt-organizations-business-master-file-extract-eo-bmf

## Download 4 regional BMFs
regional_bmfs <- purrr::imap(bmf_2025_url_ls, \(x, idx) download.file(x, destfile = paste0("data/raw/bmf_2025_", idx, ".csv")))

## Read in all 4 bmfs and combine them
regional_bmfs <- purrr::map(bmf_2025_raw_paths, 
                            data.table::fread,
                            .progress = TRUE)

bmf_2025_raw <- purrr::list_rbind(regional_bmfs)

## Check Record count: 1,898,175 from IRS Site
nrow(bmf_2025_raw) == 1898175

## Write out BMF
data.table::fwrite(bmf_2025_raw, "data/raw/bmf_2025.csv")

# Transformation Layer

bmf_2025_preprocessed <- bmf_2025_raw |>
  dplyr::mutate(ein = clean_and_format_ein(EIN))

## EIN2?

# TODO
# Merge raw bmf path vector and raw bmf urls together
# complement names with efile names, since NAME is incorrect.