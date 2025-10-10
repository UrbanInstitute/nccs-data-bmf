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

# EIN
bmf_2025_preprocessed <- bmf_2025_raw |>
  dplyr::mutate(ein = clean_and_format_ein(EIN))

## NAME
raw_names <- bmf_2025_raw$NAME

bmf_2025_raw[, c("org_name_raw",
                 "org_name_join",
                 "org_name_display",
                 "org_legal_suffix") := as.list(clean_names(raw_names, 
                                                            suffix_map, 
                                                            standardization_lookup, 
                                                            name_lookup))]


## ICO Name
bmf_2025_raw <- transform_ico_name(bmf_2025_raw)

## GEN

bmf_2025_raw <- transform_group_exemption_number(bmf_2025_raw)

## EO SUBSECTION AND CLASSIFICATION CODES

# TODO
# create copies, don't overwrite, make functions pure, preserve data lineage it must be clear that we are making changes outside the function
# Merge raw bmf path vector and raw bmf urls together
# complement names with efile names, since NAME is incorrect.
# Metadata tables:
# organization name, ico name