# Details:
# (1) -Load in BMF Data
# (1a) - Figure out what makes an address hard to parse and some type of flag
# (2) - Create a sample of 100 addresses that are unique to parse

# Load Packages
library(tidyverse)
library(readr)
library(writexl)
library(dplyr)
library(stringr)

# Load Data
unified_bmf <- readr::read_csv("C:/Users/cprinvil/Downloads/BMF_UNIFIED_V1.1 (1).csv")
unified_addr <- unified_bmf[, grepl("ADDR_", names(unified_bmf))] # ADDR Columns only

# add flags for difficult to parse addresses
unified_addr <- unified_addr |>
  dplyr::mutate(
    has_special_chars = str_detect(ORG_ADDR_FULL, "[#@&%*]"),
    has_po_box = str_detect(ORG_ADDR_FULL, regex("P\\.?O\\.? Box",
      ignore_case = TRUE
    )),
    has_rural_route = str_detect(ORG_ADDR_FULL, regex("RR|Rural Route",
      ignore_case = TRUE
    )),
    missing_number = !str_detect(ORG_ADDR_FULL, "\\d"),
  )

# now apply filter
difficult_addy <- unified_addr |>
  dplyr::filter(
    has_special_chars |
      has_po_box |
      has_rural_route |
      missing_number
  )

# Sample 100 addresses
set.seed(123) # reproducibility
sample_100 <- difficult_addy |>
  sample_n(100)

# Export the sample to a csv

readr::write_csv(sample_100, "C:/Users/cprinvil/Downloads/sample_addresses.csv")
