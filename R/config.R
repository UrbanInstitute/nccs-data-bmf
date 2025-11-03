# Parameters

bmf_2025_url_ls <- list(
  northeast_region = "https://www.irs.gov/pub/irs-soi/eo1.csv",
  mid.atlantic_region = "https://www.irs.gov/pub/irs-soi/eo2.csv",
  gulf.and.pacific.coast_region = "https://www.irs.gov/pub/irs-soi/eo3.csv",
  international.pr_region = "https://www.irs.gov/pub/irs-soi/eo4.csv"
)

bmf_2025_raw_paths <- c(
  "data/raw/bmf_2025_northeast_region.csv",
  "data/raw/bmf_2025_mid.atlantic_region.csv",
  "data/raw/bmf_2025_gulf.and.pacific.coast_region.csv",
  "data/raw/bmf_2025_international.pr_region.csv"
)

# Paths
subsection_classification_code_lookup_path <- "data/lookup/classification_subsection_code_lookup.csv"
affiliation_code_lookup_path <- "data/lookup/affiliation_code_lookup.csv"
deductibility_code_lookup_path <- "data/lookup/deductibility_code_lookup.csv"
foundation_code_lookup_path <- "data/lookup/foundation_code_lookup.csv"