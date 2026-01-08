# Parameters

bmf_raw_url <- "https://us-east-1.console.aws.amazon.com/s3/object/nccsdata?region=us-east-1&prefix=raw/bmf/2026-01-BMF.csv"

# Paths
subsection_classification_code_lookup_path <- "data/lookup/classification_subsection_code_lookup.csv"
affiliation_code_lookup_path <- "data/lookup/affiliation_code_lookup.csv"
deductibility_code_lookup_path <- "data/lookup/deductibility_code_lookup.csv"
foundation_code_lookup_path <- "data/lookup/foundation_code_lookup.csv"
activity_code_lookup_path <- "data/lookup/activity_code_lookup.csv"
organization_code_lookup_path <- "data/lookup/organization_code_lookup.csv"

# Lookup
lookup_path <- "data/lookup/bmf_code_lookup.xlsx"
lookup_ls <- openxlsx::getSheetNames(lookup_path) |>
  purrr::set_names() |>
  purrr::map(~ {
    df <- openxlsx::read.xlsx(xlsxFile = lookup_path, sheet = .x)
    data.table::setDT(df)
  })
