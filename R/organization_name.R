# Formatting organization name

# Parameters
suffix_map <- list(
  # Common Corporation variations
  "INC" = c("INC", "INC-", "INC\\.", "INCORPORATED", " I N C", "IN C", "I NC", " INC ", " INC - "),
  "CORP" = c("CORP", "CORP\\.", "CORPORATION", "COR P", "CO RP", "A NONPROFIT CORPORATION"),
  "CO" = c("CO", "CO\\.", "COMPANY"),
  # Limited Liability Company variations
  "LLC" = c("LLC", "LLC\\.", "L L C", "L\\.L\\.C.", "LIMITED LIABILITY CO"),
  "LTD" = c("LTD", "LTD\\.", "LIMITED"),
  # Professional Corporation variations
  "PC" = c("PC", "PC\\.", "P C"),
  # Trust variations
  "TR" = c("TR", "TR\\.", "TRUST", " TR "),
  "TUA" = c("TUA", "TUA\\.", "T\\.U\\.A.", "TRUST UNDER AGREEMENT"),
  "TUW" = c("TUW", "TUW\\.", "T\\.U\\.W.", "TRUST UNDER WILL", "TR UW"),
  # Org Variations
  "ORG" = c("DOT ORG", "\\sORG$"),
  "NFP" = c("NFP")
) |>
  stack() |>
  dplyr::rename(variation = values, standard_suffix = ind) |>
  dplyr::mutate(length = nchar(variation), 
                standard_suffix = as.character(standard_suffix)) |>
  dplyr::arrange(dplyr::desc(length))

name_lookup <- readxl::read_xlsx("data/validate/lookup/standardized_organization_names.xlsx")  
  
standardization_lookup <- tibble::tribble(
  ~variation, ~standardized_word,
  "Assoc\\s*$", "Association",
  "Assn", "Association",
  "Natl", "National",
  "Pta", "PTA",
  "Ptso", "PTSO",
  "Pto", "PTO",
  "Ymca", "YMCA",
  "Dmv", "DMV",
  "FBO", "",
  "\\s.*Pba\\s*", "PBA",
  "\\sPs\\s", " PS ",
  "Jgb\\s", "JGB ",
  "Camg-F", "CAMG",
  "Hcsa\\s", "HCSA ",
  "Mcmillan", "McMillan",
  "@", "\\s",
  "%", "\\s",
  "\\sNea$", " NEA",
  "\\sUsa\\s", " USA ",
  "\\sNj\\s", " NJ ",
  "\\sDc\\s*", " DC ",
)

#' @title Function to clean organization names in the BMF `NAME` column
clean_names <- function(raw_names,
                        suffix_map,
                        standardization_lookup,
                        name_lookup) {
  results <- data.table::data.table(
    org_name_raw = raw_names,
    org_name_join = NA_character_,
    org_name_display = NA_character_,
    org_legal_suffix = NA_character_
  )
  
  # First data cleaning process - extract suffixes and create new name variables
  for (i in 1:nrow(suffix_map)) {
    variation <- suffix_map$variation[i]
    standard_suffix <- suffix_map$standard_suffix[i]
    pattern <- pattern <- paste0("\\s*", variation, "\\s*$")
    
    results[is.na(org_legal_suffix) &
              stringr::str_detect(org_name_raw, pattern), `:=`(
                org_legal_suffix = standard_suffix,
                org_name_join = stringr::str_replace(org_name_raw, pattern, " "),
                org_name_display = stringr::str_to_title(stringr::str_replace(org_name_raw, pattern, " "))
              )]
  }
  
  processed_name_cols <- c("org_name_join", "org_name_display", "org_legal_suffix")
  results[, (processed_name_cols) := lapply(.SD, stringr::str_squish), .SDcols = processed_name_cols]
  
  results[, org_name_join := data.table::fifelse(is.na(org_name_join), 
                                                 org_name_raw, 
                                                 org_name_join)]
  
  results[, org_name_display := data.table::fifelse(is.na(org_name_display),
                                                    stringr::str_to_title(org_name_join),
                                                    org_name_display)]
  
  # 2nd data cleaning process - standardizing patterns
  for (i in 1:nrow(standardization_lookup)) {
    variation <- standardization_lookup[["variation"]][i]
    standardized_word <- standardization_lookup[["standardized_word"]][i]
    
    results[stringr::str_detect(org_name_display, variation), `:=` (
      org_name_display = stringr::str_replace(org_name_display, variation, standardized_word)
    )]
    
  }
  
  # 3rd data cleaning process - unique name recodings
  for (i in 1:nrow(name_lookup)) {
    results[org_name_raw == name_lookup$org_name_raw[i], `:=` (
      org_name_join = name_lookup$org_name_join[i],
      org_name_display = name_lookup$org_name_display[i],
      org_legal_suffix = name_lookup$org_legal_suffix[i]
    ) ]
  }
  
  results[, (processed_name_cols) := lapply(.SD, stringr::str_squish), .SDcols = processed_name_cols]
  
  return(results)
}

# Code for testing and validation

# Code for testing and validation

# 666 records are in the 99% CI for a SRS, add names in the name lookup to validate
# test_names <- c(
#   sample(bmf_2025_raw$NAME, 666),
#   bmf_2025_raw[NAME %in% name_lookup$org_name_raw, NAME]
# ) 
# rs <- clean_names(test_names, suffix_map, standardization_lookup, name_lookup)
# data.table::fwrite(rs, "data/validate/manual/org_names.csv")