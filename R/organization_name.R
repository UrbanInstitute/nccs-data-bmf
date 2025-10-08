# Formatting organization name

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

clean_names <- function(names){
  # Remove suffixes
  # Remove punctuation and special characters - display name (sentence case)
  # Extract suffixes to a separate column
  # Unique lookup table
}

# simple random sample at 99% confidence interval
test_names <- sample(bmf_2025_raw$NAME, 666)

results <- data.frame(
  org_name_raw = test_names,
  org_name_join= NA_character_, 
  org_name_display = NA_character_,
  org_legal_suffix = NA_character_,   
  stringsAsFactors = FALSE
)

for (i in 1:nrow(suffix_map)){
  variation <- suffix_map$variation[i]
  standard_suffix <- suffix_map$standard_suffix[i]
  pattern <- pattern <- paste0("\\s*", variation, "\\s*$")
  
  uncleaned_indices <- which(is.na(results$org_legal_suffix))
  names_to_check <- results$org_name_raw[uncleaned_indices]
  
  matches_mask <- stringr::str_detect(names_to_check, pattern)
  global_match_indices <- uncleaned_indices[matches_mask]
  
  results$org_legal_suffix[global_match_indices] <- standard_suffix
  
  results$org_name_join[global_match_indices] <- 
    stringr::str_replace(results$org_name_raw[global_match_indices],
                         pattern,
                         " ")
  results$org_name_display[global_match_indices] <- 
    stringr::str_to_title(results$org_name_join[global_match_indices])
}
results$org_name_join <- stringr::str_squish(results$org_name_join)

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
  "Educationfoundation", "Education Foundation",
  "Youthlacrosse", "Youth Lacrosse",
  "\\sPs\\s", " PS ",
  "Jgb\\s", "JGB ",
  "Camg-F", "CAMG",
  "Hcsa\\s", "HCSA ",
  "Mcmillan", "McMillan",
  "@", "\\s",
  "%", "\\s",
  "\\sNea$", " NEA",
  "\\sUsa\\s", " USA ",
  "Fwc Scholarship Foundation", "FWC Scholarship Foundation",
  "Ppep First American Resources & Services", "PPEP First American Resources & Services",
  "Aarp", "AARP",
  "Tsg Foundation", "TSG Foundation",
  "\\sNj\\s", " NJ ",
  "\\sDc\\s*", " DC ",
  "Yc Travel Msc Mariners Benefit Foundation", "YC Travel MSC Mariners Benefit Foundation",
  "Equallyoked", "Equally Yoked",
  "Templo Oasis Ad Levittown", "Templo Oasis Asambleas de Dios Levittown",
  "Friends Of Iglfa", "Friends Of IGLFA",
  "Society For Preservation & Encrgmnt Of Barbershop Quartet Singing Amer",
  "Society for the Preservation and Encouragement of Barber Shop Quartet Singing in America",
  "Fdru", "FDRU",
  "Moremarrowdonorsorg", "More Marrow Donors Org", # need to add ORG suffix
  "United Residents In Academy Homes Ii", "United Residents In Academy Homes II ", "Ddd Foundation", "DDD Foundation",
  "Cc Ball Family Foundation", "The Ball Family Foundation",
  "Monroe Mi Home Non-Profit Housing", "HOME",
  "Monument Bpw", "BPW",
  "Hsapss Pa", "HSAPSS PA"
)


results <- results |>
  dplyr::mutate(
    org_name_join = ifelse(is.na(org_name_join), org_name_raw, org_name_join),
    org_name_display = ifelse(
      is.na(org_name_display),
      stringr::str_to_title(org_name_join),
      org_name_display
    )
  )

for (i in 1:nrow(standardization_lookup)){
  variation <- standardization_lookup[["variation"]][i]
  standardized_word <- standardization_lookup[["standardized_word"]][i]
  global_indices <- 1:nrow(results)
  
  variation_mask <- stringr::str_detect(results$org_name_display, variation)
  variation_indices <- global_indices[variation_mask]
  
  results$org_name_display[variation_indices] <- stringr::str_replace(
    results$org_name_display[variation_indices],
    variation,
    standardized_word
  )
  
}

data.table::fwrite(results, "data/validate/manual/org_names.csv")

bmf_2025_raw[grepl("PPEP", bmf_2025_raw$NAME), ] |> View()


stringr::str_detect("LOWER BUCKS COUNTY ATHLETIC ASSN", " ASSN ")

# TODO
# NA values in org_name_cleaned need to be org_name_raw comverted to sentence case