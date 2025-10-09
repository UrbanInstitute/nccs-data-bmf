# Functions to transform `ICO` column: In-Care-Of name

transform_ico_name <- function(bmf_dt){
  data.table::setnames(bmf_dt, "ICO", "in_care_of_name_raw")
  # The % prefix is included with every name
  bmf_dt[, in_care_of_name_clean := stringr::str_remove(in_care_of_name_raw, "^%\\s.*")]
  bmf_dt[, in_care_of_name_clean := stringr::str_to_title(in_care_of_name_clean)]
  bmf_dt[, in_care_of_name_clean := stringr::str_squish(in_care_of_name_clean)]
  # Flag for debugging, quality assurance and metadata
  bmf_dt[, in_care_of_name_provided := data.table::fifelse(in_care_of_name_raw == "", FALSE, TRUE)]
  return(bmf_dt)
}