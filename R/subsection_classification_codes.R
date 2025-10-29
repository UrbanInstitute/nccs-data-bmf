# Script to process subsection and classification codes

subsection_classification_code_lookup <- data.table::fread(
  subsection_classification_code_lookup_path,
  select = list(
    character = c(
      "subsection_code",
      "classification_code",
      "classification_description",
      "exempt_organization_type",
      "effective_start_date",
      "effective_end_date"
    )
  )
)

# Lookup tables
subsection_orgtype_lookup <- subsection_classification_code_lookup[, .(subsection_code, exempt_organization_type)] |> unique()
classification_code_lookup <- subsection_classification_code_lookup[, .(subsection_code, classification_code, classification_description)] |> unique()

# Dimension table of subsection and classification codes
create_cl_code_dim_table <- function(bmf_dt, cl_lookup, year) {
  
  dt <- bmf_dt[, .(ein, 
                   subsection_code = as.character(SUBSECTION), 
                   classification_code_raw = as.character(CLASSIFICATION))]

  dt[, `:=` (
    cl_code_1 = substr(classification_code_raw, 1, 1),
    cl_code_2 = substr(classification_code_raw, 2, 2),
    cl_code_3 = substr(classification_code_raw, 3, 3),
    cl_code_4 = substr(classification_code_raw, 4, 4)
  )]
  
  dt_long <- data.table::melt(
    dt,
    id.vars = c("ein", "subsection_code"),
    measure.vars = c("cl_code_1", "cl_code_2", "cl_code_3", "cl_code_4"),
    value.name = "cl_code"
  )
  
  dt_long <- dt_long[cl_code != "0"] 
  dt_long[, variable := NULL]

  dt_long[cl_lookup, 
          classification_description := i.classification_description,
          on = .(subsection_code, cl_code = classification_code)]
  dt_long[, `:=`(rowID = .I,
              effective_year = year)]
  
  return(dt_long) 
}


transform_subsection_classification_codes <- function(bmf_dt, cl_code_dim_dt, orgtype_lookup) {
  bmf_dt[, .(subsection_code := as.character(SUBSECTION),
             classification_code := as.character(CLASSIFICATION))]
  
  cl_summary <- cl_code_dim_dt[, .(all_classifications_string = paste(classification_description, collapse = "; ")), by = .(ein)]
  
  bmf_dt[orgtype_lookup, exempt_organization_type := i.exempt_organization_type, on = .(subsection_code)]
  
  bmf_dt[cl_summary, all_classifications_string := i.all_classifications_string, on = .(ein)]
  
  return(bmf_dt)
}