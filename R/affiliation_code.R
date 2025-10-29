# Transform affiliation code

affiliation_code_lookup <- data.table::fread(affiliation_code_lookup_path, select = list(
  character = c("affiliation_code", "affiliation_code_definition")
))

transform_affiliation_code <- function(bmf_dt, affiliation_code_lookup = affiliation_code_lookup) {
  bmf_dt[, affiliation_code := as.character(AFFILIATION)]
  bmf_dt[affiliation_code_lookup, affiliation_code_definition := i.affiliation_code_definition, on = .(affiliation_code)]
  return(bmf_dt)
}