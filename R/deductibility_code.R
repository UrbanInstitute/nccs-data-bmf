# Script to transform DEDUCTIBILITY column containing deductibility code

deductibility_code_lookup <- data.table::fread(
  deductibility_code_lookup_path,
  select = c("deductibility_code", "deductibility_code_definition")
)

transform_deductibility_code <- function(bmf_dt, deductibility_lookup = deductibility_code_lookup) {
  bmf_dt[, deductibility_code := as.integer(DEDUCTIBILITY)]
  bmf_dt[deductibility_lookup, 
         deductibility_code_definition := i.deductibility_code_definition, 
         on = .(deductibility_code)]
  return(bmf_dt)
}