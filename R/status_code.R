transform_status_code <- function(dt, input_col, lookup, lookup_cols) {
  validate_inputs(dt, input_col, lookup, lookup_cols)
  dt_clean <- data.table::copy(dt)
  dt_clean[, status_code := as.integer(get(input_col))]
  dt_clean[lookup,
           status_code_definition := i.status_code_definition,
           on = .(status_code)]
  return(dt_clean)
}