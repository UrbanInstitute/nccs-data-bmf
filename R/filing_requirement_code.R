transform_code <- function(dt,
                           input_col,
                           lookup_key,
                           definition_col,
                           type_conversion_func,
                           lookup) {
  dt_safe <- data.table::copy(dt)
  lookup_safe <- data.table::copy(lookup)
  
  dt_safe[, (lookup_key) := type_conversion_func(get(input_col))]
  # Type checking
  if (dt_safe[, any(is.na(get(lookup_key)) &
                    !is.na(get(input_col)))]) {
    stop(paste0(input_col, " type conversion failed: NA values detected."))
  }
  data.table::setkeyv(dt_safe, lookup_key)
  data.table::setkeyv(lookup_safe, lookup_key)
  dt_safe[lookup_safe, (definition_col) := get(paste0("i.", definition_col))]
  # Post join check
  if (dt_safe[, any(is.na(get(definition_col)) &
                    !is.na(get(lookup_key)))]) {
    warning("Some valid codes are missing from the lookup table.")
  }
  return(dt_safe)
}