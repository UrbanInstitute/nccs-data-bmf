# Code to transform financial codes: Asset code (ASSET_CD) and Income code (INCOME_CD)

transform_financial_code <- function(dt, 
                                     lookup, 
                                     input_col, 
                                     lookup_key,
                                     definition_col) {
  validate_inputs(dt, 
                  input_col, 
                  lookup, 
                  lookup_cols = c(lookup_key, definition_col))
  dt_safe <- data.table::copy(dt)
  # Convert to character before integer, trapping conversion errors.
  dt_safe[, (lookup_key) := suppressWarnings(
    as.integer(
      as.character(get(input_col))
    )
  )]
  # Type checking
  if (dt_safe[, any(is.na(get(lookup_key)) & !is.na(get(input_col)))]) {
    stop("Asset code conversion failed: Non-numeric values detected.")
  }
  # Set key for fast join
  data.table::setkeyv(lookup, lookup_key)
  data.table::setkeyv(dt_safe, lookup_key)
  dt_safe[lookup,
          (definition_col) := get(paste0("i.", definition_col))]
  # Post join check
  if (dt_safe[, 
              any(
                is.na(get(definition_col)) & !is.na(get(lookup_key))
              )]) {
    warning(
      "Some valid financial codes failed lookup (missing from the lookup table)."
    )
  }
  return(dt_safe)
}