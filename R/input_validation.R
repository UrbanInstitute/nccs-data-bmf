# Validate inputs in lookup table and BMF

validate_inputs <- function(dt, input_col, lookup, lookup_cols){
  if (!all(lookup_cols %in% names(lookup))) {
    stop("Lookup table missing required columns.")
  }
  if (!(input_col %in% names(dt))) {
    stop(paste0("Input data table missing column: ", input_col))
  }
}