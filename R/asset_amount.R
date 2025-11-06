transform_financials <- function(dt, input_col, output_col){
  if (! input_col %in% names(dt)){
    stop(paste0(input_col, " not found in data.table"))
  }
  negative_values <- dt[get(input_col) < 0, unique(get(input_col))]
  if (length(negative_values) > 0){
    warning(paste0(
      "The following negative values are present in ",
      input_col, 
      ":",
      paste(negative_values, collapse = ", ")
    ))
  }
  dt_safe <- data.table::copy(dt)
  dt_safe[, (output_col) := as.numeric(get(input_col))]
  failed_conversions <- dt_safe[is.na(get(output_col)) & 
                                  !is.na(get(input_col)) &
                                  get(input_col) != "", 
                                unique(get(input_col))]
  
  if (length(failed_conversions) > 0) {
    # Critical failure: Stop the pipeline and log the bad inputs
    stop(paste0("Asset amount conversion failed for non-numeric inputs: ", 
                paste(failed_conversions, collapse = ", "), 
                ". Check source data formatting."))
  }
  return(dt_safe)
}