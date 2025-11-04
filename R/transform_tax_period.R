# Tax Period (TAX_PERIOD)

transform_tax_period <- function(dt, input_col){
  dt_safe <- data.table::copy(dt)
  if (! input_col %in% names(dt)){
    stop(paste0("Input data table missing column: ", input_col))
  }
  # Check for required length (YYYYMM = 6 characters)
  if (dt_safe[, any(nchar(get(input_col)) != 6 &
               !is.na(get(input_col)))]) {
    stop(
      paste0(
        "Input column '",
        input_col,
        "' contains non-YYYYMM values. Length check failed."
      )
    )
  }
  # Tax period is in YYYYMM format
  dt_safe[, tax_period_ymd := lubridate::ymd(
    paste0(
      as.character(get(input_col)),
      "01"
    )
  )]
  # NA Values flagged and recoded to 1900-01-01
  dt_safe[, tax_period_is_missing := is.na(tax_period_ymd)]
  dt_safe[is.na(tax_period_ymd), 
          tax_period_ymd := lubridate::ymd("1900-01-01")]
  return(dt_safe)
}