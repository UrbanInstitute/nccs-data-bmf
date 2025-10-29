## Script to transform ruling date

transform_ruling_date <- function(bmf_dt) {
  bmf_dt[, ruling_date_ym_str := as.character(RULING)]
  bmf_dt[, ruling_date_standard_str := paste0(ruling_date_ym_str, "01")]
  bmf_dt[, ruling_date_standard := lubridate::ymd(ruling_date_standard_str)]
  return(bmf_dt)
}