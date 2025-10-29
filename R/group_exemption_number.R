# Script to transform `GROUP` column containing group exemption number

transform_group_exemption_number  <- function(bmf_dt){
  dt <- data.table::copy(bmf_dt)
  data.table::setnames(dt, "GROUP", "group_exemption_number_raw")
  dt[, group_exemption_number_clean := stringr::str_pad(group_exemption_number_raw, width = 4, side = "left", pad = "0")]
  dt[, group_exemption_number_is_absent:= data.table::fifelse(
    group_exemption_number_raw == "0",
    TRUE,
    FALSE
  )]
  return(dt)
}