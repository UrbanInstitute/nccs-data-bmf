transform_accounting_period <- function(dt, acct_pd_col = "ACCT_PD") {
  if (! acct_pd_col %in% names(dt)){
    stop(paste0(acct_pd_col), " not present in data.table")
  }
  dt_safe <- data.table::copy(dt)
  dt_safe[, accounting_period  := as.integer(ACCT_PD)]
  invalid_codes <- dt_safe[!(accounting_period >= 0 & accounting_period <= 12) & !is.na(accounting_period), unique(accounting_period)]
  
  if (length(invalid_codes) > 0) {
    stop(paste0("Invalid accounting periods detected: ", 
                paste(invalid_codes, collapse = ", "),
                ". Periods must be 1-12 (or 0 for undefined values)."))
  }
  return(dt_safe)
}