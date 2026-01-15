apply_lookup_table <- function(target_dt,
                               lookup_dt,
                               target_col,
                               assign_col,
                               join_key) {
  data.table::setnames(target_dt, old = target_col, new = assign_col)
  target_dt[lookup_dt, 
            (assign_col) := get(assign_col), 
            on = join_key, 
            nomatch = NULL]
  invisible(target_dt)
}

bmf_2025_preprocessed <- apply_lookup_table(bmf_2025_raw,
                                            foundation_code_lookup,
                                            target_col = FOUNDATION,
                                            assign_col = "foundation_code")

