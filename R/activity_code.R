activity_code_cols <- c(
  "activity_code",
  "activity_code_category",
  "activity_code_definition"
)

activity_code_lookup <- data.table::fread(activity_code_lookup_path,
                                          select = activity_code_cols,
                                          colClasses = list(character = activity_code_cols))

create_activity_code_dim_table <- function(dt, lookup) {
  dt_cleaned <- clean_activity_code(dt)
  dt_cleaned[, `:=` (
    activity_code_1 = substr(activity_code, 1, 3),
    activity_code_2 = substr(activity_code, 4, 6),
    activity_code_3 = substr(activity_code, 7, 9)
  )]
  # Activity code SCD Type 2 Table
  dt_long <- data.table::melt(
    dt_cleaned,
    id.vars = c("ein"),
    measure.vars = c("activity_code_1", "activity_code_2", "activity_code_3"),
    value.name = "activity_code"
  )
  dt_long <- dt_long[activity_code != "000"] 
  dt_long[, variable := NULL]
  # 2 joins because there's not a 1-1 correspondence between codes, definitions, and categories
  dt_long[lookup, 
          activity_code_definition := i.activity_code_definition,
          on = .(activity_code)]
  dt_long[lookup, 
          activity_code_category := i.activity_code_category,
          on = .(activity_code)]
  return(dt_long)
}

transform_activity_code <- function(dt_cleaned, dim_table) {
  # Input Validation
  required_cols <- c("ein", "activity_code_definition", "activity_code_category")
  if (!all(required_cols %in% names(dim_table))) {
    stop("dim_table missing required columns: ",
         paste(setdiff(required_cols, names(dim_table)), collapse = ", "))
  }
  dim_summary <- dim_table[, 
                           .(activity_code_definitions = paste(activity_code_definition, collapse = ";"),
                             activity_code_categories = paste(activity_code_category, collapse = ";"))
                           , by = .(ein)]
  
  dt_cleaned[dim_summary, 
             `:=` (activity_code_definitions = i.activity_code_definitions,
               activity_code_categories = i.activity_code_categories), 
             on = .(ein)]
  return(dt_cleaned)
}

clean_activity_code <- function(dt, activity_col_raw = "ACTIVITY") {
  dt[, activity_code := as.character(get(activity_col_raw))]
  # Validation checks
  if (any(nchar(dt$activity_code) > 9)) {
    warning("Activity codes with > 9 characters found; padding may be incorrect")
  }
  dt[, activity_code := stringr::str_pad(
    activity_code, 
    width = 9,
    side = "left",
    pad = "0"
  )]
  return(dt)
}
