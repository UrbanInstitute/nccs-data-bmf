# Script to transform foundation code: FOUNDATION

foundation_code_lookup <- data.table::fread(
  foundation_code_lookup_path,
  select = c("foundation_code", "foundation_code_definition")
)

transform_foundation_code <- function(bmf_dt, lookup = foundation_code_lookup) {
  bmf_dt[, foundation_code := as.integer(FOUNDATION)]
  bmf_dt[lookup,
         foundation_code_definition := i.foundation_code_definition,
         on = .(foundation_code)]
  return(bmf_dt)
}

bmf_2025_raw |>
  dplyr::filter(FOUNDATION %in% c(6, 7, 25)) |>
  View()
