# ORGANIZATION CODE (ORGANIZATION)
organization_code_lookup_cols <- c("organization_code", "organization_code_definition")

organization_code_lookup <- data.table::fread(
  organization_code_lookup_path,
  select = organization_code_lookup_cols
)

transform_organization_code <- function(
    dt, 
    input_col,
    lookup = organization_code_lookup, 
    lookup_cols = organization_code_lookup_cols) {
  # Input validation
  if (!all(lookup_cols %in% names(lookup))) {
    stop("Lookup table missing required columns.")
  }
  if (!(input_col %in% names(dt))) {
    stop(paste0("Input data table missing column: ", input_col))
  }
  dt_clean <- data.table::copy(dt)
  dt_clean[, organization_code := as.integer(get(input_col))]
  dt_clean[lookup,
           organization_code_definition := i.organization_code_definition,
           on = .(organization_code)]
  return(dt_clean)
}
