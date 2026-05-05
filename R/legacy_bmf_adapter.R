# ============================================================================
# legacy_bmf_adapter.R
# Harmonize legacy NCCS 501CX-NONPROFIT-PX BMF files to the current schema.
#
# Crosswalk source: data/crosswalks/XWALK-BMF-V2.0.csv
# Inventory source: data/crosswalks/legacy_column_inventory.csv
# ============================================================================

# Mapping from current-schema raw column -> set of transform output columns.
# Used by compute_legacy_output_columns() to slim Phase 11 output to only those
# columns whose underlying input was actually populated in the legacy file.
RAW_TO_OUTPUT_MAP <- list(
  EIN              = c("ein", "ein_raw"),
  NAME             = c("org_name_raw", "org_name_join", "org_name_display",
                       "org_parent_name", "org_legal_suffix"),
  ICO              = c("in_care_of_name_raw", "in_care_of_name_clean",
                       "in_care_of_name_provided"),
  STREET           = c("org_addr_street_raw", "org_addr_street",
                       "org_addr_is_po_box", "org_addr_is_rural_route",
                       "org_addr_has_special_chars", "org_addr_missing_number"),
  CITY             = c("org_addr_city_raw", "org_addr_city"),
  STATE            = c("org_addr_state_raw", "org_addr_state",
                       "org_addr_state_invalid"),
  ZIP              = c("org_addr_zip_raw", "org_addr_zip5", "org_addr_zip4",
                       "org_addr_zip"),
  GROUP            = c("group_exemption_number_raw", "group_exemption_number",
                       "group_exemption_is_member"),
  SUBSECTION       = c("subsection_code", "exempt_organization_type"),
  CLASSIFICATION   = c("classification_code", "all_classifications_string"),
  AFFILIATION      = c("affiliation_code", "affiliation_code_definition"),
  RULING           = c("ruling_date_ym_str", "ruling_date", "ruling_date_is_missing"),
  DEDUCTIBILITY    = c("deductibility_code", "deductibility_code_definition"),
  FOUNDATION       = c("foundation_code", "foundation_code_definition"),
  ACTIVITY         = c("activity_code", "activity_code_definitions",
                       "activity_code_categories"),
  ORGANIZATION     = c("organization_code", "organization_code_definition"),
  STATUS           = c("status_code", "status_code_definition"),
  TAX_PERIOD       = c("tax_period_ym_str", "tax_period_ymd", "tax_period_is_missing"),
  ASSET_CD         = c("asset_code", "asset_code_definition"),
  INCOME_CD        = c("income_code", "income_code_definition"),
  FILING_REQ_CD    = c("filing_requirement_code", "filing_requirement_code_definition"),
  PF_FILING_REQ_CD = c("pf_filing_requirement_code", "pf_filing_requirement_code_definition"),
  ASSET_AMT        = c("asset_amount"),
  INCOME_AMT       = c("income_amount"),
  REVENUE_AMT      = c("revenue_amount"),
  NTEE_CD          = c("ntee_code_raw", "ntee_code_clean", "ntee_code_definition",
                       "ntee_code_major_group", "ntee_common_code",
                       "ntee_common_code_definition", "naics_code",
                       "nteev2", "nteev2_code", "nteev2_subsector",
                       "nteev2_subsector_definition", "nteev2_org_type"),
  SORT_NAME        = c("dba_name_raw", "dba_name"),
  ACCT_PD          = c("accounting_period")
)

# Output columns derived from multiple raw inputs. Kept only when ALL listed
# raw inputs were populated.
DERIVED_OUTPUT_COLUMNS <- list(
  org_addr_full      = c("STREET", "CITY", "STATE", "ZIP"),
  org_addr_is_missing = c("STREET", "CITY", "STATE", "ZIP")
)


#' Load the legacy BMF crosswalk (XWALK-BMF-V2.0.csv)
#'
#' @return data.table keyed by legacy_name_upper
load_crosswalk_v2 <- function(path = here::here("data", "crosswalks", "XWALK-BMF-V2.0.csv")) {
  if (!file.exists(path)) {
    log_error(sprintf("Crosswalk not found: %s", path))
  }
  xw <- data.table::fread(path, na.strings = c("", "NA"))
  required <- c("legacy_name_upper", "current_name", "disposition")
  missing <- setdiff(required, names(xw))
  if (length(missing) > 0) {
    log_error(sprintf("Crosswalk missing required columns: %s",
                      paste(missing, collapse = ", ")))
  }
  data.table::setkey(xw, legacy_name_upper)
  xw
}


#' Harmonize a raw legacy BMF data.table to current schema column names
#'
#' Steps: (1) uppercase column names; (2) error on columns not in crosswalk;
#' (3) drop columns marked drop; (4) rename columns marked rename;
#' (5) NA-fill any BMF_REQUIRED_COLUMNS that remain absent so existing
#' transforms can run unmodified.
#'
#' @param dt data.table with raw legacy BMF columns
#' @param crosswalk data.table from load_crosswalk_v2()
#' @return list(dt = harmonized data.table, report = list of harmonization details)
harmonize_legacy_bmf <- function(dt, crosswalk) {
  dt <- data.table::copy(dt)
  original_names <- names(dt)

  data.table::setnames(dt, original_names, toupper(original_names))
  upper_names <- names(dt)

  unknown <- setdiff(upper_names, crosswalk$legacy_name_upper)
  if (length(unknown) > 0) {
    log_error(sprintf(
      "Legacy BMF contains %d column(s) not present in XWALK-BMF-V2.0.csv: %s. Add a disposition before processing.",
      length(unknown), paste(unknown, collapse = ", ")
    ))
  }

  # Drop columns marked drop
  drops <- intersect(upper_names, crosswalk[disposition == "drop", legacy_name_upper])
  if (length(drops) > 0) {
    dt[, (drops) := NULL]
  }

  # Rename columns marked rename
  rename_rows <- crosswalk[disposition == "rename" & legacy_name_upper %in% names(dt)]
  rename_pairs <- list()
  if (nrow(rename_rows) > 0) {
    data.table::setnames(dt, rename_rows$legacy_name_upper, rename_rows$current_name)
    rename_pairs <- stats::setNames(
      as.list(rename_rows$current_name),
      rename_rows$legacy_name_upper
    )
  }

  # NA-fill missing required columns
  na_filled <- setdiff(BMF_REQUIRED_COLUMNS, names(dt))
  for (col in na_filled) {
    dt[, (col) := NA_character_]
  }

  report <- list(
    timestamp = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"),
    input_column_count = length(original_names),
    input_columns = original_names,
    columns_dropped = drops,
    columns_renamed = rename_pairs,
    columns_na_filled = na_filled,
    output_column_count = ncol(dt)
  )

  log_info(sprintf(
    "Harmonization: %d input cols -> dropped %d, renamed %d, NA-filled %d, final %d cols",
    length(original_names), length(drops), nrow(rename_rows),
    length(na_filled), ncol(dt)
  ))

  list(dt = dt, report = report)
}


#' Compute the set of transform-output columns to keep in Phase 11 output
#'
#' Given the columns actually present in a legacy file (post-harmonization,
#' pre-NA-fill), returns the union of:
#'   (a) output columns whose backing raw column was renamed (populated)
#'   (b) derived output columns whose ALL backing raw columns were populated
#'
#' @param populated_raw_columns character vector of current-schema raw columns
#'   that have real data (i.e., legacy file had a rename source for them)
#' @return character vector of transform-output column names to retain
compute_legacy_output_columns <- function(populated_raw_columns) {
  keep <- unlist(RAW_TO_OUTPUT_MAP[populated_raw_columns], use.names = FALSE)

  for (out_col in names(DERIVED_OUTPUT_COLUMNS)) {
    needed <- DERIVED_OUTPUT_COLUMNS[[out_col]]
    if (all(needed %in% populated_raw_columns)) {
      keep <- c(keep, out_col)
    }
  }

  unique(keep)
}


#' Save a harmonization report as JSON
#'
#' @param report list returned by harmonize_legacy_bmf()
#' @param output_path character path
save_harmonization_report <- function(report, output_path) {
  dir <- dirname(output_path)
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
  jsonlite::write_json(report, output_path, auto_unbox = TRUE, pretty = TRUE,
                       null = "null", na = "null")
  log_info(sprintf("Harmonization report saved: %s", output_path))
}
