# ============================================================================
# organization_name.R
# Transform NAME column to standardized organization name fields
# ============================================================================

# ============================================================================
# Module Constants
# ============================================================================

#' Legal suffix mappings from variations to standard forms
#' @noRd
SUFFIX_MAP <- data.table::data.table(
  variation = c(
    # Common Corporation variations
    "INC", "INC-", "INC\\.", "INCORPORATED", " I N C", "IN C", "I NC", " INC ", " INC - ",
    "CORP", "CORP\\.", "CORPORATION", "COR P", "CO RP", "A NONPROFIT CORPORATION",
    "CO", "CO\\.", "COMPANY",
    # Limited Liability Company variations
    "LLC", "LLC\\.", "L L C", "L\\.L\\.C.", "LIMITED LIABILITY CO",
    "LTD", "LTD\\.", "LIMITED",
    # Professional Corporation variations
    "PC", "PC\\.", "P C",
    # Trust variations
    "TR", "TR\\.", "TRUST", " TR ",
    "TUA", "TUA\\.", "T\\.U\\.A.", "TRUST UNDER AGREEMENT",
    "TUW", "TUW\\.", "T\\.U\\.W.", "TRUST UNDER WILL", "TR UW",
    # Org Variations
    "DOT ORG", "\\sORG$",
    "NFP"
  ),
  standard_suffix = c(
    rep("INC", 9),
    rep("CORP", 6),
    rep("CO", 3),
    rep("LLC", 5),
    rep("LTD", 3),
    rep("PC", 3),
    rep("TR", 4),
    rep("TUA", 4),
    rep("TUW", 5),
    rep("ORG", 2),
    "NFP"
  )
)

# Add length column and sort by length descending (longest patterns first)
SUFFIX_MAP[, length := nchar(variation)]
data.table::setorder(SUFFIX_MAP, -length)

#' Word standardization patterns for display names
#' @noRd
STANDARDIZATION_PATTERNS <- data.table::data.table(
  variation = c(
    "Assoc\\s*$", "Assn", "Natl", "Pta", "Ptso", "Pto",
    "Ymca", "Dmv", "FBO", "\\s.*Pba\\s*", "\\sPs\\s",
    "Jgb\\s", "Camg-F", "Hcsa\\s", "Mcmillan",
    "@", "%", "\\sNea$", "\\sUsa\\s", "\\sNj\\s", "\\sDc\\s*"
  ),
  standardized_word = c(
    "Association", "Association", "National", "PTA", "PTSO", "PTO",
    "YMCA", "DMV", "", "PBA", " PS ",
    "JGB ", "CAMG", "HCSA ", "McMillan",
    " ", " ", " NEA", " USA ", " NJ ", " DC "
  )
)

# Output column names
ORG_NAME_OUTPUT_COLS <- c(
  "org_name_raw",
  "org_name_join",
  "org_name_display",
  "org_legal_suffix"
)

# ============================================================================
# Internal Helper Functions
# ============================================================================

#' Extract Legal Suffix from Organization Name
#'
#' @description
#' Iterates through suffix patterns (longest first) to extract and standardize
#' legal suffixes from organization names. Only the first matching suffix is
#' extracted to avoid double-processing.
#'
#' @param dt data.table with org_name_raw, org_legal_suffix, org_name_join,
#'   org_name_display columns
#' @param suffix_map data.table with variation, standard_suffix, length columns
#'
#' @return data.table modified in place with extracted suffixes
#'
#' @noRd
.extract_legal_suffix <- function(dt, suffix_map) {

  for (i in seq_len(nrow(suffix_map))) {
    variation <- suffix_map$variation[i]
    standard_suffix <- suffix_map$standard_suffix[i]
    pattern <- paste0("\\s*", variation, "\\s*$")

    # Only process rows without an already-extracted suffix
    dt[is.na(org_legal_suffix) &
         stringr::str_detect(org_name_raw, pattern), `:=`(
           org_legal_suffix = standard_suffix,
           org_name_join = stringr::str_replace(org_name_raw, pattern, " "),
           org_name_display = stringr::str_to_title(
             stringr::str_replace(org_name_raw, pattern, " ")
           )
         )]
  }

  return(dt)
}

#' Apply Word Standardization Patterns
#'
#' @description
#' Applies pattern-based word standardizations to display names (e.g.,
#' "Assn" -> "Association", "Pta" -> "PTA").
#'
#' @param dt data.table with org_name_display column
#' @param patterns data.table with variation, standardized_word columns
#'
#' @return data.table modified in place with standardized display names
#'
#' @noRd
.apply_standardization_patterns <- function(dt, patterns) {

  for (i in seq_len(nrow(patterns))) {
    variation <- patterns$variation[i]
    standardized_word <- patterns$standardized_word[i]

    dt[stringr::str_detect(org_name_display, variation),
       org_name_display := stringr::str_replace(
         org_name_display, variation, standardized_word
       )]
  }

  return(dt)
}

#' Apply Manual Name Overrides
#'
#' @description
#' Applies manual name overrides from a lookup table for specific organizations
#' that require special handling beyond pattern-based standardization.
#'
#' @param dt data.table with org_name_raw, org_name_join, org_name_display,
#'   org_legal_suffix columns
#' @param name_lookup data.table or data.frame with org_name_raw (key) and
#'   org_name_join, org_name_display, org_legal_suffix columns
#'
#' @return data.table modified in place with manual overrides applied
#'
#' @noRd
.apply_manual_overrides <- function(dt, name_lookup) {

  if (is.null(name_lookup) || nrow(name_lookup) == 0) {
    return(dt)
  }

  # Convert to data.table if needed
  if (!data.table::is.data.table(name_lookup)) {
    name_lookup <- data.table::as.data.table(name_lookup)
  }

  # Apply overrides row by row
  for (i in seq_len(nrow(name_lookup))) {
    dt[org_name_raw == name_lookup$org_name_raw[i], `:=`(
      org_name_join = name_lookup$org_name_join[i],
      org_name_display = name_lookup$org_name_display[i],
      org_legal_suffix = name_lookup$org_legal_suffix[i]
    )]
  }

  return(dt)
}

# ============================================================================
# Main Transformation Functions
# ============================================================================

#' Clean and Standardize Organization Names
#'
#' @description
#' Transforms raw organization names through a three-step cleaning process:
#' 1. Extract legal suffixes (INC, CORP, LLC, etc.) using pattern matching
#' 2. Apply word standardizations (abbreviations, acronyms)
#' 3. Apply manual overrides from lookup table (optional)
#'
#' The function produces three output columns:
#' - `org_name_raw`: Original name (preserved)
#' - `org_name_join`: Cleaned name suitable for joining/matching
#' - `org_name_display`: Title-cased name for display purposes
#' - `org_legal_suffix`: Extracted and standardized legal suffix
#'
#' @param raw_names character vector of raw organization names
#' @param suffix_map data.table with columns: variation, standard_suffix, length.
#'   Default uses module constant SUFFIX_MAP.
#' @param standardization_patterns data.table with columns: variation, standardized_word.
#'   Default uses module constant STANDARDIZATION_PATTERNS.
#' @param name_lookup data.table/data.frame with manual overrides. Must have columns:
#'   org_name_raw, org_name_join, org_name_display, org_legal_suffix.
#'   Default is NULL (no manual overrides).
#'
#' @return data.table with columns: org_name_raw, org_name_join, org_name_display,
#'   org_legal_suffix
#'
#' @examples
#' \dontrun{
#' # Basic usage with default patterns
#' names_dt <- clean_organization_names(bmf_raw$NAME)
#'
#' # With custom name lookup for manual overrides
#' name_overrides <- data.table::data.table(
#'   org_name_raw = "PECULIAR ORG NAME INC",
#'   org_name_join = "Peculiar Organization",
#'   org_name_display = "Peculiar Organization",
#'   org_legal_suffix = "INC"
#' )
#' names_dt <- clean_organization_names(bmf_raw$NAME, name_lookup = name_overrides)
#' }
#'
#' @export
clean_organization_names <- function(raw_names,
                                     suffix_map = SUFFIX_MAP,
                                     standardization_patterns = STANDARDIZATION_PATTERNS,
                                     name_lookup = NULL) {


  # Input validation

  if (!is.character(raw_names)) {
    stop("raw_names must be a character vector")
  }

  if (!data.table::is.data.table(suffix_map)) {
    stop("suffix_map must be a data.table")
  }

  if (!all(c("variation", "standard_suffix") %in% names(suffix_map))) {
    stop("suffix_map must have columns: variation, standard_suffix")
  }

  if (!data.table::is.data.table(standardization_patterns)) {
    stop("standardization_patterns must be a data.table")
  }

  if (!all(c("variation", "standardized_word") %in% names(standardization_patterns))) {
    stop("standardization_patterns must have columns: variation, standardized_word")
  }

  # Initialize result table

results <- data.table::data.table(
    org_name_raw = raw_names,
    org_name_join = NA_character_,
    org_name_display = NA_character_,
    org_legal_suffix = NA_character_
  )

  # Step 1: Extract legal suffixes
  .extract_legal_suffix(results, suffix_map)

  # Squish whitespace in processed columns
  processed_cols <- c("org_name_join", "org_name_display", "org_legal_suffix")
  results[, (processed_cols) := lapply(.SD, stringr::str_squish),
          .SDcols = processed_cols]

  # Fill in missing join names with raw names
  results[is.na(org_name_join), org_name_join := org_name_raw]

  # Fill in missing display names with title-cased join names
  results[is.na(org_name_display),
          org_name_display := stringr::str_to_title(org_name_join)]

  # Step 2: Apply word standardizations
  .apply_standardization_patterns(results, standardization_patterns)

  # Step 3: Apply manual overrides (if provided)
  .apply_manual_overrides(results, name_lookup)

  # Final whitespace cleanup
  results[, (processed_cols) := lapply(.SD, stringr::str_squish),
          .SDcols = processed_cols]

  return(results)
}

#' Transform Organization Name Column
#'
#' @description
#' Transforms the NAME column from raw BMF data into standardized organization
#' name fields. This is the main entry point for the BMF pipeline.
#'
#' @param dt data.table containing BMF data with NAME column
#' @param input_col character name of input column (default: "NAME")
#' @param name_lookup_path character path to Excel file with manual name overrides.
#'   Set to NULL to skip manual overrides. Default is NULL.
#'
#' @return data.table with new columns:
#'   \itemize{
#'     \item org_name_raw - Original organization name
#'     \item org_name_join - Cleaned name for matching/joining
#'     \item org_name_display - Title-cased display name
#'     \item org_legal_suffix - Extracted legal suffix (INC, CORP, etc.)
#'   }
#'
#' @examples
#' \dontrun{
#' # Basic transformation
#' bmf <- transform_organization_name(bmf_raw)
#'
#' # With manual name lookup file
#' bmf <- transform_organization_name(
#'   bmf_raw,
#'   name_lookup_path = "data/lookup/standardized_organization_names.xlsx"
#' )
#' }
#'
#' @export
transform_organization_name <- function(dt,
                                        input_col = "NAME",
                                        name_lookup_path = NULL) {

  # Input validation
  validate_data_table(dt, input_col, context = "BMF data")

  # Safe copy
  dt_safe <- data.table::copy(dt)

  # Load name lookup if path provided
  name_lookup <- NULL
  if (!is.null(name_lookup_path)) {
    if (!file.exists(name_lookup_path)) {
      warning(sprintf(
        "Name lookup file not found: %s. Proceeding without manual overrides.",
        name_lookup_path
      ))
    } else {
      name_lookup <- data.table::as.data.table(
        readxl::read_xlsx(name_lookup_path)
      )
      message(sprintf(
        "Loaded %d manual name overrides from %s",
        nrow(name_lookup),
        name_lookup_path
      ))
    }
  }

  # Extract raw names
  raw_names <- dt_safe[[input_col]]

  # Clean names
  name_results <- clean_organization_names(
    raw_names = raw_names,
    name_lookup = name_lookup
  )

  # Add result columns to main table
  dt_safe[, (ORG_NAME_OUTPUT_COLS) := name_results]

  # Quality report
  suffix_counts <- dt_safe[!is.na(org_legal_suffix), .N]
  total_rows <- nrow(dt_safe)
  message(sprintf(
    "Organization names: %s of %s (%0.1f%%) have extracted legal suffixes",
    format(suffix_counts, big.mark = ","),
    format(total_rows, big.mark = ","),
    100 * suffix_counts / total_rows
  ))

  return(dt_safe)
}

# ============================================================================
# Backward Compatibility
# ============================================================================

#' @rdname clean_organization_names
#' @description Legacy alias for clean_organization_names()
#' @export
clean_names <- function(raw_names,
                        suffix_map = SUFFIX_MAP,
                        standardization_lookup = STANDARDIZATION_PATTERNS,
                        name_lookup = NULL) {
  clean_organization_names(
    raw_names = raw_names,
    suffix_map = suffix_map,
    standardization_patterns = standardization_lookup,
    name_lookup = name_lookup
  )
}
