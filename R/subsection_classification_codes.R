# ============================================================================
# subsection_classification_codes.R
# Transform SUBSECTION and CLASSIFICATION columns with dimension table
# ============================================================================

# ============================================================================
# Module Constants
# ============================================================================

SUBSECTION_CLASSIFICATION_LOOKUP_COLS <- c(
  "subsection_code",
  "classification_code",
  "classification_description",
  "exempt_organization_type",
  "effective_start_date",
  "effective_end_date"
)

# Sentinel value for undefined classification codes
CLASSIFICATION_CODE_UNDEFINED <- "0"

# ============================================================================
# Lookup Table Loading
# ============================================================================

subsection_classification_code_lookup <- data.table::fread(
  subsection_classification_code_lookup_path,
  select = SUBSECTION_CLASSIFICATION_LOOKUP_COLS,
  colClasses = list(character = c(
    "subsection_code",
    "classification_code",
    "classification_description",
    "exempt_organization_type"
  ))
)

# Derived lookup tables
subsection_orgtype_lookup <- subsection_classification_code_lookup[,
  .(subsection_code, exempt_organization_type)
] |> unique()

classification_code_lookup <- subsection_classification_code_lookup[,
  .(subsection_code, classification_code, classification_description)
] |> unique()

# ============================================================================
# Dimension Table Function
# ============================================================================

#' Create Classification Code Dimension Table
#'
#' @description
#' Creates an SCD Type 2 dimension table from the CLASSIFICATION column.
#' The raw CLASSIFICATION field contains up to 4 classification codes
#' (one character each). This function unpivots them into a long-format
#' dimension table with one row per EIN-classification code combination.
#'
#' @param dt data.table containing BMF data with SUBSECTION and CLASSIFICATION columns
#' @param lookup data.table classification code lookup table
#' @param year character effective year for the dimension table
#'
#' @return data.table with columns:
#'   \itemize{
#'     \item ein - Employer Identification Number
#'     \item subsection_code - IRC subsection code
#'     \item cl_code - Single classification code character
#'     \item classification_description - Human-readable description
#'     \item effective_year - Year of the BMF extract
#'     \item rowID - Unique row identifier
#'   }
#'
#' @examples
#' \dontrun{
#' dim_table <- create_cl_code_dim_table(bmf_raw, classification_code_lookup, "2025")
#' }
#'
#' @export
create_cl_code_dim_table <- function(dt,
                                     lookup = classification_code_lookup,
                                     year) {

  # Input validation
  validate_data_table(dt, c("ein", "SUBSECTION", "CLASSIFICATION"), context = "BMF data")
  validate_lookup(
    lookup,
    c("subsection_code", "classification_code", "classification_description"),
    "classification_code_lookup"
  )

  # Extract relevant columns and convert types
  dt_work <- dt[, .(
    ein,
    subsection_code = as.character(SUBSECTION),
    classification_code_raw = as.character(CLASSIFICATION)
  )]

  # Extract individual classification codes (up to 4 characters)
  dt_work[, `:=`(
    cl_code_1 = substr(classification_code_raw, 1, 1),
    cl_code_2 = substr(classification_code_raw, 2, 2),
    cl_code_3 = substr(classification_code_raw, 3, 3),
    cl_code_4 = substr(classification_code_raw, 4, 4)
  )]

  # Melt to long format
  dt_long <- data.table::melt(
    dt_work,
    id.vars = c("ein", "subsection_code"),
    measure.vars = c("cl_code_1", "cl_code_2", "cl_code_3", "cl_code_4"),
    value.name = "cl_code"
  )

  # Remove undefined codes (0) and empty values
  dt_long <- dt_long[cl_code != CLASSIFICATION_CODE_UNDEFINED & cl_code != ""]

  # Remove variable column
  dt_long[, variable := NULL]

  # Join with lookup for descriptions
  dt_long[lookup,
          classification_description := i.classification_description,
          on = .(subsection_code, cl_code = classification_code)]

  # Add metadata
  dt_long[, `:=`(
    rowID = .I,
    effective_year = year
  )]

  # Validate join success
  validate_join_success(
    dt_long,
    key_col = "cl_code",
    result_col = "classification_description",
    lookup_name = "classification_code_lookup"
  )

  return(dt_long)
}

# ============================================================================
# Main Transformation Function
# ============================================================================

#' Transform Subsection and Classification Codes
#'
#' @description
#' Transforms SUBSECTION and CLASSIFICATION columns by adding the exempt
#' organization type and a concatenated string of all classification
#' descriptions for each organization.
#'
#' @param dt data.table containing BMF data with SUBSECTION column
#' @param dim_table data.table classification dimension table from
#'   \code{create_cl_code_dim_table()}
#' @param orgtype_lookup data.table mapping subsection codes to organization types
#'
#' @return data.table with new columns:
#'   \itemize{
#'     \item subsection_code - IRC subsection code (character)
#'     \item classification_code - Raw classification codes (character)
#'     \item exempt_organization_type - Type of exempt organization
#'     \item all_classifications_string - Semicolon-separated descriptions
#'   }
#' 
#' @note
#' BMF pipeline function. Modifies input in place for efficiency. Caller should pass a copy if original must be preserved.
#' 
#' @examples
#' \dontrun{
#' dim_table <- create_cl_code_dim_table(bmf_raw, classification_code_lookup, "2025")
#' bmf_transformed <- transform_subsection_classification_codes(
#'   bmf_raw, dim_table, subsection_orgtype_lookup
#' )
#' }
#'
#' @export
transform_bmf_subsection_classification_codes <- function(dt,
                                                      dim_table,
                                                      orgtype_lookup = subsection_orgtype_lookup) {

  # Input validation
  validate_data_table(dt, c("SUBSECTION", "CLASSIFICATION"), context = "BMF data")
  validate_data_table(dim_table, c("ein", "classification_description"),
                      context = "classification dimension table")
  validate_lookup(orgtype_lookup, c("subsection_code", "exempt_organization_type"),
                  "subsection_orgtype_lookup")

  # Add subsection and classification columns
  dt[, subsection_code := as.character(SUBSECTION)]
  dt[, classification_code := as.character(CLASSIFICATION)]

  # Aggregate dimension table: concatenate all descriptions per EIN
  cl_summary <- dim_table[, .(
    all_classifications_string = paste(classification_description, collapse = "; ")
  ), by = .(ein)]

  # Join organization type from subsection
  dt[orgtype_lookup,
          exempt_organization_type := i.exempt_organization_type,
          on = .(subsection_code)]

  # Join classification descriptions
  dt[cl_summary,
          all_classifications_string := i.all_classifications_string,
          on = .(ein)]

  return(dt)
}
