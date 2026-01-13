# ============================================================================
# transform_ntee_code.R
# Functions to transform NTEE Code columns from IRS BMF data
# ============================================================================

# ============================================================================
# Module Constants
# ============================================================================

# Sentinel values for NTEE codes
NTEE_INVALID <- "INVALID"
NTEE_UNDEFINED <- "UNDEFINED"

# Required lookup table columns
NTEE_CODE_LOOKUP_COLS <- c("ntee_code", "naics_code", "ntee_code_definition")
NTEE_MAJOR_GROUP_LOOKUP_COLS <- c("ntee_code_first_character", "ntee_code_major_group")
NTEE_ACTIVITY_LOOKUP_COLS <- c("ntee_activity_code", "ntee_activity_code_definition")

# Helper column names (dot prefix for internal use)
HELPER_COLUMNS <- c(".len", ".first", ".last", ".int23", ".char23")

# Columns to set to UNDEFINED when NTEE code is invalid
COLS_TO_FIX <- c("naics_code", "ntee_code_major_group", "ntee_code_definition")

# NTEEV2 subsector special mappings
NTEEV2_SUBSECTOR_UNIVERSITY <- c("B40", "B41", "B42", "B43", "B50")
NTEEV2_SUBSECTOR_HOSPITAL <- c("E20", "E21", "E22", "E24")

# ============================================================================
# Input Validation Functions
# ============================================================================

.validate_ntee_lookups <- function(ntee_code_lookup,
                                   ntee_major_group_lookup,
                                   activity_code_lookup) {

  # Validate ntee_code lookup

  missing_ntee <- setdiff(NTEE_CODE_LOOKUP_COLS, names(ntee_code_lookup))
  if (length(missing_ntee) > 0) {
    stop("ntee_code_lookup missing required columns: ",
         paste(missing_ntee, collapse = ", "))
  }

  # Validate major group lookup
  missing_major <- setdiff(NTEE_MAJOR_GROUP_LOOKUP_COLS,
                           names(ntee_major_group_lookup))
  if (length(missing_major) > 0) {
    stop("ntee_major_group_lookup missing required columns: ",
         paste(missing_major, collapse = ", "))
  }

  # Validate activity code lookup
  missing_activity <- setdiff(NTEE_ACTIVITY_LOOKUP_COLS,
                              names(activity_code_lookup))
  if (length(missing_activity) > 0) {
    stop("activity_code_lookup missing required columns: ",
         paste(missing_activity, collapse = ", "))
  }

  invisible(TRUE)
}

.validate_ntee_input_column <- function(dt, input_ntee_col) {
  if (!(input_ntee_col %in% names(dt))) {
    stop("Input data.table missing column: ", input_ntee_col)
  }

  if (!("ein" %in% names(dt))) {
    stop("Input data.table missing required 'ein' column for SCD table.")
  }

  invisible(TRUE)
}

# ============================================================================
# SCD Table Functions
# ============================================================================

.build_ntee_scd_table <- function(dt, year) {
  scd <- dt[, .(
    ein,
    ntee_code_raw,
    ntee_code_clean,
    ntee_code_definition,
    ntee_code_major_group,
    ntee_activity_code,
    nteev2_code,
    nteev2_subsector,
    nteev2_org_type
  )]
  scd[, effective_year := year]
  return(scd)
}

.write_ntee_scd_table <- function(scd, path) {
  if (is.null(path) || is.na(path) || path == "") {
    message("No path provided; SCD table not written to file.")
    return(invisible(FALSE))
  }

  tryCatch({
    data.table::fwrite(scd, path)
    message(sprintf("SCD table written to: %s (%s rows)",
                    path, format(nrow(scd), big.mark = ",")))
    invisible(TRUE)
  }, error = function(e) {
    warning("Failed to write SCD table to '", path, "': ", e$message)
    invisible(FALSE)
  })
}

# ============================================================================
# Main Transformation Function
# ============================================================================

#' Transform NTEE Code Column
#'
#' @description
#' Transforms raw NTEE codes from the IRS BMF into standardized NTEE and
#' NTEEV2 codes with associated definitions and classifications.
#'
#' @param bmf data.table containing BMF data with an EIN column and the
#'   NTEE code column specified by \code{input_ntee_col}.
#' @param ntee_code_lookup data.table lookup with columns:
#'   \code{ntee_code}, \code{naics_code}, \code{ntee_code_definition}.
#' @param ntee_major_group_lookup data.table lookup with columns:
#'   \code{ntee_code_first_character}, \code{ntee_code_major_group}.
#' @param activity_code_lookup data.table lookup with columns:
#'   \code{ntee_activity_code}, \code{ntee_activity_code_definition}.
#' @param input_ntee_col character name of the column containing raw NTEE
#'   codes (default: "NTEE_CD").
#' @param year character year for the SCD table's effective_year column.
#' @param path character file path for writing the SCD table. If NULL or
#'   empty, the SCD table is not written to disk.
#' @param write_scd logical whether to write the SCD table to file.
#'   Default TRUE for backward compatibility.
#'
#' @return A data.table with transformed NTEE columns added:
#'   \code{ntee_code_raw}, \code{ntee_code_clean}, \code{ntee_code_definition},
#'   \code{ntee_code_major_group}, \code{naics_code}, \code{ntee_activity_code},
#'   \code{ntee_activity_code_definition}, \code{nteev2_code},
#'   \code{nteev2_subsector}, \code{nteev2_org_type}, \code{nteev2}.
#'
#' @details
#' Transformation steps:
#' 1. Validates all input lookup tables and columns
#' 2. Cleans and standardizes raw NTEE codes to 3-character format
#' 3. Marks invalid/empty codes as "INVALID" or "UNDEFINED"
#' 4. Joins lookup tables for definitions and classifications
#' 5. Derives NTEEV2 codes, subsectors, and organization types
#' 6. Optionally creates and writes an SCD table
#' 7. Prints a quality report of the transformation results
#'
#' @examples
#' \dontrun{
#' bmf_transformed <- transform_ntee_code(
#'   bmf_raw,
#'   ntee_code_lookup = lookup_ls$ntee_code,
#'   ntee_major_group_lookup = lookup_ls$ntee_code_major_group,
#'   activity_code_lookup = lookup_ls$ntee_code_activity_type,
#'   input_ntee_col = "NTEE_CD",
#'   year = "2025",
#'   path = "data/output/ntee_scd.csv"
#' )
#' }
#'
#' @export
transform_ntee_code <- function(bmf,
                                ntee_code_lookup,
                                ntee_major_group_lookup,
                                activity_code_lookup,
                                input_ntee_col = "NTEE_CD",
                                year,
                                path = NULL,
                                write_scd = TRUE) {

  # ---------------------------------------------------------------------------
  # Input Validation
  # ---------------------------------------------------------------------------
  .validate_ntee_lookups(ntee_code_lookup,
                         ntee_major_group_lookup,
                         activity_code_lookup)
  .validate_ntee_input_column(bmf, input_ntee_col)

  # ---------------------------------------------------------------------------
  # Safe Copy (pure function pattern)
  # ---------------------------------------------------------------------------
  dt <- data.table::copy(bmf)

  # Build valid codes set from lookup
  valid_ntee_codes <- c(NTEE_INVALID, NTEE_UNDEFINED, ntee_code_lookup$ntee_code)

  # ---------------------------------------------------------------------------
  # Raw Code Preparation
  # ---------------------------------------------------------------------------
  dt[, ntee_code_raw := as.character(get(input_ntee_col))]
  dt[, ntee_code_raw := toupper(trimws(ntee_code_raw))]

  # Validate raw code lengths
  .ntee_raw_validation(dt)

  # ---------------------------------------------------------------------------
  # Helper Columns for Transformation Logic
  # ---------------------------------------------------------------------------
  dt[, `:=`(
    .len = nchar(ntee_code_raw),
    .first = substr(ntee_code_raw, 1, 1),
    .char23 = substr(ntee_code_raw, 2, 3)
  )]
  dt[, `:=`(
    .last = substr(ntee_code_raw, .len, .len),
    .int23 = suppressWarnings(as.integer(.char23))
  )]

  # ---------------------------------------------------------------------------
  # NTEE Code Standardization
  # ---------------------------------------------------------------------------
  dt[, ntee_code_clean := data.table::fcase(
    .len == 0,                          NTEE_UNDEFINED,
    !grepl("[A-Z]", .first),            NTEE_INVALID,
    .len == 1,                          paste0(ntee_code_raw, "00"),
    .len == 2,                          paste0(ntee_code_raw, "0"),
    .len == 3,                          ntee_code_raw,
    .len == 4 & grepl("[A-Z]", .last),  substr(ntee_code_raw, 1, 3),
    .len == 4 & grepl("[0-9]", .last),  paste0(.first, .last, "0"),
    default = NTEE_INVALID
  )]

  # Mark codes not in lookup as invalid
  dt[!ntee_code_clean %chin% valid_ntee_codes,
     ntee_code_clean := NTEE_INVALID]

  # ---------------------------------------------------------------------------
  # Activity Code Extraction
  # ---------------------------------------------------------------------------
  dt[, ntee_activity_code := data.table::fcase(
    .len == 4 & grepl("[A-Z]", .last),  paste0(.last, "00"),
    .len == 4 & grepl("[0-9]", .last),  substr(ntee_code_raw, 2, 3),
    default = NTEE_UNDEFINED
  )]

  # ---------------------------------------------------------------------------
  # Lookup Joins
  # ---------------------------------------------------------------------------

  # Activity code definition - primary lookup
  dt[activity_code_lookup,
     ntee_activity_code_definition := i.ntee_activity_code_definition,
     on = .(ntee_activity_code)]

  # Activity code definition - fallback to ntee_code_lookup for secondary codes
  dt[is.na(ntee_activity_code_definition),
     ntee_activity_code_definition := ntee_code_lookup$ntee_code_definition[
       match(ntee_activity_code, ntee_code_lookup$ntee_code)
     ]]

  # Major group lookup (using .first helper column)
  dt[ntee_major_group_lookup,
     ntee_code_major_group := i.ntee_code_major_group,
     on = .(.first = ntee_code_first_character)]

  # NTEE code definition and NAICS lookup
  dt[ntee_code_lookup,
     `:=`(naics_code = i.naics_code,
          ntee_code_definition = i.ntee_code_definition),
     on = .(ntee_code_clean = ntee_code)]

  # ---------------------------------------------------------------------------
  # Handle Invalid/Undefined Codes
  # ---------------------------------------------------------------------------
  dt[ntee_code_clean %chin% c(NTEE_UNDEFINED, NTEE_INVALID),
     (COLS_TO_FIX) := NTEE_UNDEFINED]

  # ---------------------------------------------------------------------------
  # NTEEV2 Transformation
  # ---------------------------------------------------------------------------
  dt <- .nteev2_code_transform(dt)

  # ---------------------------------------------------------------------------
  # Cleanup Helper Columns
  # ---------------------------------------------------------------------------
  dt[, (HELPER_COLUMNS) := NULL]

  # ---------------------------------------------------------------------------
  # SCD Table Creation and Output
  # ---------------------------------------------------------------------------
  scd <- .build_ntee_scd_table(dt, year)

  if (write_scd && !is.null(path)) {
    .write_ntee_scd_table(scd, path)
  }

  .ntee_output_validation(scd)

  return(dt)
}

# ============================================================================
# Helper Functions
# ============================================================================

.ntee_raw_validation <- function(dt) {
  ntee_nchar_tbl <- table(nchar(dt[, ntee_code_raw]))

  if (any(as.integer(names(ntee_nchar_tbl)) > 4)) {
    stop("NTEE codes with more than 4 characters detected. ",
         "Distribution: ",
         paste(names(ntee_nchar_tbl), ntee_nchar_tbl,
               sep = "=", collapse = ", "))
  }

  message("NTEE Code Character Length Distribution:")
  print(ntee_nchar_tbl)

  invisible(NULL)
}

.nteev2_code_transform <- function(dt) {
  # NTEEV2 Code
  dt[, nteev2_code := data.table::fcase(
    .len == 3 & .int23 <= 19, paste0(.first, "00"),
    .len == 4 & .int23 <= 19, paste0(.first, .last, "0"),
    default = ntee_code_clean
  )]

  # NTEEV2 Subsector
  dt[, nteev2_subsector := data.table::fcase(
    nteev2_code %chin% NTEEV2_SUBSECTOR_UNIVERSITY, "UNI",
    nteev2_code %chin% NTEEV2_SUBSECTOR_HOSPITAL,   "HOS",
    .first == "A",                                   "ART",
    .first == "B",                                   "EDU",
    .first %chin% c("C", "D"),                       "ENV",
    .first %chin% c("E", "F", "G", "H"),             "HEL",
    .first %chin% LETTERS[9:16],                     "HMS",
    .first == "Q",                                   "IFA",
    .first %chin% c("R", "S", "T", "U", "V", "W"),   "PSB",
    .first == "X",                                   "REL",
    .first == "Y",                                   "MMB",
    .first == "Z",                                   "UNU",
    default = NTEE_UNDEFINED
  )]

  # NTEEV2 Organization Type
  dt[, nteev2_org_type := data.table::fcase(
    .char23 == "01", "AA",
    .char23 == "02", "MT",
    .char23 == "03", "PA",
    .char23 == "05", "RP",
    .char23 == "11", "MS",
    .char23 == "12", "MM",
    .char23 == "19", "NS",
    default = "RG"
  )]

  # Full NTEEV2 composite code
  dt[, nteev2 := paste(nteev2_subsector, nteev2_code, nteev2_org_type, sep = "-")]

  return(dt)
}

.ntee_output_validation <- function(scd) {
  total_rows <- nrow(scd)
  undefined_count <- nrow(scd[ntee_code_clean == NTEE_UNDEFINED])
  invalid_count <- nrow(scd[ntee_code_clean == NTEE_INVALID])
  valid_count <- total_rows - undefined_count - invalid_count

  message("--- NTEE Code Mapping Quality Report ---")
  message(sprintf("Total Records:    %s", format(total_rows, big.mark = ",")))
  message(sprintf("Valid Codes:      %s (%0.2f%%)",
                  format(valid_count, big.mark = ","),
                  (valid_count / total_rows) * 100))
  message(sprintf("Undefined Codes:  %s (%0.2f%%)",
                  format(undefined_count, big.mark = ","),
                  (undefined_count / total_rows) * 100))
  message(sprintf("Invalid Codes:    %s (%0.2f%%)",
                  format(invalid_count, big.mark = ","),
                  (invalid_count / total_rows) * 100))

  # Warn if data quality is concerning
  invalid_rate <- (invalid_count / total_rows) * 100
  if (invalid_rate > 5) {
    warning(sprintf("High invalid NTEE code rate: %.2f%%. Review input data.",
                    invalid_rate))
  }

  invisible(NULL)
}
