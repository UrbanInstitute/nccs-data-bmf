# ============================================================================
# ico_name.R
# Transform ICO (In-Care-Of) name column
# ============================================================================

# ============================================================================
# Module Constants
# ============================================================================
#
# The ICO field contains names prefixed with "%" followed by a space
# and sometimes additional characters before the actual name.
ICO_PREFIX_PATTERN <- "^%"

# Placeholder values that should be treated as missing
ICO_PLACEHOLDER_PATTERN <- "^(NA|N/A|NONE|NO|UNKNOWN)$"

# Name suffixes and honorifics to keep uppercase after title case
ICO_UPPERCASE_SUFFIXES <- c("Jr", "Sr", "Ii", "Iii", "Iv", "Phd", "Esq", "Cpa", "Mba", "Md")
ICO_UPPERCASE_REPLACE <- c("JR", "SR", "II", "III", "IV", "PHD", "ESQ", "CPA", "MBA", "MD")

# Output column names
ICO_OUTPUT_COLS <- c(
  "in_care_of_name_raw",
  "in_care_of_name_clean",
  "in_care_of_name_provided"
)

# ============================================================================
# Transformation Function
# ============================================================================

#' Transform In-Care-Of Name
#'
#' @description
#' Transforms the ICO (In-Care-Of) column from raw BMF data. The ICO field
#' contains the name of a person or organization that handles mail for the
#' nonprofit, typically formatted with a "%" prefix.
#'
#' @param dt data.table containing BMF data with ICO column
#' @param input_col character name of input column (default: "ICO")
#'
#' @return data.table with new columns:
#'   \itemize{
#'     \item in_care_of_name_raw - Original ICO value
#'     \item in_care_of_name_clean - Cleaned and title-cased name
#'     \item in_care_of_name_provided - Logical flag indicating if ICO was provided
#'   }
#' 
#' @note
#' BMF pipeline function. Modifies input in place for efficiency. Caller should pass a copy if original must be preserved.
#' 
#' @examples
#' \dontrun{
#' bmf_transformed <- transform_ico_name(bmf_raw)
#' }
#'
#' @export
transform_bmf_ico_name <- function(dt, input_col = "ICO") {

  # Input validation
  validate_data_table(dt, input_col, context = "BMF data")

  # Preserve raw value (renamed for clarity)
  dt[, in_care_of_name_raw := as.character(get(input_col))]

  # Clean: remove % prefix pattern
  dt[, in_care_of_name_clean := stringr::str_remove(
    in_care_of_name_raw,
    ICO_PREFIX_PATTERN
  )]

  # Standardize: title case and squish whitespace
  dt[, in_care_of_name_clean := stringr::str_to_title(in_care_of_name_clean)]
  dt[, in_care_of_name_clean := stringr::str_squish(in_care_of_name_clean)]

  # Convert placeholder values to NA
  dt[stringr::str_detect(in_care_of_name_clean,
                         stringr::regex(ICO_PLACEHOLDER_PATTERN, ignore_case = TRUE)),
     in_care_of_name_clean := NA_character_]

  # Restore uppercase for name suffixes and honorifics
  for (i in seq_along(ICO_UPPERCASE_SUFFIXES)) {
    pattern <- paste0("\\b", ICO_UPPERCASE_SUFFIXES[i], "\\b")
    dt[, in_care_of_name_clean := stringr::str_replace_all(
      in_care_of_name_clean,
      stringr::regex(pattern, ignore_case = FALSE),
      ICO_UPPERCASE_REPLACE[i]
    )]
  }

  # Flag for quality assurance and metadata
  dt[, in_care_of_name_provided := data.table::fifelse(
    is.na(in_care_of_name_raw) | in_care_of_name_raw == "" | is.na(in_care_of_name_clean),
    FALSE,
    TRUE
  )]

  # Quality report
  provided_count <- dt[in_care_of_name_provided == TRUE, .N]
  total_rows <- nrow(dt)
  message(sprintf(
    "In-Care-Of name: %s of %s (%0.1f%%) have ICO names provided",
    format(provided_count, big.mark = ","),
    format(total_rows, big.mark = ","),
    100 * provided_count / total_rows
  ))

  return(dt)
}
