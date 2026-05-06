# ============================================================================
# activity_code.R
# Transform ACTIVITY column to activity codes with dimension table
# ============================================================================

# ============================================================================
# Module Constants
# ============================================================================

ACTIVITY_CODE_LOOKUP_COLS <- c(
  "activity_code",
  "activity_code_category",
  "activity_code_definition"
)

# Sentinel value for undefined activity codes
ACTIVITY_CODE_UNDEFINED <- "000"

# ============================================================================
# Lookup Table Loading
# ============================================================================

activity_code_lookup <- lookup_ls$activity_code[
  , 
  (ACTIVITY_CODE_LOOKUP_COLS) := lapply(.SD, as.character), 
  .SDcols = ACTIVITY_CODE_LOOKUP_COLS
]

# ============================================================================
# Helper Functions
# ============================================================================

#' Clean Activity Code Column
#'
#' @description
#' Cleans and pads the raw ACTIVITY column to ensure consistent 9-character format.
#' The ACTIVITY field contains up to 3 activity codes (3 characters each).
#'
#' @param dt data.table containing BMF data with ACTIVITY column
#' @param input_col character name of input column (default: "ACTIVITY")
#'
#' @return data.table with cleaned activity_code column (9 chars, left-padded)
#'
#' @noRd
.clean_activity_code <- function(dt, input_col = "ACTIVITY") {

  # Input validation
  validate_data_table(dt, input_col, context = "BMF data")

  # Safe copy
  dt_safe <- data.table::copy(dt)

  # Convert to character
  dt_safe[, activity_code := as.character(get(input_col))]

  # Validation: check for unexpected lengths. Skip when the column is
  # entirely NA (common in legacy files with no ACTIVITY data) — max() on
  # all-NA returns -Inf with a noisy warning that adds no signal.
  if (any(!is.na(dt_safe$activity_code))) {
    max_len <- max(nchar(dt_safe$activity_code), na.rm = TRUE)
    if (max_len > 9) {
      warning(sprintf(
        "Activity codes with > 9 characters found (max: %d); padding may be incorrect",
        max_len
      ))
    }
  }

  # Pad to 9 characters (3 codes x 3 chars each)
  dt_safe[, activity_code := stringr::str_pad(
    activity_code,
    width = 9,
    side = "left",
    pad = "0"
  )]

  return(dt_safe)
}

# ============================================================================
# Dimension Table Functions
# ============================================================================

#' Create Activity Code Dimension Table
#'
#' @description
#' Creates an SCD Type 2 dimension table from the ACTIVITY column.
#' The raw ACTIVITY field contains up to 3 activity codes concatenated.
#' This function unpivots them into a long-format dimension table with
#' one row per EIN-activity code combination.
#'
#' @param dt data.table containing BMF data with ACTIVITY column and ein
#' @param lookup data.table activity code lookup table
#' @param input_col character name of input column (default: "ACTIVITY")
#'
#' @return data.table with columns:
#'   \itemize{
#'     \item ein - Employer Identification Number
#'     \item activity_code - 3-character activity code
#'     \item activity_code_definition - Human-readable definition
#'     \item activity_code_category - Activity category
#'   }
#'
#' @examples
#' \dontrun{
#' dim_table <- create_activity_code_dim_table(bmf_raw, activity_code_lookup)
#' }
#'
#' @export
create_activity_code_dim_table <- function(dt,
                                           lookup = activity_code_lookup,
                                           input_col = "ACTIVITY") {

  # Input validation
  validate_data_table(dt, c("ein", input_col), context = "BMF data")
  validate_lookup(lookup, ACTIVITY_CODE_LOOKUP_COLS, "activity_code_lookup")

  # Clean activity code column
  dt_cleaned <- .clean_activity_code(dt, input_col)

  # Extract the 3 individual activity codes
  dt_cleaned[, `:=`(
    activity_code_1 = substr(activity_code, 1, 3),
    activity_code_2 = substr(activity_code, 4, 6),
    activity_code_3 = substr(activity_code, 7, 9)
  )]

  # Melt to long format (one row per activity code)
  dt_long <- data.table::melt(
    dt_cleaned,
    id.vars = "ein",
    measure.vars = c("activity_code_1", "activity_code_2", "activity_code_3"),
    value.name = "activity_code"
  )

  # Remove undefined activity codes (000)
  dt_long <- dt_long[activity_code != ACTIVITY_CODE_UNDEFINED]

  # Remove the variable column (not needed)
  dt_long[, variable := NULL]

  # Join with lookup table for definitions and categories
  # Using two separate joins because there's not a 1-1 correspondence
  dt_long[lookup,
          activity_code_definition := i.activity_code_definition,
          on = .(activity_code)]

  dt_long[lookup,
          activity_code_category := i.activity_code_category,
          on = .(activity_code)]

  # Validate join success
  validate_join_success(
    dt_long,
    key_col = "activity_code",
    result_col = "activity_code_definition",
    lookup_name = "activity_code_lookup"
  )

  return(dt_long)
}

# ============================================================================
# Main Transformation Function
# ============================================================================

#' Transform Activity Code
#'
#' @description
#' Transforms activity codes by joining the dimension table summary back to
#' the main BMF data. Adds concatenated strings of all activity code
#' definitions and categories for each organization.
#'
#' @param dt data.table containing BMF data with ein column
#' @param dim_table data.table activity code dimension table from
#'   \code{create_activity_code_dim_table()}
#'
#' @return data.table with new columns:
#'   \itemize{
#'     \item activity_code_definitions - Semicolon-separated definitions
#'     \item activity_code_categories - Semicolon-separated categories
#'   }
#'
#' @note
#' BMF pipeline function. Modifies input in place for efficiency. Caller should pass a copy if original must be preserved.
#'
#' @examples
#' \dontrun{
#' dim_table <- create_activity_code_dim_table(bmf_raw, activity_code_lookup)
#' bmf_transformed <- transform_activity_code(bmf_raw, dim_table)
#' }
#'
#' @export
transform_bmf_activity_code <- function(dt) {

  # Input validation
  validate_data_table(dt, "ein", context = "BMF data")
  dt <- .clean_activity_code(dt)
  
  dim_table <- create_activity_code_dim_table(
    bmf,
    lookup = activity_code_lookup
  )

  required_dim_cols <- c("ein", "activity_code_definition", "activity_code_category")
  validate_data_table(dim_table, required_dim_cols, context = "activity code dimension table")


  # Aggregate dimension table: concatenate all definitions and categories per EIN
  dim_summary <- dim_table[, .(
    activity_code_definitions = paste(activity_code_definition, collapse = "; "),
    activity_code_categories = paste(activity_code_category, collapse = "; ")
  ), by = .(ein)]

  # Join summary back to main table
  dt[dim_summary,
          `:=`(
            activity_code_definitions = i.activity_code_definitions,
            activity_code_categories = i.activity_code_categories
          ),
          on = .(ein)]

  return(dt)
}
