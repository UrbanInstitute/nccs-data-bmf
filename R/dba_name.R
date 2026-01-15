# ============================================================================
# dba_name.R
# Transform SORT_NAME column to doing-business-as name
# ============================================================================

#' Transform DBA (Doing Business As) Name Column
#'
#' @description
#' Transforms the SORT_NAME column from raw BMF data into a cleaned
#' doing-business-as name. Most organizations do not have a secondary name,
#' so empty strings are converted to NA.
#'
#' @param dt data.table containing BMF data with SORT_NAME column
#' @param input_col character name of input column (default: "SORT_NAME")
#'
#' @return data.table with new columns:
#'   \itemize{
#'     \item dba_name_raw - Original SORT_NAME value (empty strings as NA)
#'     \item dba_name - Title-cased, whitespace-cleaned name
#'   }
#'
#' @examples
#' \dontrun{
#' bmf <- transform_dba_name(bmf_raw)
#' }
#'
#' @export
transform_dba_name <- function(dt, input_col = "SORT_NAME") {
  validate_data_table(dt, input_col, context = "BMF data")

  dt_safe <- data.table::copy(dt)

  # Preserve raw, convert "" to NA
  dt_safe[, dba_name_raw := as.character(get(input_col))]
  dt_safe[dba_name_raw == "", dba_name_raw := NA_character_]

  # Clean version: title case + whitespace cleanup
  dt_safe[, dba_name := stringr::str_to_title(stringr::str_squish(dba_name_raw))]

  # Quality report
  dba_count <- dt_safe[!is.na(dba_name), .N]
  message(sprintf(
    "DBA names: %s of %s (%0.1f%%) have secondary names",
    format(dba_count, big.mark = ","),
    format(nrow(dt_safe), big.mark = ","),
    100 * dba_count / nrow(dt_safe)
  ))

  return(dt_safe)
}
