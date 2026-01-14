# ============================================================================
# group_exemption_number.R
# Transform GROUP column (Group Exemption Number - GEN)
# ============================================================================

# ============================================================================
# Module Constants
# ============================================================================

# GEN is a 4-digit number (0000-9999)
GEN_LENGTH <- 4

# Value indicating no group exemption
GEN_ABSENT_VALUE <- "0"

# Output column names
GEN_OUTPUT_COLS <- c(
  "group_exemption_number_raw",
  "group_exemption_number",
  "group_exemption_is_member"
)

# ============================================================================
# Transformation Function
# ============================================================================

#' Transform Group Exemption Number
#'
#' @description
#' Transforms the GROUP column from raw BMF data. The Group Exemption Number
#' (GEN) is a 4-digit number assigned by the IRS to a central organization
#' that has obtained a group exemption letter. Organizations with GEN = 0
#' are independent (not part of a group exemption).
#'
#' @param dt data.table containing BMF data with GROUP column
#' @param input_col character name of input column (default: "GROUP")
#'
#' @return data.table with new columns:
#'   \itemize{
#'     \item group_exemption_number_raw - Original GROUP value
#'     \item group_exemption_number - Padded 4-digit GEN
#'     \item group_exemption_is_member - Logical flag: TRUE if part of group exemption
#'   }
#' 
#' @note
#' BMF pipeline function. Modifies input in place for efficiency. Caller should pass a copy if original must be preserved.
#' 
#' @examples
#' \dontrun{
#' bmf_transformed <- transform_group_exemption_number(bmf_raw)
#' }
#'
#' @export
transform_bmf_group_exemption_number <- function(dt, input_col = "GROUP") {

  # Input validation
  validate_data_table(dt, input_col, context = "BMF data")

  # Preserve raw value
  dt[, group_exemption_number_raw := as.character(get(input_col))]

  # Pad to 4 digits for consistent formatting
  dt[, group_exemption_number := stringr::str_pad(
    group_exemption_number_raw,
    width = GEN_LENGTH,
    side = "left",
    pad = "0"
  )]

  # Flag: is this org part of a group exemption?
  # GEN = 0 means independent (not a group member)
  dt[, group_exemption_is_member := data.table::fifelse(
    group_exemption_number_raw == GEN_ABSENT_VALUE |
      is.na(group_exemption_number_raw) |
      group_exemption_number_raw == "",
    FALSE,
    TRUE
  )]

  # Quality report
  member_count <- dt[group_exemption_is_member == TRUE, .N]
  total_rows <- nrow(dt)
  message(sprintf(
    "Group exemption: %s of %s (%0.1f%%) are group exemption members",
    format(member_count, big.mark = ","),
    format(total_rows, big.mark = ","),
    100 * member_count / total_rows
  ))

  # Unique GEN count (excluding 0)
  unique_gens <- dt[group_exemption_is_member == TRUE,
                         uniqueN(group_exemption_number)]
  if (unique_gens > 0) {
    message(sprintf(
      "Group exemption: %s unique group exemption numbers found",
      format(unique_gens, big.mark = ",")
    ))
  }

  return(dt)
}
