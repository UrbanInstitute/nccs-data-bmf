# column_counts.R
# Shared by pre_checks.R and post_checks.R (backlog Z9). Kept in its own file
# so that either of those can be sourced on its own.

#' Number of non-missing values in every column of a table
#'
#' A value is missing when it is NA, or an empty string for character columns.
#' @param dt data.table or data.frame
#' @return named integer vector, one entry per column
#' @export
count_nonempty_values <- function(dt) {
  purrr::map_int(dt, function(column) {

    is_filled <- !is.na(column)

    # A blank cell in these files is usually an empty string, not NA.
    if (is.character(column)) {
      is_filled <- is_filled & column != ""
    }

    sum(is_filled)
  })
}
