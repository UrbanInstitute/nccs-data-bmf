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
  vapply(names(dt), function(col) {
    x <- dt[[col]]
    if (is.character(x)) sum(!is.na(x) & x != "") else sum(!is.na(x))
  }, integer(1))
}
