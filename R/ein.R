#' @title Function to clean and format EIN
clean_and_format_ein <- function(ein_vector) {
  clean_ein <- stringr::str_remove_all(ein_vector, "[^0-9]")
  padded_ein <- stringr::str_pad(clean_ein, width = 9, side = "left", pad = "0")
  formatted_ein <- stringr::str_replace(padded_ein, "^(..)(.*)$", "\\1-\\2")
  return(formatted_ein)
}