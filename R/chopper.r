#' Detectable regex substring expressions
#' @description
#' Creates a regex expression that detects a minimum truncated substring,
#' and additional letters up to the full string.
#' Useful for matching string columns where names might be truncated.
#' @param full_string The entire string
#' @param min_string The minimum string from which to be expanded
#' @return a regex expression
#' @examples
#' # example code
#' chopper("Jacksonville", "Jacksonv")
#' chopper("identifiable", "identifi")
chopper <- function(full_string, min_string) {
  if (!str_detect(full_string, paste0("^", min_string))) {
    stop("Error, ", min_string, " is not a substring of ", full_string)
  }
  chuleta <- str_remove(full_string, min_string)
  chicarron <- map_chr(0:str_length(chuleta), ~ str_trunc(chuleta, .x, "right", ellipsis = "")) %>%
    paste0(collapse = "|") %>%
    paste0(min_string, "(", ., ")")
  return(chicarron)
}
