#' Detectable regex substring expressions
#' @description
#' Creates a regex expression that detects a minimum truncated substring,
#' and additional letters up to the full string on either side.
#' Useful for matching string columns where names might be truncated.
#' @param full_string The entire string
#' @param start What position in the string to start the minimum truncate. Defaults to 1
#' @param end What position in the string terminates the minimum truncate. Defaults to the end of the string
#' @return a regex expression
#' @examples
#' # example code
#' chopper("Jacksonville", start=3, end=6)
#' chopper("identifiable", end=7)

chopper <- function(str, start=1, end=str_length(str)) {
  base <- str_sub(str, start=start, end=end)
  ends <- str_sub(str, start = end+1)
  heads <- str_sub(str, end = start-1)

  if (str_length(ends)>0) {
    ends <- sapply(0:str_length(ends),
                   \(i) str_sub(ends, end = i)) %>%
      paste0(collapse = "|") %>%
      paste0("(", ., ")")
  }

  if (str_length(heads)>0) {
    heads <- sapply(1:str_length(heads),
                    \(i) str_sub(heads, start = i)) %>%
      paste0(collapse = "|") %>%
      paste0("(", ., ")")
  }

  regx <- paste0(heads, base, ends)

  return(regx)
}

# chopper_deprecated <- function(full_string, min_string) {
#   if (!str_detect(full_string, paste0("^", min_string))) {
#     stop("Error, ", min_string, " is not a substring of ", full_string)
#   }
#   chuleta <- str_remove(full_string, min_string)
#   chicarron <- map_chr(0:str_length(chuleta), ~ str_trunc(chuleta, .x, "right", ellipsis = "")) %>%
#     paste0(collapse = "|") %>%
#     paste0(min_string, "(", ., ")")
#   return(chicarron)
# }
