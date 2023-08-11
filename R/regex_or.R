#' Regex 'or'
#' @description Create a regex 'or' statement, with the option of bookending it as a word.
#' @param ... Strings for the or statement. Can be strings or regex expressions.
#' @param as_word Whether to return a regex or that's detectable only as an isolated word.
#' @return A regex expression
#' @examples
#' # example code
#' regex_or("Alachua", "Gainesville", "GAINESVILLE")
#' regex_or(chopper("Gainesville", "Gainesv"), chopper("Alachua County", "Alachua C"))
regex_or <- function(..., as_word = TRUE) {
  ors <- paste(..., sep = "|")
  if (!as_word) {ors <- paste0("(", ors, ")")}
  else {ors <- paste0("(^| )(", ors, ")($| )")}

  return(ors)
}
