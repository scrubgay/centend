#' Visualize owner entity graphs
#'
#' Pass a vector of component ids and visualize connections between
#' names and addresses. Useful for debugging connection errors.
#'
#' @param id A single id. Multiple ids are supported but results don't look good.
#' @param records A records object from a wcc query.
#' @param output Name of output html file. Defaults to `./temp.html`
#' @param open Whether to automatically open in a browser. Defaults to TRUE.
#' @param ... Other parameters to pass to \link[networkD3]{simpleNetwork}.
#' @return The string to the output file, invisibly.

visualize_components <- function(id, records, output="./temp.html", open=TRUE, ...) {
  if (!(requireNamespace("networkD3", quietly=TRUE) & requireNamespace("htmlwidgets", quietly=TRUE))) {
    stop('"networkD3" and "htmlwidgets" must be installed to use this function')
  }

  owners <- lapply(id, \(id) who_is(records, id) %>% .$Owner %>% mutate(id = id)) %>%
    bind_rows() %>%
    mutate(address = paste(address, city)) %>%
    select(address, name)

  network <- networkD3::simpleNetwork(owners, zoom=T, ...)

  htmlwidgets::saveWidget(network, file=output)

  if (open == TRUE) {browseURL(output)}

  return(invisible(output))
}
