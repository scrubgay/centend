#' Create Cypher queries
#'
#' Using glue templates, create queries for working with Neo4j.
#'

read_cypher <- function(path) {
  system.file(path, package = "centend")
  file <- readLines(path)
  file <- Filter(\(str) !str_detect(str, "^//") & !(str_length(str) < 2), file)
  file <- paste0(file, collapse = "\n")
  return(file)
}

cypher_import <- function(parcels_file = "parcels.csv", filings_file = "filings.csv", officers_file = "officers.csv", output_path = NULL) {
  file <- read_cypher("./cypher/import.cql")

  query <- glue::glue(file, .open = "<<", .close = ">>")

  if (is.character(output_path)) {writeLines(query, output_path)}

  return(invisible(query))
}

cypher_wcc <- function(years, output_prefix, output_path = NULL) {
  file <- read_cypher("./cypher/wcc.cql")

  queries <- lapply(years, \(year) {
    outputName = paste0(output_prefix, year)
    query <- glue::glue(file, .open = "<<", .close = ">>")
  }) %>% as.character()

  queries <- paste0(queries, collapse = "\n\n")

  if (is.character(output_path)) {writeLines(queries, output_path)}

  return(invisible(queries))
}
