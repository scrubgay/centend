#' Cypher templates
#'
#' Create Cypher import and export scripts using glue scripts. Each script has
#' specific parameters to fill in.

read_cypher <- function(file) {
  readLines(file) %>%
    .[!str_detect(., "^//")] %>% #remove n
    paste(collapse = "\n")
}

write_cypher <- function(cql, file) {
  writeLines(cql, file)
}

create_cypher_import <- function(parcel_file="./parcels.csv", filings_file="./filings.csv", officers_file="officers.csv") {
  if (!all(c(parcel_file, filings_file, officers_file) %>% str_detect(".csv$"))) {
    stop("Error: all file names must end in '.csv'")
  }

  read_cypher("./cypher_templates/importer.cql") %>% glue::glue()
}
