#' Postprocessing functions
#' @aliases load_records tagged_properties component_summary
#' @description These are functions to operate on a json export result of a
#' Weakly-Connected-Components result from Neo4j. Really just some shortcuts.

#' @describeIn load_records Load records
load_records <- function(path) {
  records <- jsonlite::fromJSON(path, flatten = TRUE)
  records <- records %>%
    rename_with(~ str_remove(.x, "^node.") %>% str_remove("^properties.")) %>%
    mutate(labels = as.character(labels))

  return(records)
}

#' @describeIn load_records Tag properties
tagged_properties <- function(records) {
  records %>%
    filter(labels == "Property") %>%
    select(parcel_id, year, componentId)
}

#' @describeIn load_records Summarize results
component_summary <- function(records) {
  records %>%
    count(componentId, labels) %>%
    pivot_wider(names_from = labels, values_from = n) %>%
    filter(!is.na(Property))
}

#' @describeIn load_records Get identities
who_is <- function(records, componentId) {
  labels <- unique(records$labels) %>%
    .[. != "Owner"]

  map(labels, \(label) {
    records %>%
      filter(labels == label,
             componentId == componentId) %>%
      select(where(~ all(!is.na(.x))), -identity, -labels, -elementId, -componentId)
  }) %>%
    `names<-`(labels)
}
