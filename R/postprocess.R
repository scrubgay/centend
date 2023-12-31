#' Postprocessing functions
#' @aliases load_records tagged_properties component_summary
#' @description These are functions to operate on a json export result of a
#' Weakly-Connected-Components result from Neo4j. Really just some shortcuts.

#' @describeIn load_records Load records
load_records <- function(path, .apoc = FALSE) {
  if (.apoc) {
    records <- jsonlite::stream_in(file(path)) %>%
      jsonlite::flatten() %>%
      as.data.frame() %>%
      select(-node.type, -node.id)
  }
  else {
    records <- jsonlite::fromJSON(path, flatten = TRUE)
  }
  records <- records %>%
    rename_with(~ str_remove(.x, "^node.") %>% str_remove("^properties.")) %>%
    mutate(labels = as.character(labels))

  return(records)
}

#' @describeIn load_records Tag properties
tagged_properties <- function(records) {
  records %>%
    filter(labels == "Property") %>%
    select(where(~ any(!is.na(.x))), -labels)
}

#' @describeIn load_records Summarize results
component_summary <- function(records) {
  records %>%
    count(componentId, labels) %>%
    tidyr::pivot_wider(names_from = labels, values_from = n, values_fill = 0) %>%
    filter(!is.na(Property))
}

#' @describeIn load_records Get identities
who_is <- function(records, component) {
  labels <- unique(records$labels)

  lapply(labels, \(label) {
    records %>%
      filter(componentId == component,
             labels == label) %>%
      select(where(~ all(!is.na(.x))), -labels, -componentId)
  }) %>%
    `names<-`(labels)
}

#' @describeIn load_records Get corporate entities
corporate_components <- function(records) {
  records %>%
    filter(labels == "OwnerName") %>%
    group_by(componentId) %>%
    mutate(corporate = any(type == "Corporate")) %>%
    distinct(componentId, corporate)
}

