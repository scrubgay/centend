#' Find officers of corporations
#' @description matches a vector of corporation ids to officers
#' @details This function assumes a normalized business registry where
#' officers are located in another table than the filings
#' @param corp_ids a vector of corporate ids,
#' @param officers a table of officers
#' @param join_id a column in officers corresponding to corp_ids
#' @param ... columns on which an officer can be uniquely identified
#' @returns a data frame with officers

match_corp_officers <- function(corp_ids, officers, join_id, ...) {
  officers <- officers %>%
    select(corp_id = {{join_id}},
           ...) %>%
    filter(corp_id %in% corp_ids) %>%
    collect()

  officers <- officers %>%
    unite(off_id, ..., na.rm = TRUE, remove = FALSE) %>%
    rowwise() %>%
    mutate(
      off_id = rlang::hash(off_id)
    ) %>%
    ungroup()

  return(officers)
}
