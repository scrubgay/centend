#' Match owner names to corporate filings
#' @description Matches a vector of owner names to corporate filings
#' @details Using a table of corporate filings, finds corporate filings that
#' match strings with owner names
#' @param owner_names a vector of owner names
#' @param filings a data frame consisting of corporate filings
#' @param id a unique identifying column within filings
#' @param name the name column in filings
#' @returns a crosswalk data frame of owner_names to corporate filings, may be one-to-many

connect_owners_to_corps <- function(owner_names, filings, id, name) {
  filings <- filings %>%
    select(corp_id = {{id}},
           corp_name = {{name}}) %>%
    group_by(str_extract(corp_name, "^...."))

  group_keys <- group_keys(filings) %>% .[[1]]

  filings <- filings %>%
    group_split() %>%
    `names<-`(group_keys)

  owner_names <- unique(owner_names)

  xwalk <- map_dfr(owner_names, \(owner) {
    first_chars <- str_extract(owner, "^....")
    if (!(first_chars %in% names(filings))) {return()}
    filings_sub <- filings[[first_chars]]

    filings_sub <- filings_sub %>%
      filter(str_detect(corp_name, paste0("^", owner))) %>%
      mutate(owner_name = owner) %>%
      select(-matches("str_extrac")) %>%
      relocate(owner_name)

    return(filings_sub)
  }, .progress = TRUE)

  return(xwalk)
}
