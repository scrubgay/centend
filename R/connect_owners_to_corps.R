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
    select(corp_id = {{id}}, corp_name = {{name}})
    group_by(str_extract(corp_name, "^.."))

  group_keys <- group_keys(filings) %>% .[[1]]

  filings <- filings %>%
    group_split() %>%
    `names<-`(group_keys)

  owner_names <- unique(owner_names)

  xwalk <- map_dfr(owner_names, \(owner) {
    first_two <- str_extract(owner, "^..")
    filings_sub <- filings[[first_two]]

    filings_sub <- filings_sub %>%
      filter(stringi::stri_detect_fixed(owner, corp_name) |
               stringi::stri_detect_fixed(corp_name, owner)) %>%
      mutate(owner_name = owner) %>%
      relocate(owner_name)

    return(filings_sub)
  })

  return(xwalk)
}
