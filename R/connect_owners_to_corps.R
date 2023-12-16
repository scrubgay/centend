#' Match owner names to corporate filings
#' @description Matches a vector of owner names to corporate filings
#' @details Using a table of corporate filings, finds corporate filings that
#' match strings with owner names
#' @param parcels an identified parcel dataset with an owner_name_adj column
#' @param filings a data frame consisting of corporate filings
#' @param id a unique identifying column within filings
#' @param name the name column in filings
#' @returns a crosswalk data frame of owner_names to corporate filings, may be one-to-many

connect_owners_to_corps <- function(parcels, filings, id, name) {
  owner_names <- parcels %>%
    filter(owner_type == "Corporate") %>%
    distinct(owner_name_adj) %>%
    data.table::setDT(key = "owner_name_adj")

  trunc = max(str_length(owner_names$owner_name_adj), na.rm=TRUE)

  filings <- filings %>%
    select(corp_id = {{id}},
           corp_name = {{name}}) %>%
    mutate(corp_name_trunc = str_sub(corp_name, end=trunc) %>% str_trim()) %>%
    data.table::setDT(key = "corp_name_trunc")

  owner_names <- merge(owner_names, filings, by.x = "owner_name_adj", by.y = "corp_name_trunc")
  owner_names <- data.table::setDF(owner_names)

  return(owner_names)
}

# connect_owners_to_corps <- function(owner_names, filings, id, name) {
#   filings <- filings %>%
#     select(corp_id = {{id}},
#            corp_name = {{name}}) %>%
#     group_by(str_extract(corp_name, "^...."))
#
#   group_keys <- group_keys(filings) %>% .[[1]]
#
#   filings <- filings %>%
#     group_split() %>%
#     `names<-`(group_keys)
#
#   owner_names <- unique(owner_names)
#
#   xwalk <- map_dfr(owner_names, \(owner) {
#     first_chars <- str_extract(owner, "^....")
#     if (!(first_chars %in% names(filings))) {return()}
#     filings_sub <- filings[[first_chars]]
#
#     filings_sub <- filings_sub %>%
#       filter(str_detect(corp_name, paste0("^", owner))) %>%
#       mutate(owner_name = owner) %>%
#       select(-matches("str_extrac")) %>%
#       relocate(owner_name)
#
#     return(filings_sub)
#   }, .progress = TRUE)
#
#   return(xwalk)
# }
