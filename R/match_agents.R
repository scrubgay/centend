#' Match agents in addresses
#' @description Takes output from \code{identify_parcels} and matches against
#' a table of registered agent addresses. If an owner address matches
#' a registered agent address, return true
#' @details Every column provided in ... must provide true when
#' @param parcels a tagged output from \code{identify_parcels}
#' @param agents a table of registered agent addresses
#' @param address the address column in agents
#' @param city the city column in agents
#' @returns parcels with a modified agent column

match_agents <- function(parcels, agents, address, city) {
  agents <- agents %>%
    mutate(agent = TRUE) %>%
    select(owner_address = {{address}},
           owner_city = {{city}},
           agent) %>%
    data.table::setDT(agents, key = c("owner_address", "owner_city"))

  parcels <- data.table::setDT(parcels, key = c("owner_address", "owner_city")) %>%
    merge(agents, all.x=TRUE)

  parcels <- data.table::setDF(parcels) %>%
    mutate(agent = ifelse(is.na(agent), FALSE, TRUE))# %>%
    #relocate(owner_address, owner_city, .before = owner_address_full)

  return(parcels)
}

# match_agents <- function(parcels, agents, address, city) {
#   # this method is for dplyr methods
#   agents <- agents %>%
#     group_by(str_extract({{address}}, "^...."))
#
#   group_keys <- group_keys(agents) %>% .[[1]]
#
#   agents <- agents %>%
#     group_split() %>%
#     `names<-`(group_keys)
#
#   message("Matching agents...")
#
#   find_match <- function(addr, cit) {
#       first_chars <- str_extract(addr, "^....")
#       if (!(first_chars %in% names(agents))) {return(FALSE)}
#
#       agents_sub <- agents[[first_chars]] %>%
#         filter({{address}} == addr,
#                {{city}} == cit)
#
#       return(nrow(agents_sub) > 0)
#   }
#
#   parcels <- parcels %>%
#     mutate(agent = map2_lgl(
#       owner_address, owner_city,
#       \(addr, cit) {
#         first_chars <- str_extract(addr, "^....")
#         if (!(first_chars %in% names(agents))) {return(FALSE)}
#
#         agents_sub <- agents[[first_chars]] %>%
#           filter({{address}} == addr,
#                  {{city}} == cit)
#
#         return(nrow(agents_sub) > 0)
#       }, .progress = TRUE))
#
#   return(parcels)
#
# }
#
