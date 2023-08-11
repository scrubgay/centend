#' Match agents in addresses
#' @description takes output from \code{identify_parcels} and matches against
#' a table of registered agent addresses. If an owner address matches
#' a registered agent address, return true
#' @details Every column provided in ... must provide true when
#' @param parcels a tagged output from \code{identify_parcels}
#' @param agents a table of registered agent addresses
#' @param ... columns from agents to find in the parcel owner addresses
#'

match_agents <- function(parcels, agents, address, city) {
  agents <- agents %>%
    group_by(str_extract(address, "^.."))

  group_keys <- group_keys(agents) %>% .[[1]]

  agents <- agents %>%
    group_split() %>%
    `names<-`(group_keys)

  parcels <- parcels %>%
    mutate(agent = map_lgl(owner_address, \(addr) {
      first_two <- str_extract(addr, "^..")
      agents_sub <- agents[[first_two]]

      agents_sub <- agents_sub %>%
        mutate(match = stringi::stri_detect_fixed(addr, {{address}}) & stringi::stri_detect_fixed(addr, {{city}}))

      return(any(agents_sub$match))

    }))
}
