#' Uniquely identify parcels and parcel owners
#' @description Takes a data frame of parcels and owners and grants unique identifiers
#' @details this function is designed on a one-to-one mapping from
#' a parcel and a single owner. It should still provide similar results on
#' one-to-many mappings but it is yet untested.
#' @param parcels A data frame
#' @param parcel_id The column containing the parcel id
#' @param parcel_year The column containing the parcel year
#' @param owner_name The column containing the owner name
#' @param owner_address The column containing the owner street address
#' @param owner_city The column containing the owner city
#' @param ... Any other columns to include, can be renamed through this interface
#' @returns A data frame with unique identifiers

identify_parcels <- function(parcels, parcel_id, parcel_year, owner_name, owner_address, owner_city, ...) {
  parcels <- parcels %>%
    select(parcel_id = {{parcel_id}},
           parcel_year = {{parcel_year}},
           owner_name = {{owner_name}},
           owner_address = {{owner_address}},
           owner_city = {{owner_city}},
           ...) %>%
    mutate(owner_type = tag_owners(owner_name)) %>%
    mutate(owner_name_adj = ifelse(owner_type == "Other", paste(owner_name, str_extract(owner_address, "(?<=(^| ))[0-9]+(?= )")), owner_name))

  parcels <- parcels %>%
    rowwise() %>%
    mutate(
      py_id = rlang::hash(c(parcel_id, parcel_year)),
      owner_id = rlang::hash(c(owner_name_adj, owner_address)),
      agent = FALSE
    ) %>%
    ungroup() %>%
    tidyr::unite(owner_address_full, owner_address, owner_city, sep = ", ", na.rm = TRUE, remove = FALSE) %>%
    relocate(py_id, .before = parcel_id) %>%
    relocate(owner_id, .before = owner_address) %>%
    relocate(owner_address_full, .before = owner_address) %>%
    relocate(owner_name_adj, .after = owner_name) %>%
    relocate(owner_type, .after = owner_name_adj)

  return(parcels)
}
