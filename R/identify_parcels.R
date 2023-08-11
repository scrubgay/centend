#' Uniquely identify parcels and parcel owners
#' @description Takes a data frame of parcels and owners and grants unique identifiers
#' @details this function is designed on a one-to-one mapping from
#' a parcel and a single owner. It should still provide similar results on
#' one-to-many mappings but it is yet untested.
#' @param parcels a data frame
#' @param parcel_id the column containing the parcel id
#' @param parcel_year the column containing the parcel year
#' @param owner_name the column containing the owner name
#' @param owner_address the column containing the owner address. Should be a single column
#' that corresponds to the irreducible owner address search string, e.g. a street
#' address and city already combined
#' based on hashing methods
#' @returns A data frame with unique identifiers

identify_parcels <- function(parcels, parcel_id, parcel_year, owner_name, owner_address) {
  parcels <- parcels %>%
    select(parcel_id = {{parcel_id}},
           parcel_year = {{parcel_year}},
           owner_name = {{owner_name}},
           owner_address = {{owner_address}})

  parcels <- parcels %>%
    rowwise() %>%
    mutate(
      py_id = rlang::hash(c(parcel_id, parcel_year)),
      owner_id = rlang::hash(c(owner_name, owner_address)),
      agent = FALSE
    ) %>%
    ungroup() %>%
    relocate(py_id, .before = parcel_id) %>%
    relocate(owner_id, .after = owner_address)

  return(parcels)
}
