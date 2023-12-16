#' Identify not-owner-occupied status
#' @description Identifies single-family rentals and second homes. Outputs two
#' vectors, one a status of "Owner-occupied", "Not owner-occupied", and "Unknown",
#' and another as to why something was categorized accordingly.
#' @param data input data frame
#' @return a modified input data frame with two vectors

# Needs stringi dependency fixed
identify_nooh <- function(data,
                          owner_address, owner_city, owner_state, owner_zip,
                          physical_address, physical_zip,
                          homestead, n_units,
                          records,
                          .state_strings = c("FL", "FLORIDA", "FLA"),
                          .city_strings) {

  corporate_components <- corporate_components(records)
  n_properties <- component_summary(records) %>%
    select(contains("component"), Property)

  data <- data %>%
    left_join(corporate_components) %>%
    left_join(n_properties)

  data <- data %>%
    mutate(owner_address = str_remove_all({{owner_address}}, "[^0-9A-Z- /]"),
           nooh_type = case_when(
             corporate == TRUE ~ "NOOH;Corporate-owned",
             {{n_units}} > 4 ~ "NOOH;Large multifamily",
             !({{owner_state}} %in% .state_strings) ~ "NOOH;Out-of-state",
             !str_detect({{owner_address}}, "BOX") & {{owner_zip}} != {{physical_zip}} ~ "NOOH;Non-matching ZIP, not a PO Box",
             is.na({{owner_address}}) | is.na({{physical_address}}) ~ "Unknown;Empty owner or physical address",
             str_detect({{owner_address}}, {{physical_address}}) |
             str_detect({{physical_address}}, {{owner_address}}) ~ "OOH;Similar addresses",
             {{homestead}} == TRUE ~ "OOH;Homestead status and not corporate",
             str_detect({{owner_address}}, str_extract({{physical_address}}, "^(\\d)+")) ~ "OOH;House numbers match", #matches string address
             Property > 4 ~ "NOOH;Multiple properties owned", #to-do, property types for besides single-family
             str_detect({{owner_address}}, "(PO |P O |PO)BOX") ~ "Unknown;PO Box, unable to determine",
             stringdist::stringdist({{owner_address}}, {{physical_address}}) > 7 ~ "NOOH;Strings dissimilar",
             !({{owner_city}} %in% .city_strings) ~ "NOOH;Out-of-city",
             .default = "Unknown;Unable to determine, all other cases"
           )
    ) %>%
    tidyr::separate_wider_delim(nooh_type, delim = ";", names = c("nooh_type", "nooh_reason"))

  return(data)
}

# # needs a huge rewrite
# identify_nooh <- function(data,
#                           componentId,
#                           py_id, parcel_id,
#                           owner_address, owner_city, owner_state, owner_zip,
#                           physical_address, physical_zip,
#                           homestead, n_units,
#                           records,
#                           .state_strings = c("FL", "FLORIDA", "FLA"),
#                           .city_strings) {
#   corporate_components <- corporate_components(records)
#   n_properties <- component_summary(records) %>%
#     select(contains("component"), Property)
#
#   # create a regex that finds the components of the physical address in the owner address
#   data <- data %>%
#     rowwise() %>%
#     mutate(search = ifelse(
#       is.na({{owner_address}})|is.na({{physical_address}}),
#       NA,
#       any(stringi::stri_detect(
#         {{owner_address}},
#         regex = paste0(
#           "(^| )",
#           str_split_1({{physical_address}}, " ") %>%
#             .[str_length(.) > 4],
#           paste0("( |$)")
#         )
#       ))
#       )) %>%
#     ungroup()
#
#   # join the results from above
#   data <- data %>%
#     left_join(corporate_components) %>%
#     left_join(n_properties)
#
#   # the workflow
#   data <- data %>%
#     mutate(owner_address = str_remove_all({{owner_address}}, "[^0-9A-Z- /]"),
#            nooh_type = case_when(
#              corporate == TRUE ~ "NOOH;Corporate-owned",
#              {{n_units}} > 4 ~ "NOOH;Large multifamily",
#              !({{owner_state}} %in% .state_strings) ~ "NOOH;Out-of-state",
#              !str_detect({{owner_address}}, "BOX") & {{owner_zip}} != {{physical_zip}} ~ "NOOH;Non-matching ZIP, not a PO Box",
#              is.na({{owner_address}}) | is.na({{physical_address}}) ~ "Unknown;Empty owner or physical address",
#              {{owner_address}} == {{physical_address}} |
#                stringi::stri_detect({{owner_address}}, fixed = {{physical_address}}) |
#                stringi::stri_detect({{physical_address}}, fixed = {{owner_address}}) ~ "OOH;Similar addresses",
#              {{homestead}} == TRUE ~ "OOH;Homestead status and not corporate",
#              stringi::stri_detect({{owner_address}}, regex = str_extract({{physical_address}}, "^(\\d)+")) ~ "OOH;House numbers match", #matches string address
#              Property > 4 ~ "NOOH;Multiple properties owned", #to-do, property types for besides single-family
#              str_detect({{owner_address}}, "(PO |P O |PO)BOX") ~ "Unknown;PO Box, unable to determine",
#              stringdist::stringdist({{owner_address}}, {{physical_address}}) > 7 ~ "NOOH;Strings dissimilar",
#              !({{owner_city}} %in% .city_strings) ~ "NOOH;Out-of-city", # to-do, what when owner_city is spelled differently? multi-county cases?             search == TRUE ~ "Unknown;Unable to determine",
#              .default = "Unknown;Unable to determine, all other cases"
#            )
#     ) %>%
#     select(-search)
#
#   return(data)
# }
