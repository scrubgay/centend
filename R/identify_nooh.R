#' Identify not-owner-occupied status
#' @description Identifies single-family rentals and second homes. Outputs two
#' vectors, one a status of "Owner-occupied", "Not owner-occupied", and "Unknown",
#' and another as to why something was categorized accordingly.
#' @param data input data frame
#' @return a modified input data frame with two vectors

identify_nooh <- function(data,
                          componentId,
                          py_id, parcel_id
                          owner_address, owner_city, owner_state, owner_zip,
                          physical_address, physical_city, physical_zip,
                          homestead, n_units,
                          records,
                          .state_strings = c("FL", "FLORIDA", "FLA"),
                          .city_strings) {
  corporate_components <- corporate_components(records)
  n_properties <- component_summary(records) %>%
    select(contains("component"), Property)

  # create a regex that finds the components of the physical address in the owner address
  data <- data %>%
    rowwise() %>%
    mutate(search = any(stringi::stri_detect(
      {{owner_address}},
      regex = paste0(
        "(^| )",
        str_split_1({{physical_address}}, " ") %>%
          .[str_length(.) > 4],
        paste0("( |$)")
        )
      ))) %>%
    ungroup()

  # join the results from above
  data <- data %>%
    left_join(corporate_components) %>%
    left_join(n_properties)

  # the workflow
  data <- data %>%
    mutate(owner_address = str_remove_all({{owner_address}}, "[^0-9A-Z- /]"),
           # nooh = case_when(
           #   corporate == TRUE ~ TRUE,
           #   !({{owner_state}} %in% .state_strings) ~ TRUE,
           #   !({{owner_city}} %in% .city_strings) ~ TRUE, # to-do, what when owner_city is spelled differently? multi-county cases?
           #   !str_detect({{owner_address}}, "BOX") & {{owner_zip}} != {{physical_zip}} ~ TRUE,
           #   {{owner_address}} == {{physical_address}} |
           #     stringi::stri_detect({{owner_address}}, fixed = {{physical_address}}) |
           #     stringi::stri_detect({{physical_address}}, fixed = {{owner_address}}) ~ FALSE,
           #   {{homestead}} == TRUE ~ FALSE,
           #   stringi::stri_detect({{owner_address}}, regex = str_extract({{physical_address}], "^(\\d)+")) ~ FALSE, #matches string address
           #   Property > 4 ~ TRUE, #to-do, property types for besides single-family
           #   str_detect({{owner_address}}, "(PO |P O |PO)BOX") ~ NA,
           #   stringdist::stringdist({{owner_address}}, {{physical_address}}) > 7 ~ TRUE,
           #   search == TRUE ~ NA,
           #   .default = NA
           # ),
           nooh_type = case_when(
             corporate == TRUE ~ "NOOH;Corporate-owned",
             !({{owner_state}} %in% .state_strings) ~ "NOOH;Out-of-state",
             !({{owner_city}} %in% .city_strings) ~ "NOOH;Out-of-city", # to-do, what when owner_city is spelled differently? multi-county cases?
             !str_detect({{owner_address}}, "BOX") & {{owner_zip}} != {{physical_zip}} ~ "NOOH;Non-matching ZIP, not a PO Box",
             {{owner_address}} == {{physical_address}} |
               stringi::stri_detect({{owner_address}}, fixed = {{physical_address}}) |
               stringi::stri_detect({{physical_address}}, fixed = {{owner_address}}) ~ "OOH;Similar addresses",
             {{homestead}} == TRUE ~ "OOH;Homestead status and not corporate",
             stringi::stri_detect({{owner_address}}, regex = str_extract({{physical_address}], "^(\\d)+")) ~ "OOH;House numbers match", #matches string address
             Property > 4 ~ "NOOH;Multiple properties owned", #to-do, property types for besides single-family
             str_detect({{owner_address}}, "(PO |P O |PO)BOX") ~ "Unknown;PO Box, unable to determine",
             stringdist::stringdist({{owner_address}}, {{physical_address}}) > 7 ~ "NOOH;Strings dissimilar",
             search == TRUE ~ "Unknown;Unable to determine",
             .default = "Unknown;Unable to determine, all other cases"
           ),
    )

  return(data)
}
