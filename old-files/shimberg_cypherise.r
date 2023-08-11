# cyphere.r
# scripts to transform data sets into form for cypher
# this is the shimberg version, using the shimberg data

devtools::load_all("../amassr/")
library(rlang)
library(fs)

naify_address <- \(x) {
  ifelse(
    word(x, 1) %>%
      str_remove("(:|;)") %>%
      str_detect(regex_or(
        "ATT", "ATTN", "ATTENTION", "C./O", "C.O",
        "C/", "C/0", "C/I", "C/JO", "C/O", "C\\O", "CO/")
      ),
    NA,
    x
  )
}

# FL tax parcels ----

cypherize_fl_parcels <- function(data) {
  parcels <- data %>%
    select(
      parcel_id,
      county,
      year,
      physical_address = address,
      physical_address2 = address2,
      physical_city = city_clean,
      physical_zip = zip,
      dor_use_code = dor_uc_text,
      homestead,
      owner_name,
      owner_address1 = owner_address,
      owner_address2,
      owner_city,
      owner_state,
      owner_zip
    ) %>%
    collect() %>%
    rowwise() %>%
    mutate(id = hash(paste0(parcel_id, county, year)),
           homestead = !is.na(homestead)) %>%
    ungroup() %>%
    relocate(id) %>%
    mutate(across(
      where(~ is.character(.x)),
      \(x)
      x %>%
        str_to_upper() %>%
        str_remove_all("[^0-9A-Z/\\- ]") %>%
        #str_remove_all("[^0-9A-Z/\\- ]") %>%
        str_squish()))
  
  owners <- parcels %>%
    select(contains("owner_")) %>%
    distinct() %>%
    mutate(
      owner_type = categorize_owner(owner_name),
      a1 = naify_address(owner_address1),
      a2 = naify_address(owner_address2)) %>%
    unite(owner_address, a1, a2, sep = ", ", na.rm = TRUE) %>%
    relocate(owner_address, .after = owner_name) %>%
    rowwise() %>%
    mutate(owner_id = hash(paste0(owner_name, owner_address, owner_city))) %>%
    ungroup() %>%
    relocate(owner_id) %>%
    relocate(owner_type, .after = owner_name)
  
  agents <- semi_join(
    owners,
    known_fl_agents_all,
    join_by(owner_address == REGISTERED_AGENT_ADDRESS,
            owner_city == REGISTERED_AGENT_CITY)) %>%
    select(owner_id)
  
  agents2 <- semi_join(
    owners,
    known_fl_agents_all,
    join_by(owner_address == REGISTERED_AGENT_ADDRESS,
            owner_city == REGISTERED_AGENT_CITY)) %>%
    select(owner_id)
  
  agents <- bind_rows(agents, agents2) %>%
    distinct() %>%
    mutate(ADDR_IS_AGENT = TRUE)
  
  owners <- left_join(owners, agents) %>%
    mutate(ADDR_IS_AGENT = ifelse(is.na(ADDR_IS_AGENT), FALSE, TRUE))
  
  owners <- owners %>%
    rowwise() %>%
    mutate(
      owner_address_id = hash(paste0(owner_address, owner_city)),
      owner_name_id = hash(owner_name)
    ) %>%
    ungroup()
  
  oAddr <- owners %>%
    select(id = owner_address_id, owner_address, owner_city, owner_state, owner_zip, ADDR_IS_AGENT) %>%
    distinct()
  
  oName <- owners %>%
    select(id = owner_name_id, owner_name, owner_type)

  parcels <- parcels %>%
    left_join(owners %>% select(-owner_address, owner_id, -ADDR_IS_AGENT)) %>%
    select(-contains("owner_"), owner_id)
  
  owners <- owners %>%
    select(id = owner_id, owner_name_id, owner_address_id)

  return(list(
    parcels = parcels,
    owners = owners,
    owner_address = oAddr,
    owner_name = oName
  ))
}

write_cypherized_parcels <- function(cypher, export_dir) {
  write_csv(cypher$parcels, path(export_dir, "parcels.csv"), na = "", quote = "all")
  write_csv(cypher$owners, path(export_dir, "owners_crosswalk.csv"), na = "", quote = "all")
  write_csv(cypher$owner_name, path(export_dir,"owners_name.csv"), na = "", quote = "all")
  write_csv(cypher$owner_address, path(export_dir, "owners_addr.csv"), na = "", quote = "all")
}

# FL corporate filings ----

cypherize_sunbiz <- function(dbconn, parcels_cyp) {
  filings <- tbl(dbconn, "corporations")
  officers <- tbl(dbconn, "officers")
  
  corps_in_parcels <- parcels_cyp$owner_name %>%
    filter(owner_type == "Corporate") %>%
    distinct(owner_name) %>%
    deframe()
  
  all_corps <- filings %>%
    select(COR_NAME) %>%
    arrange(COR_NAME) %>%
    collect() %>%
    deframe() %>%
    split(str_extract(., "^..."))
  
  crosswalk <- map_dfr(corps_in_parcels, \(c) {
    all_corps_sub <- all_corps[[str_extract(c, "^...")]]
    return(tibble(
      DOR_NAME = c,
      SUNBIZ_NAME = all_corps_sub[str_detect(all_corps_sub, paste0("^", c))]
    ))
  }, .progress = TRUE) %>%
    distinct()
  
  # corps_in_sunbiz_outright <- filings %>%
  #   filter(COR_NAME %in% corps_in_parcels) %>%
  #   distinct(COR_NAME) %>%
  #   collect() %>%
  #   deframe()
  # 
  # corps_not_in_sunbiz_outright <- corps_in_parcels[!(corps_in_parcels %in% corps_in_sunbiz_outright)]
  # 
  # corps_detectable <- map(corps_not_in_sunbiz_outright, \(c) {
  #   srch <- all_corps[[str_extract(c, "^...")]]
  #   srch[str_detect(srch, paste0("^", c))] %>%
  #     return()
  #   }, .progress = TRUE)
  # 
  # corps <- map2_dfr(
  #   corps_detectable, 
  #   corps_not_in_sunbiz_outright,
  #   \(x,y) tibble(Name_in_parcel = y, Name_in_Sunbiz = x)
  # ) %>%
  #   filter(if_all(everything(), ~ !is.na(.x))) %>%
  #   bind_rows(
  #     tibble(
  #       Name_in_parcel = corps_in_parcels,
  #       Name_in_Sunbiz = corps_in_parcels
  #     )
  #   )
  # 
  # names_in_sunbiz <- corps$Name_in_Sunbiz
  
  sunbiz_corps <- crosswalk$SUNBIZ_NAME
  
  listings <- filings %>%
    filter(COR_NAME %in% sunbiz_corps) %>%
    collect()
  
  cor_nums <- listings$CORPORATION_NUMBER
  
  officer_listings <- officers %>%
    filter(CORPORATION_NUMBER %in% cor_nums) %>%
    collect() %>%
    rowwise() %>%
    mutate(OFF_ID = hash(paste0(OFFICER_NAME, OFFICER_ADDRESS, OFFICER_CITY))) %>%
    relocate(OFF_ID) %>%
    relocate(CORPORATION_NUMBER, .after = last_col())
  
  # return(list(
  #   crosswalk = corps,
  #   listings = listings,
  #   officers = officer_listings
  # ))
  
  return(list(
    crosswalk = crosswalk,
    listings = listings,
    officers = officer_listings
  ))
}

write_sunbiz <- function(sunbiz, export_dir) {
  write_csv(sunbiz$crosswalk, path(export_dir, "crosswalk.csv"), quote = "all", na = "")
  write_csv(sunbiz$listings, path(export_dir, "filings.csv"), quote = "all", na = "")
  write_csv(sunbiz$officers, path(export_dir, "officers.csv"), quote = "all", na = "")
}
