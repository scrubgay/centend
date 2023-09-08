# Duval.R
# test on duval
# where the fuck did the other script go hello

conn <- DBI::dbConnect(RSQLite::SQLite(), "./data/first_coast.db")
parcels <- tbl(conn, "first_coast")
parcels <- parcels %>%
  filter(year %in% c(22),
         housing_type == "Single Family")

parcels <- collect(parcels)
parcels <- parcels %>%
  mutate(across(c(owner_address, owner_address2), remove_attn),
         across(where(is.character), ~ .x %>% str_to_upper %>% str_remove_all("[^0-9A-Z- ]")),
         owner_name = ifelse(is.na(owner_name)|owner_name == "", "NA", owner_name)) %>%
  unite(owner_address, c(owner_address, owner_address2), sep = " ", na.rm = TRUE)

parcels <- identify_parcels(parcels, parcel_id, year, owner_name, owner_address, owner_city, county, homestead, housing_type = dor_uc_text, year_built = eff_yr_blt, units = residential_units)
parcels <- parcels %>%
  mutate(homestead = !is.na(homestead))


sconn <- DBI::dbConnect(RSQLite::SQLite(), "../shimberg/cypheriser/data/sunbiz.db")
agents <- tbl(sconn, "corporations") %>%
  distinct(REGISTERED_AGENT_ADDRESS, REGISTERED_AGENT_CITY)
agents <- collect(agents)

parcels <- match_agents(parcels, agents, REGISTERED_AGENT_ADDRESS, REGISTERED_AGENT_CITY)

corporations <- tbl(sconn, "corporations") %>%
  filter(STATUS == "A") %>%
  collect()

corp_xwalk <- parcels %>%
  filter(owner_type == "Corporate") %>%
  distinct(owner_name_adj) %>%
  deframe() %>%
  connect_owners_to_corps(corporations, CORPORATION_NUMBER, COR_NAME)

officers <- tbl(sconn, "officers")

officers_present <- match_corp_officers(unique(corp_xwalk$corp_id), officers, CORPORATION_NUMBER)

write_csv(parcels, "./exports/firstcoast/parcels.csv", na = "", quote = "all")
write_csv(corp_xwalk, "./exports/firstcoast/filings.csv", na = "", quote = "all")
write_csv(officers_present, "./exports/firstcoast/officers.csv", na = "", quote = "all")

# postprocessing ----

library(tidyverse)
library(sf)
devtools::load_all()

pgeo <- list.files("./data/parcel_geog", pattern = ".shp$", full.names = TRUE, recursive = TRUE)
pgeo <- map_dfr(pgeo, \(p) {
  read_sf(p) %>%
    st_centroid() %>%
    st_transform(st_crs(3086))
}, .progress = TRUE)

records <- load_records("./data/firstcoast.json", .apoc = TRUE)

property_summary <- records %>%
  filter(labels == "Property") %>%
  count(componentId, county) %>%
  pivot_wider(names_from = county, values_from = n) %>%
  mutate(across(-componentId, ~ ifelse(is.na(.x), 0, .x))) %>%
  mutate(TOTAL = NASSAU + BAKER + DUVAL + CLAY + `ST JOHNS`)

corporate <- corporate_components(records)

parcels <- tagged_properties(records)
parcels <- parcels %>%
  left_join(property_summary, join_by(componentId)) %>%
  left_join(corporate, join_by(componentId))

parcels_corp <- parcels %>%
  filter(corporate) %>%
  mutate(investor_category = LETTERS[(log10(TOTAL) %/% 0.5) + 1] %>% as.factor()) %>%
  left_join(pgeo, join_by(parcel_id == PARCELNO)) %>%
  st_as_sf()

ggplot(parcels_corp) +
  geom_sf(aes(color = investor_category), size = 0.5) +
  scale_color_brewer(type = "seq", palette = "YlGnBu") +
  theme_bw()

write_sf(parcels_corp, "./exports/firstcoast_corporate_owned_parcels.gpkg")

top <- property_summary %>% slice_max(n = 20, order_by = TOTAL) %>%
  mutate(entity = "") %>%
  relocate(entity)

write_csv(top, "./exports/firstcoast_top20_20230907.csv")

details <- map(top$componentId, \(id) {
  who_is(records, id) %>% .$Owner
}) %>%
  `names<-`(paste0("ID", top$componentId))

writexl::write_xlsx(details, "./exports/firstcoast_top20_20230907_details.xlsx", format_headers = FALSE)
