# transformer.R
# test on duval/first coast
# where the fuck did the other script go hello

library(tidyverse)
devtools::load_all()

conn <- DBI::dbConnect(RSQLite::SQLite(), "./data/duval_parcels_time.db")
parcels <- tbl(conn, "duval_parcels")
parcels <- parcels %>%
  filter(year == 22,
         dor_uc %in% c(1:4, 8))
parcels <- collect(parcels)

parcels <- parcels %>%
  mutate(across(c(owner_address, owner_address2), remove_attn),
         across(where(is.character), ~ .x %>% str_to_upper %>% str_remove_all("[^0-9A-Z- ]") %>% str_squish),
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

export_dir <- "./exports/duval_allhousing_2022/"
dir.create(export_dir)

write_csv(parcels, file.path(export_dir, "parcels.csv"), na = "", quote = "all")
write_csv(corp_xwalk,  file.path(export_dir, "filings.csv"), na = "", quote = "all")
write_csv(officers_present, file.path(export_dir, "officers.csv"), na = "", quote = "all")
