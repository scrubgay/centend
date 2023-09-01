# Duval.R
# test on duval
# where the fuck did the other script go hello

conn <- DBI::dbConnect(RSQLite::SQLite(), "./data/duval_parcels_time.db")
parcels <- tbl(conn, "duval_parcels")
parcels <- parcels %>%
  filter(year %in% c(19,22),
         housing_type == "Single Family")

parcels <- collect(parcels)
parcels <- parcels %>%
  mutate(across(c(owner_address, owner_address2), remove_attn)) %>%
  unite(owner_address, c(owner_address, owner_address2), sep = " ", na.rm = TRUE)

parcels <- identify_parcels(parcels, parcel_id, year, owner_name, owner_address, owner_city)

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

write_csv(parcels, "./exports/parcels.csv", na = "", quote = "all")
write_csv(corp_xwalk, "./exports/filings.csv", na = "", quote = "all")
write_csv(officers_present, "./exports/officers.csv", na = "", quote = "all")
