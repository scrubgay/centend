# normal shit----

pconn <- DBI::dbConnect(RSQLite::SQLite(), "../shimberg/cypheriser/data/duval_parcels_time.db")

parcels <- tbl(pconn, "duval_parcels")
parcels <- parcels %>%
  filter(dor_uc == 1,
         year %in% c(19,22)) %>%
  select(year, parcel_id, contains("owner")) %>%
  collect()
parcels <- parcels %>%
  mutate(
    across(where(is.character), ~ str_remove_all(.x, "[^A-Za-z0-9 ]")),
    across(contains("owner_address"), ~ remove_attn(.x))) %>%
  unite(owner_address, owner_address, owner_address2, sep = ", ", na.rm = TRUE)

idd_p <- identify_parcels(parcels, parcel_id, year, owner_name, owner_address, owner_city)

sconn <- DBI::dbConnect(RSQLite::SQLite(), "../shimberg/cypheriser/data/sunbiz.db")
agents <- tbl(sconn, "corporations")
agents <- agents %>%
  distinct(REGISTERED_AGENT_ADDRESS, REGISTERED_AGENT_CITY) %>%
  rename_with(~ .x %>% str_remove("REGISTERED_") %>% str_to_lower())
agents <- collect(agents)
corps <- tbl(sconn, "corporations")
corps <- corps %>%
  rename_with(~ .x %>% str_to_lower()) %>%
  filter(status == "A") %>%
  select(corporation_number, cor_name) %>%
  collect()

idd_p <- match_agents(idd_p, agents, agent_address, agent_city)

idd_p <- idd_p %>%
  mutate(owner_type = tag_owners(owner_name))

filing_xwalk <- connect_owners_to_corps(
  idd_p %>%
    filter(owner_type == "Corporate") %>%
    distinct(owner_name) %>%
    deframe(),
  corps, corporation_number, cor_name)

officers <- tbl(sconn, "officers") %>%
  rename_with(~ .x %>% str_to_lower)

officers_xwalk <- match_corp_officers(
  filing_xwalk$corp_id,
  officers, corporation_number, officer_name, officer_address, officer_city)



# neo4j shit ----

walk2(
  list(idd_p, filing_xwalk, officers_xwalk),
  list("parcels.csv", "filings.csv", "officers.csv"),
  \(data, name) {
    write_csv(data, paste0("./exports/", name), quote = "all", na = "")
  }
)
