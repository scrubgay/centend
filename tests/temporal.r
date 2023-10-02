# temporal.r
# tracking changes in parcels based on characteristics of owners

library(tidyverse)
library(tidycensus)
census_api_key(read_file("./data/api.txt"), install = TRUE, overwrite = TRUE)
library(sf)
devtools::load_all()

records_prior <- load_records("./data/fc_uncorrected_20230913/firstcoast21.json", .apoc = TRUE)
records_post <- load_records("./data/firstcoast22.json", .apoc = TRUE)

## Check functions ----

ps_prior <- component_summary(records_prior)
ps_post <- component_summary(records_post)

parcel_info <- DBI::dbConnect(RSQLite::SQLite(), "./data/first_coast.db") %>%
  tbl("first_coast") %>%
  filter(year %in% c(21,22),
         housing_type == "Single Family") %>%
  select(year, parcel_id,
         phy_addr = address, phy_addr2 = address2, phy_zip = zip,
         contains("owner")) %>%
  collect()

parcel_info2 <- DBI::dbConnect(RSQLite::SQLite(), "./data/first_coast.db") %>%
  tbl("first_coast") %>%
  filter(year %in% c(21,22),
         housing_type == "Single Family",
         county == "Duval") %>%
  # select(year, parcel_id,
  #        phy_addr = address, phy_addr2 = address2, phy_zip = zip,
  #        contains("owner")) %>%
  collect()

sales <- read_csv("./data/duval_sales_2021.csv")

parcels_prior <- tagged_properties(records_prior)
parcels_post <- tagged_properties(records_post)

parcels_prior <- parcels_prior %>%
  mutate(year = as.integer(year)) %>%
  left_join(parcel_info, join_by(year, parcel_id)) %>%
  mutate(
    across(where(is.character), ~ .x %>% str_to_upper() %>% str_remove_all("[^0-9A-Z- ]")),
    across(contains("owner_address"), remove_attn)
  ) %>%
  unite(owner_address, contains("owner_address"), sep = " ", na.rm = TRUE) %>%
  filter(county == "DUVAL")

parcels_post <- parcels_post %>%
  mutate(year = as.integer(year)) %>%
  left_join(parcel_info, join_by(year, parcel_id)) %>%
  mutate(
    across(where(is.character), ~ .x %>% str_to_upper() %>% str_remove_all("[^0-9A-Z- ]")),
    across(contains("owner_address"), remove_attn)
  ) %>%
  unite(owner_address, contains("owner_address"), sep = " ", na.rm = TRUE) %>%
  filter(county == "DUVAL")

cities <- read_csv("./data/Florida_Places.csv") %>%
  filter(County %in% c("DUVAL", "ST JOHNS", "CLAY", "BAKER", "SAINT JOHNS", "NASSAU")) %>%
  select(Place) %>%
  deframe()

nooh <- bind_rows(
  identify_nooh(
    parcels_prior, componentId,
    id, parcel_id,
    owner_address, owner_city, owner_state, owner_zip,
    phy_addr, phy_zip,
    homestead, units,
    records_prior, .city_strings = cities
  ) %>%
    separate_wider_delim(nooh_type, ";", names = c("nooh_status", "nooh_explanation")),
  identify_nooh(
    parcels_post, componentId,
    id, parcel_id,
    owner_address, owner_city, owner_state, owner_zip,
    phy_addr, phy_zip,
    homestead, units,
    records_post, .city_strings = cities
  ) %>%
    separate_wider_delim(nooh_type, ";", names = c("nooh_status", "nooh_explanation"))
)

nooh_change <- nooh %>%
  filter(parcel_id %in% (sales %>% filter(year == 2021) %>% select(parcel_id22) %>% deframe())) %>%
  select(parcel_id, year, nooh_status, componentId) %>%
  pivot_wider(names_from = year, values_from = c(nooh_status, componentId))

nooh_change %>%
  count(nooh_status_21, nooh_status_22) %>%
  filter(if_all(everything(), ~ !is.na(.x) & .x != "Unknown")) %>%
  ggplot(aes(axis1 = nooh_status_21, axis2 = nooh_status_22, y = n)) +
  geom_alluvium(aes(fill = nooh_status_21), width = 1/12) +
  geom_stratum(width = 1/12, color = "grey") +
  geom_label(stat = "stratum", aes(label = after_stat(stratum))) +
  theme_minimal() +
  scale_fill_viridis_d() +
  labs(
    y = "Count",
    fill = "2021 Status"
  ) +
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank())

ggsave("./exports/NOOH_TO_OOH.png", dpi = 300, width = 8, height = 4)

nooh_change %>%
  count(nooh_status_21, nooh_status_22) %>%
  filter(if_all(everything(), ~ !is.na(.x) & .x != "Unknown")) %>%
  mutate(p = n/sum(n))

nooh_change %>%
  filter(nooh_status_22 == "NOOH", nooh_status_21 == "OOH") %>%
  count(componentId_22, sort = TRUE) %>%
  mutate(p = n/sum(n))

who_is(records_post, 15427) %>% .$Owner

# 6872 is progress in previous
