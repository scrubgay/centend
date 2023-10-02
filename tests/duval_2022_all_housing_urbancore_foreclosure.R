# urbancore
# investigate urban core landlords

library(tidyverse)
library(sf)
devtools::load_all()

records_link <- "./data/dv22_fc.json"

#year <- str_extract(records_link, "..(?=\\.json)")
year = "22"

records <- load_records(records_link, .apoc = TRUE)

# percentage of foreclosed single family by component

fc.pop.sum <- records %>%
  filter(labels == "Parcel",
         id %in% (
           records %>%
             filter(labels == "Property" & housing_type == "SINGLE FAMILY") %>%
             pull(parcel_id))) %>%
  count(foreclosed, taxdelinquent) %>%
  mutate(p = n/sum(n))

fc.sum <- records %>%
  filter(labels == "Parcel",
         id %in% (records %>%
           filter(labels == "Property" & housing_type == "SINGLE FAMILY") %>%
           pull(parcel_id))) %>%
  count(componentId, foreclosed, taxdelinquent) %>%
  group_by(componentId) %>%
  mutate(total = sum(n, na.rm = TRUE))

fc.sum <- fc.sum %>%
  ungroup() %>%
  mutate(status = case_when(
    !is.na(foreclosed) & !is.na(taxdelinquent) ~ "fc_td",
    !is.na(foreclosed) ~ "fc",
    !is.na(taxdelinquent) ~ "td",
    .default = "stable"
  )) %>%
  select(-c(foreclosed, taxdelinquent)) %>%
  pivot_wider(names_from = status, values_from = n, values_fill = 0)

fc.sum <- fc.sum %>%
  mutate(across(stable:fc_td, ~ .x/total, .names = "{.col}.p"))

evic.sum <- read_csv("~/../Downloads/parcel_evictions.csv") %>%
  mutate(across(everything(), ~ ifelse(.x == "null", NA, .x)),
         parcel_id = str_remove_all(parcel_id, '"'))

evictions <- inner_join(evic.sum,
           records %>%
             filter(labels == "Property", housing_type == "SINGLE FAMILY") %>%
             select(parcel_id)
) %>%
  full_join(records %>%
              filter(labels == "Property", housing_type == "SINGLE FAMILY") %>%
              select(parcel_id))

evictions <- evictions %>%
  mutate(evictions = ifelse(is.na(evictions),0,evictions))

ggplot(evictions) +
  geom_histogram(aes(x = evictions)) +
  facet_wrap(vars(foreclosed), nrow = 1)
