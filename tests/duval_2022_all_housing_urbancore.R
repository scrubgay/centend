# urbancore
# investigate urban core landlords

library(tidyverse)
library(sf)
devtools::load_all()

records_link <- "./data/dv22_uncorrected.json"

#year <- str_extract(records_link, "..(?=\\.json)")
year = "22"

records <- load_records(records_link, .apoc = TRUE)

# check if valid ----

comp.sum <- component_summary(records)

# top 10 evictors ----

t10_evix <- comp.sum %>%
  slice_max(Eviction, n = 10) %>%
  pull(componentId) %>%
  imap_dfr(\(.x, .idx) who_is(records, .x) %>%
            .$Owner %>%
            mutate(
              componentId = .x,
              order = .idx) %>%
            relocate(componentId))

# cross sector ----

property_summary <- records %>%
  filter(labels == "Property") %>%
  count(componentId, housing_type) %>%
  pivot_wider(names_from = housing_type, values_from = n, values_fill = 0)

# urban core ----

parcel_geos <- read_sf("./data/parcel_geog/duval_2022pin/duval_2022pin.shp")

urbancore_filter <- read_sf("../shimberg/jax-fwbc-20230518/neighborhood-bounds/exports/Jacksonville_Neighborhoods.shp")

parcel_list <- parcel_geos %>%
  st_filter(urbancore_filter %>% st_transform(st_crs(parcel_geos))) %>%
  as_tibble %>%
  pull(PARCELNO)

records_urbancore <- records %>%
  filter(labels == "Property" & parcel_id %in% parcel_list) %>%
  distinct(componentId)

records_urbancore <- records %>%
  inner_join(records_urbancore)

records_urbancore <- records_urbancore %>%
  mutate(urbancore = case_when(
    parcel_id %in% parcel_list ~ TRUE,
    labels == "Property" ~ FALSE,
    .default = NA
    ))

urbancore_summary <- records_urbancore %>%
  filter(labels == "Property", housing_type == "SINGLE FAMILY") %>%
  count(componentId, urbancore) %>%
  pivot_wider(names_from = urbancore, names_prefix = "CORE_", values_from = n, values_fill = 0) %>%
  arrange(desc(CORE_TRUE))

writexl::write_xlsx(urbancore_summary, "./exports/urbancore_summary.xlsx", format_headers = FALSE)
biggest_owners <- urbancore_summary %>%
  filter(CORE_TRUE >= 10) %>%
  pull(componentId)
biggest_owners %>%
  map(~ who_is(records, .x) %>% .$Owner) %>%
  `names<-`(biggest_owners) %>%
  writexl::write_xlsx("./exports/urbancore_details.xlsx", format_headers = FALSE)
