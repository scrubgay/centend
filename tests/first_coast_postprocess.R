# first_coast_postprocess.R

# postprocessing ----

library(tidyverse)
library(tidycensus)
census_api_key(read_file("./data/api.txt"), install = TRUE, overwrite = TRUE)
library(sf)
devtools::load_all()

county <- get_decennial(
  "county",
  variables = "P2_001N",
  state = "FL",
  county = c("Duval", "St. Johns", "Baker", "Nassau", "Clay"),
  output = "wide",
  geometry = TRUE,
  cache_table = TRUE
) %>%
  select(-P2_001N) %>%
  st_as_sf() %>%
  st_transform(st_crs(3086))

bg <- get_decennial(
  "block group",
  table = "P2",
  state = "FL",
  county = c("Duval", "St. Johns", "Baker", "Nassau", "Clay"),
  output = "wide",
  geometry = TRUE,
  cache_table = TRUE
) %>%
  select(
    GEOID,
    NAME,
    total = P2_001N,
    white = P2_005N,
    black = P2_006N,
    hispanic = P2_002N,
    native = P2_007N,
    asian = P2_008N,
    pacisl = P2_009N,
    other = P2_010N,
    mixed = P2_011N
  ) %>%
  mutate(across(white:mixed, ~ ifelse(is.na(.x), 0, .x / total), .names = "{.col}.p")) %>%
  st_as_sf() %>%
  st_transform(st_crs(3086))

pgeo <- list.files("./data/parcel_geog", pattern = ".shp$", full.names = TRUE, recursive = TRUE)
pgeo <- map_dfr(pgeo, \(p) {
  county = p %>% str_extract("(?<=pin/).*(?=_2022pin.shp)")
  read_sf(p) %>%
    st_centroid() %>%
    st_transform(st_crs(3086)) %>%
    mutate(county = county)
}, .progress = TRUE)

records <- load_records("./data/firstcoast22.json", .apoc = TRUE)

cities <- read_csv("./data/Florida_Places.csv") %>%
  filter(County %in% c("DUVAL", "ST JOHNS", "CLAY", "BAKER", "SAINT JOHNS", "NASSAU")) %>%
  select(Place) %>%
  deframe()

property_summary <- records %>%
  filter(labels == "Property") %>%
  count(componentId, county) %>%
  pivot_wider(names_from = county, values_from = n, values_fill = 0) %>%
  mutate(TOTAL = NASSAU + BAKER + DUVAL + CLAY + `ST JOHNS`)

parcels <- tagged_properties(records)

parcel_info <- DBI::dbConnect(RSQLite::SQLite(), "./data/first_coast.db") %>%
  tbl("first_coast") %>%
  filter(year %in% c(22),
         housing_type == "Single Family") %>%
  collect()

parcel_info <- parcel_info %>%
  select(year, parcel_id,
         phy_addr = address, phy_addr2 = address2, phy_zip = zip,
         contains("owner"),
         just_value, tot_lvg_area, lnd_sqfoot)

parcels <- parcels %>%
  mutate(year = as.integer(year)) %>%
  left_join(parcel_info, join_by(year, parcel_id)) %>%
  mutate(
    across(where(is.character), ~ .x %>% str_to_upper() %>% str_remove_all("[^0-9A-Z- ]")),
    across(contains("owner_address"), remove_attn)
    ) %>%
  unite(owner_address, contains("owner_address"), sep = " ", na.rm = TRUE)

nooh <- identify_nooh(
  parcels, componentId,
  id, parcel_id,
  owner_address, owner_city, owner_state, owner_zip,
  phy_addr, phy_zip,
  homestead, units,
  records, .city_strings = cities
) %>%
  separate_wider_delim(nooh_type, ";", names = c("nooh_status", "nooh_explanation"))

nooh <- nooh %>%
  left_join(pgeo, join_by(parcel_id == PARCELNO)) %>%
  st_as_sf() %>%
  st_join(bg %>% select(GEOID)) %>%
  as_tibble() %>%
  select(-geometry)

by_size <- tagged_properties(records) %>%
  left_join(property_summary %>% select(componentId, TOTAL)) %>%
  select(parcel_id, componentId, total = TOTAL) %>%
  mutate(size = LETTERS[(log10(total) %/% 0.5) + 1])

nooh <- nooh %>%
  left_join(by_size, join_by(parcel_id))

bg <- bg %>%
  right_join(
    nooh %>%
      count(GEOID, nooh_status) %>%
      pivot_wider(names_from = nooh_status, values_from = n, values_fill = 0) %>%
      mutate(across(NOOH:Unknown, ~ .x / (NOOH + OOH + Unknown), .names = "{.col}.p")), join_by(GEOID)) %>%
  right_join(
    nooh %>%
      filter(nooh_status == "NOOH") %>%
      count(GEOID, size) %>%
      arrange(GEOID, size) %>%
      pivot_wider(names_from = size, values_from = n, values_fill = 0) %>%
      mutate(across(c(A,B,C,D,E,`F`,G), ~ .x / (A + B + C + D + E + `F` + G), .names = "{.col}.p")), join_by(GEOID)
  )

# top 5 ----
top5 <- property_summary %>%
  slice_max(TOTAL, n = 5) %>%
  select(componentId)

top5$Entity <- c("Progress Residential", "American Homes 4 Rent", "Invitation Homes", "FirstKey Homes", "Amherst Group")

top5 <- nooh %>%
  filter(componentId %in% top5$componentId) %>%
  left_join(top5)

top5 %>%
  mutate(year_built = as.integer(year_built)) %>%
  ggplot() +
  geom_histogram(aes(x = just_value/1000)) +
  facet_grid(rows = vars(Entity)) +
  theme_bw() +
  labs(
    x = "Just Value (thousands)",
    y = "Count"
  )

ggsave("./exports/top5_jv.png", dpi = 300, units = "in", height = 7, width = 4)

top5 %>%
  mutate(year_built = as.integer(year_built)) %>%
  ggplot() +
  geom_histogram(aes(x = year_built)) +
  facet_grid(rows = vars(Entity)) +
  theme_bw() +
  labs(
    x = "Year Built",
    y = "Count"
  )

ggsave("./exports/top5_year.png", dpi = 300, units = "in", height = 7, width = 4)

top5 %>%
  mutate(year_built = as.integer(year_built),
         tot_lvg_area = as.integer(tot_lvg_area)) %>%
  ggplot() +
  geom_histogram(aes(x = tot_lvg_area)) +
  facet_grid(rows = vars(Entity)) +
  theme_bw() +
  labs(
    x = "Total livable area (sq ft)",
    y = "Count"
  )

ggsave("./exports/top5_size.png", dpi = 300, units = "in", height = 7, width = 4)

top5 %>%
  mutate(year_built = as.integer(year_built),
         tot_lvg_area = as.integer(tot_lvg_area)) %>%
  filter(lnd_sqfoot <= 50000) %>%
  ggplot() +
  geom_histogram(aes(x = lnd_sqfoot)) +
  facet_grid(rows = vars(Entity)) +
  theme_bw() +
  labs(
    x = "Total land (sq ft)",
    y = "Count"
  )

ggsave("./exports/top5_land.png", dpi = 300, units = "in", height = 7, width = 4)


# nooh analysis ----

ggplot(bg) +
  geom_sf(aes(fill = NOOH), color = NA) +
  geom_sf(data = county, fill = NA, linewidth = 1) +
  scale_fill_distiller(palette = "YlOrRd", direction = 1) +
  theme_bw() +
  labs(
    fill = "NOOH (%)"
  )

nooh_owners_map <- map(LETTERS[1:7], \(c) {
  ggplot(bg) +
    geom_sf(aes(fill = .data[[paste0(c, ".p")]] * 100), linewidth = 0.01) +
    scale_fill_distiller("% owned", palette = "YlOrRd", direction = 1) +
    ggtitle(paste0(c, ")")) +
    theme_bw() +
    theme(legend.position = "bottom")
})

m <- gridExtra::grid.arrange(
  nooh_owners_map[[1]],
  nooh_owners_map[[2]],
  nooh_owners_map[[3]],
  nooh_owners_map[[4]],
  nooh_owners_map[[5]],
  nooh_owners_map[[6]],
  nooh_owners_map[[7]],
  nrow = 2
)

nooh %>%
  distinct() %>%
  left_join(pgeo, join_by(parcel_id == PARCELNO)) %>%
  st_as_sf %>%
  write_sf("./exports/firstcoast22_parcels.gpkg")
write_sf(bg, "./exports/firstcoast22.gpkg")
write_sf(county, "./exports/counties.gpkg")

hex <- st_make_grid(county, cellsize = 1609, square = FALSE) %>% as_tibble %>%
  rowid_to_column %>%
  st_as_sf

hex <- hex %>%
  left_join(
    nooh %>%
      filter(nooh_status == "NOOH") %>%
      left_join(pgeo, join_by(parcel_id == PARCELNO)) %>%
      st_as_sf() %>%
      st_join(hex) %>%
      as_tibble() %>%
      count(rowid)
  ) %>%
  rename(NOOH = n) %>%
  left_join(
    nooh %>%
      filter(Property >= 100) %>%
      left_join(pgeo, join_by(parcel_id == PARCELNO)) %>%
      st_as_sf() %>%
      st_join(hex) %>%
      as_tibble() %>%
      count(rowid)
  )

hex <- hex %>%
  st_filter(county)

write_sf(hex, "./exports/firstcoast22_hex_nooh_count.gpkg")


# concentration analysis ----
hhi <- function(x) {sum((x / sum(x, na.rm = TRUE)) ** 2, na.rm = TRUE) * 10000}

conc <- nooh %>%
  filter(nooh_status == "NOOH") %>%
  count(GEOID, componentId.x) %>%
  group_by(GEOID) %>%
  summarize(hhi = hhi(n),
            count = sum(n, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(!is.na(hhi))

bg %>%
  left_join(conc) %>%
  mutate(
    hhi_cat = case_when(
      hhi < 1000 ~ "Unconcentrated",
      hhi < 1500 ~ "Low",
      hhi < 2500 ~ "Moderate",
      hhi >= 2500 ~ "Concentrated"
    ) %>%
      factor(levels = c("Unconcentrated", "Low", "Moderate", "Concentrated"), ordered = TRUE)
  ) %>%
  st_as_sf %>%
  write_sf("./exports/fc22_hhi.gpkg")
