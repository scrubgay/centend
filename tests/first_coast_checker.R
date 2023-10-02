# first_coast_checker.R
# check correct or incorrectness of certain orders

library(tidyverse)
devtools::load_all()

records_link <- "./data/fc_uncorrected_20230913/firstcoast21.json"

year <- str_extract(records_link, "..(?=\\.json)")

records <- load_records(records_link, .apoc = TRUE)

# investigate how correct it is ----
property_summary <- records %>%
  filter(labels == "Property") %>%
  count(componentId, county) %>%
  pivot_wider(names_from = county, values_from = n, values_fill = 0) %>%
  mutate(TOTAL = DUVAL)
  #mutate(TOTAL = NASSAU + BAKER + DUVAL + CLAY + `ST JOHNS`)

top <- property_summary %>% slice_max(n = 20, order_by = TOTAL) %>%
  mutate(entity = "") %>%
  relocate(entity)

write_csv(top, paste0("./exports/fc_ss_check/firstcoast_top20_", year, ".csv"))

details <- map(top$componentId, \(id) {
  who_is(records, id) %>% .$Owner
}) %>%
  `names<-`(paste0("ID", top$componentId))

writexl::write_xlsx(details, paste0("./exports/fc_ss_check/firstcoast_top20_", year, "_details.xlsx"), format_headers = FALSE)

