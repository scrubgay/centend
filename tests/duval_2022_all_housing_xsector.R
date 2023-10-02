# first_coast_checker.R
# check correct or incorrectness of certain orders

library(tidyverse)
devtools::load_all()

records_link <- "./data/duval-22-allhousing.json"

#year <- str_extract(records_link, "..(?=\\.json)")
year = "22"

records <- load_records(records_link, .apoc = TRUE)

# investigate how correct it is ----
# property_summary <- records %>%
#   filter(labels == "Property") %>%
#   count(componentId, county) %>%
#   pivot_wider(names_from = county, values_from = n, values_fill = 0) %>%
#   mutate(TOTAL = DUVAL)
#   #mutate(TOTAL = NASSAU + BAKER + DUVAL + CLAY + `ST JOHNS`)

property_summary <- records %>%
  filter(labels == "Property") %>%
  count(componentId, housing_type) %>%
  pivot_wider(names_from = housing_type, values_from = n, values_fill = 0)

xsector <- property_summary %>%
  filter(if_any(c(`SINGLE FAMILY`, `MOBILE HOMES`), ~ .x > 0),
         if_any(c(`MULTI-FAMILY - 10 UNITS OR MORE`), ~ .x > 0)) %>%
  arrange(desc(`MULTI-FAMILY - 10 UNITS OR MORE`))

xsector_details <- xsector$componentId %>%
  map(~ who_is(records, .x) %>% .$Owner) %>%
  `names<-`(xsector$componentId)

exportdir <- "./exports/duval_allhousing_2022/"

writexl::write_xlsx(xsector, file.path(exportdir, "xsector_counts.xlsx"), format_headers = FALSE)
writexl::write_xlsx(xsector_details, file.path(exportdir, "xsector_details.xlsx"), format_headers = FALSE)
