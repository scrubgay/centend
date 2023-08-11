# create_sunbiz.r

library(tidyverse)
library(fs)
library(lubridate)
library(RSQLite)

create_sunbiz_sqlite <- function(sunbiz_dir) {
  files <- list.files(sunbiz_dir)
  
  if("sunbiz.db" %in% files) {
    stop("Error: 'sunbiz.db' already exists in ", sunbiz_dir)
  }
  if(!("sunbiz_fields.csv" %in% files)) {
    stop("Error: 'sunbiz_fields.csv' is required and does not exist in ", sunbiz_dir)
  }
  
  sunbiz_fields <- read_csv(path(sunbiz_dir, "sunbiz_fields.csv"), show_col_types = FALSE)
  
  files <- list.files(sunbiz_dir) %>%
    Filter(function(x) {str_detect(x, "cordata")}, .)
  
  walk(files, function(f) {
    message("Reading ", f, "...")
    
    data <- read_fwf(
      path(sunbiz_dir, f),
      fwf_widths(sunbiz_fields$LENGTH, sunbiz_fields$FIELD_NAME),
      col_select = -contains("FILLER")
    )
    
    data <- data %>%
      # filter out unnecessary fields
      # select(-contains("FILLER")) %>%
      mutate(FILE_DATE = mdy(FILE_DATE)) %>%
      
      # clean
      mutate(across(where(is.character),
                    ~ str_to_upper(.x) %>% 
                      str_remove_all("[^0-9A-Z ]") %>%
                      str_squish()
      ))
    
    reports <- paste0("REPORT_(YEAR|DATE)_", c(1,2,3)) %>%
      map_dfr( ~
        data %>%
          select(CORPORATION_NUMBER,
                 matches(.x)) %>%
          rename_with(~ str_remove(.x, "(?<=REPORT_(DATE|YEAR))_\\d"), cols = contains("REPORT")) %>%
          mutate(across(contains("DATE"), mdy))
      )
    
    reports <- reports %>%
      filter(!is.na(REPORT_DATE) & !is.na(REPORT_YEAR))
    
    officers <- c("OFFICER_1_*", "OFFICER_2_*", "OFFICER_3_*", "OFFICER_4_*", "OFFICER_5_*", "OFFICER_6_*") %>%
      map_dfr( ~
        data %>%
          select(CORPORATION_NUMBER,
                 matches(.x)) %>%
          rename_with(~ str_remove(.x, "(?<=OFFICER_)\\d_")) %>%
          mutate(`OFFICER_ZIP+4` = as.character(`OFFICER_ZIP+4`))
      ) %>%
      filter(!is.na(OFFICER_NAME))
    
    data <- data %>%
      select(-contains("OFFICER_"),
             -contains("REPORT_"))
    
    conn <- dbConnect(SQLite(), path(sunbiz_dir, "sunbiz.db"))
    
    dbWriteTable(conn, "corporations", data, append = TRUE)
    dbWriteTable(conn, "officers", officers, append = TRUE)
    dbWriteTable(conn, "reports", reports, append = TRUE)
    
    dbDisconnect(conn)
  })

}

create_sunbiz_sqlite("~/sunbiz/")


# create agents -----------------------------------------------------------

library(dbplyr)

conn <- dbConnect(SQLite(), "~/sunbiz/sunbiz.db")

corporates <- tbl(conn, "corporations")

agents <- corporates %>%
  select(starts_with("REGISTERED_AGENT")) %>%
  filter(REGISTERED_AGENT_TYPE == "C",
         REGISTERED_AGENT_ADDRESS != "N/A",
         REGISTERED_AGENT_ADDRESS != "NA",
         str_length(REGISTERED_AGENT_ADDRESS) >= 6,
         !(is.na(REGISTERED_AGENT_ADDRESS))) %>%
  distinct(pick(REGISTERED_AGENT_ADDRESS:REGISTERED_AGENT_STATE)) %>%
  filter(if_all(everything(), ~ !is.na(.x))) %>%
  mutate(across(everything(), ~ str_to_upper(.x))) %>%
  collect() %>%
  mutate(across(everything(), ~ str_remove_all(.x, "[^0-9A-Z ]" %>% str_squish()))) %>%
  filter(!str_detect(REGISTERED_AGENT_CITY, "[0-9]"),
         REGISTERED_AGENT_CITY != "") %>%
  distinct() %>%
  arrange(REGISTERED_AGENT_STATE, REGISTERED_AGENT_CITY, REGISTERED_AGENT_ADDRESS)

write_csv(agents, "../Projects/shimberg/iop/parcel-analysis/agents.csv", na = "")
