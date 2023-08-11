# converte.r

source("shimberg_cypherise.r")

# Do the parcels for selected years ----

pconn <- DBI::dbConnect(RSQLite::SQLite(), "../duval_parcels_time.db")
parcels <- tbl(pconn, "duval_parcels")
parcels_22 <- parcels %>% filter(year == 22, dor_uc_text == "Single Family")
parcels_19 <- parcels %>% filter(year == 19, dor_uc_text == "Single Family")

parcels_22 <- cypherize_fl_parcels(parcels_22)
write_cypherized_parcels(parcels_22, "export_22_20230615/")

parcels_19 <- cypherize_fl_parcels(parcels_19)
write_cypherized_parcels(parcels_19, "export_19_20230615/")

# Do the sunbiz shit ----

sconn <- DBI::dbConnect(RSQLite::SQLite(), "../sunbiz.db")
sunbiz_22 <- cypherize_sunbiz(sconn, parcels_22)
sunbiz_19 <- cypherize_sunbiz(sconn, parcels_19)

write_sunbiz(sunbiz_22, "export_22_20230615/")
write_sunbiz(sunbiz_19, "export_19_20230615/")
