#' Create property nodes by time
#' @description This method creates a true representation of property owned
#' by a single entity over time using sales data. For example, it will divide
#' a parcel P sold in 2008, 2012, and 2019 into four properties, 2000 (or
#' whatever the beginning of the target period is)-2008,
#' 2008-2012, 2012-2019, and 2019-2020 (or whatever the end target period is).

create_property_time_blocks <- function(sales, parcels) {
  date_bounds = ym(c("2000-09", "2022-11"))

  sales <- read_csv("./data/duval_sales.csv")
  sale_codes <- readxl::read_xlsx("./data/real_property_transfer_codes.xlsx")

  sales <- sales %>%
    left_join(sale_codes, join_by(trans == code))

  sales %>%
    filter(distressed) %>%
    mutate(trans = factor(trans)) %>%
    ggplot() +
    geom_histogram(aes(x = year, group = trans, fill = trans), bins = 13) +
    scale_fill_brewer(palette = "Accent")

  sales <- sales %>%
    mutate(sale_date = ym(paste(year, month, sep = "-"))) %>%
    filter(sale_date >= date_bounds[1], sale_date <= date_bounds[2])

  pcon <- DBI::dbConnect(RSQLite::SQLite(), "./data/duval_parcels_time.db")
  parcels <- tbl(pcon, "duval_parcels")
  parcels <- parcels %>%
    filter(housing_type == "Single Family") %>%
    select(parcel_id, address, address2, contains("owner"), year) %>%
    collect()

  unsold <- parcels %>%
    select(parcel_id22 = parcel_id) %>%
    distinct()

  unsold <- bind_rows(
    unsold %>% mutate(sale_date = date_bounds[1]),
    unsold %>% mutate(sale_date = date_bounds[2]),
    sales %>% filter(parcel_id22 %in% parcels$parcel_id) %>%
      select(parcel_id22, sale_date)
  )

  sale_breaks <- unsold %>%
    group_by(parcel_id22) %>%
    summarize(breaks = list(
      unique(sale_date) %>%
        sort()
    )) %>%
    ungroup()

  time_properties <- sale_breaks %>%
    rowwise() %>%
    reframe(
      parcel_id = parcel_id22,
      begin_date = breaks[1:(length(breaks)-1)],
      end_date = breaks[2:(length(breaks))]
    ) %>%
    ungroup()

  time_properties <- time_properties %>%
    rowid_to_column("p_id")

  parcels <- parcels %>%
    mutate(read_date = paste(year+2000, "10", sep = "-") %>% ym)

  # for joining with information, october 2018 is the read date#

  parcel_sum <- parcels %>%
    select(parcel_id, read_date, address:address2, contains("owner")) %>%
    inner_join(
      time_properties %>% head(100),
      join_by(
        parcel_id,
        between(read_date, begin_date, end_date)
      )
    ) %>%
    arrange(p_id, read_date) %>%
    group_by(p_id) %>%
    distinct(pick(owner_name:owner_state)) %>%
    ungroup()
}
