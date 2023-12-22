#' Utility renaming functions
#'
#' This is a function that applies Shimberg Center business rules to
#' column names of data from either the Florida Department of Revenue or
#' Shimberg's preprocessing server. Not really meant for use in the real world.
#'
#' @param data Parcel or sales data.
#' @param what One of `"parcels"` or `"sales"`.
#' @param from One of `"fdor"` or `"ec2"`.
#' @return The cleaned data source
#' @export shimbergize

shimbergize <- function(data, what, from) {
  if (what == "parcels") {data = shimbergize_parcels(data, from)}
  else if (what == "sales") {data = shimbergize_sales(data, from)}
}

shimbergize_parcels <- function(data, source) {
  if (source == "fdor") {
    data <- data %>% select(
      year = ASMNT_YR,
      county_id = CO_NO,
      parcel_id = PARCEL_ID,
      address = PHY_ADDR1,
      address2 = PHY_ADDR2,
      city_from_dor = PHY_CITY,
      zip = PHY_ZIPCD,
      dor_uc = DOR_UC,
      pa_uc = PA_UC,
      actualyear_built = ACT_YR_BLT,
      eff_yr_blt = EFF_YR_BLT,
      buildings = NO_BULDNG,
      tot_lvg_area = TOT_LVG_AREA,
      homestead = EXMPT_01,
      residential_units = NO_RES_UNTS,
      owner_name = OWN_NAME,
      owner_address = OWN_ADDR1,
      owner_address2 = OWN_ADDR2,
      owner_city = OWN_CITY,
      owner_state = OWN_STATE,
      owner_zip = OWN_ZIPCD,
      just_value = JV,
    ) %>%
      mutate(
        year = year - 2000,
        owner_zip = as.character(owner_zip),
        across(c(dor_uc, pa_uc), as.integer)
      )
  } else if (source == "ec2") {
    data <- data %>% select(
      year,
      county_id = co_no,
      parcel,
      parcel_id,
      address = phy_addr1,
      address2 = phy_addr2,
      city_from_dor = phy_city,
      zip = phy_zip,
      dor_uc,
      pa_uc,
      actualyear_built = act_yr_blt,
      eff_yr_blt = eff_yr_blt,
      buildings = no_buldng,
      tot_lvg_area,
      homestead = exmpt_01,
      residential_units = no_res_unts,
      owner_name = own_name,
      owner_address = own_addr1,
      owner_address2 = own_addr2,
      owner_city = own_city,
      owner_state = own_state,
      owner_zip = own_zipcd,
      just_value = jv,
      assessed_value = av_sd,
      land_value = lnd_val,
      land_sqft = lnd_sqfoot
    ) %>%
      mutate(
        owner_zip = as.character(owner_zip),
        across(c(dor_uc, pa_uc), as.integer)
      )
  }
  # presaved in the package
  data <- data %>% left_join(county_codes, join_by(county_id == id))

  return(data)
}

shimbergize_sales <- function(data, source) {
  if (source == "fdor") {
    data <- data %>%
      rename_with(~ str_to_lower(.x)) %>%
      mutate(parcel = parcel_id %>% str_remove_all(" ")) %>%
      select(
        co_no,
        parcel,
        parcel_id,
        sale = sale_prc,
        year = sale_yr,
        month = sale_mo,
        trans = qual_cd,
        improv = vi_cd,
      )
  }

  data <- data %>%
    mutate(co_no = as.integer(co_no)) %>%
    left_join(county_codes, join_by(co_no == id))

  return(data)
}
