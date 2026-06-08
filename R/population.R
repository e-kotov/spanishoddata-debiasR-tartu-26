#' Fetch Resident Population Estimates from MITMS
#' Based on type = 'overnight_stays' where id_residence == id_overnight_stay
fetch_mitms_population <- function(dates) {
  configure_data_cache()

  # Fetch overnight stays
  res <- spanishoddata::spod_get(type = "overnight_stays", zones = "muni", dates = dates, max_download_size_gb = 5)

  # Filter for residents staying in their own zone
  # 'n_persons' represents the MITMS estimate of residents for that day
  res_residents <- res |>
    dplyr::filter(id_residence == id_overnight_stay) |>
    dplyr::group_by(id_residence) |>
    dplyr::summarise(mitms_resident_count = mean(n_persons, na.rm = TRUE), .groups = "drop") |>
    dplyr::rename(origin = id_residence) |>
    dplyr::collect()

  return(res_residents)
}

#' Fetch the 2022 municipal census population used for coverage validation
fetch_census_population <- function() {
  ineAtlas::get_atlas("demographics", "municipality") |>
    dplyr::filter(year == 2022) |>
    dplyr::select(mun_code, population) |>
    dplyr::rename(origin = mun_code)
}
