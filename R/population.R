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

#' Fetch unified covariates for bias adjustment
fetch_covariates <- function() {
  # Based on covariates.md logic
  income_atlas <- ineAtlas::get_atlas("income", "municipality")
  demographics_atlas <- ineAtlas::get_atlas("demographics", "municipality")
  gini_p80p20_atlas <- ineAtlas::get_atlas("gini_p80p20", "municipality")
  
  # Fetch census-specific demographics (2021 context)
  # Higher ed and employment typically from the base demographics or specific census operation
  # For now, we use available atlas columns
  
  covs <- demographics_atlas |>
    dplyr::filter(year == "2023") |>
    dplyr::select(
      area = mun_code, 
      mun_name, 
      mean_age, 
      pct_under18, 
      pct_over65, 
      pct_spanish,
      population
    ) |>
    dplyr::left_join(
      income_atlas |>
        dplyr::filter(year == "2023") |>
        dplyr::select(area = mun_code, net_income_equiv),
      by = "area"
    ) |>
    dplyr::left_join(
      gini_p80p20_atlas |>
        dplyr::filter(year == "2023") |>
        dplyr::select(area = mun_code, gini),
      by = "area"
    )
    
  return(covs)
}

#' Calculate centroid-to-centroid distances for municipalities
calculate_distances <- function(zones_raw) {
  # Calculate centroids
  centroids <- zones_raw |>
    sf::st_centroid()
  
  # Calculate distance matrix (in meters)
  dist_matrix <- sf::st_distance(centroids)
  
  # Convert to long format for debiasR
  areas <- centroids$id
  colnames(dist_matrix) <- areas
  rownames(dist_matrix) <- areas
  
  dist_df <- as.data.frame(as.table(dist_matrix)) |>
    dplyr::transmute(
      origin = as.character(Var1),
      destination = as.character(Var2),
      distance_km = as.numeric(Freq) / 1000
    ) |>
    # Add a small epsilon to zero distances (intra-zonal) to allow log transformation
    dplyr::mutate(distance_km = dplyr::if_else(distance_km == 0, 0.5, distance_km))
    
  return(dist_df)
}
