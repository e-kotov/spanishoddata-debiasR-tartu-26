#' Fetch Mobile Phone Mobility Data
fetch_mpd <- function(dates, hourly = FALSE) {
  configure_data_cache()
  res <- spanishoddata::spod_get(type = "od", zones = "muni", dates = dates, max_download_size_gb = 5)

  # Calculate number of days in the range
  num_days <- as.numeric(as.Date(dates["end"]) - as.Date(dates["start"])) + 1

  # Grouping columns
  group_cols <- c("id_origin", "id_destination")
  if (hourly) {
    group_cols <- c(group_cols, "hour")
  }

  # Filter for home-to-work_or_study or infrequent_activity commutes and average daily flow
  res_processed <- res |>
    dplyr::filter(activity_origin == "home", activity_destination %in% c("work_or_study", "infrequent_activity")) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) |>
    dplyr::summarise(flow = sum(n_trips, na.rm = TRUE) / !!num_days, .groups = "drop") |>
    dplyr::collect()

  res_processed
}

#' Clean and Prepare Mobile Phone Data
clean_mpd <- function(mpd_raw) {
  # Map spanishoddata zone IDs to official INE codes and aggregate over dates
  res <- mpd_raw |>
    dplyr::transmute(
      origin = as.character(id_origin),
      destination = as.character(id_destination),
      flow = as.numeric(flow),
      mpd_source = "Orange"
    )

  if ("hour" %in% colnames(mpd_raw)) {
    res$hour <- mpd_raw$hour
  }

  res
}
