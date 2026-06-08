#' Fetch Mobile Phone Mobility Data
fetch_mpd <- function(
  dates,
  hourly = FALSE,
  purposes = c("work_or_study", "infrequent_activity"),
  group_activities = FALSE
) {
  configure_data_cache()
  res <- spanishoddata::spod_get(type = "od", zones = "muni", dates = dates, max_download_size_gb = 5)

  # Calculate number of days in the range
  num_days <- as.numeric(as.Date(dates["end"]) - as.Date(dates["start"])) + 1

  # Grouping columns
  group_cols <- c("id_origin", "id_destination")
  if (hourly) {
    group_cols <- c(group_cols, "hour")
  }
  if (group_activities) {
    group_cols <- c(group_cols, "activity_origin", "activity_destination")
  }

  # Filter for commutes and average daily flow
  if (is.null(purposes) || "all" %in% purposes) {
    res_processed <- res |>
      dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) |>
      dplyr::summarise(flow = sum(n_trips, na.rm = TRUE) / !!num_days, .groups = "drop") |>
      dplyr::collect()
  } else {
    res_processed <- res |>
      dplyr::filter(activity_origin == "home", activity_destination %in% purposes) |>
      dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) |>
      dplyr::summarise(flow = sum(n_trips, na.rm = TRUE) / !!num_days, .groups = "drop") |>
      dplyr::collect()
  }

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
  if ("activity_origin" %in% colnames(mpd_raw)) {
    res$activity_origin <- mpd_raw$activity_origin
  }
  if ("activity_destination" %in% colnames(mpd_raw)) {
    res$activity_destination <- mpd_raw$activity_destination
  }

  res
}
