#' Merge datasets for bias analysis
merge_datasets <- function(mpd_clean, benchmark_clean) {
  # Commuter benchmark target (for TRUE accuracy measurement)
  bench_df <- benchmark_clean |>
    dplyr::select(origin, target)

  # Total user trips per zone (Daily only)
  user_df <- mpd_clean |>
    dplyr::group_by(origin) |>
    dplyr::summarise(user_count = sum(flow, na.rm = TRUE), .groups = "drop")

  # Create the coverage data frame
  coverage_df <- bench_df |>
    dplyr::inner_join(user_df, by = "origin") |>
    dplyr::mutate(
      mpd_source = "MITMS",
      population = target
    ) |>
    dplyr::select(-target)

  list(
    mpd = mpd_clean,
    benchmark = benchmark_clean,
    coverage = coverage_df
  )
}

#' Dedicated merge for HOURLY accuracy
merge_hourly_accuracy <- function(mpd_clean, benchmark_clean) {
  bench_df <- benchmark_clean |> dplyr::select(origin, target)

  # Robust Hourly Join: Ensure all 24 hours exist
  all_hours <- expand.grid(
    origin = unique(bench_df$origin),
    hour = 0:23,
    stringsAsFactors = FALSE
  )

  user_df <- mpd_clean |>
    dplyr::group_by(origin, hour) |>
    dplyr::summarise(user_count = sum(flow, na.rm = TRUE), .groups = "drop")

  coverage_df <- all_hours |>
    dplyr::left_join(user_df, by = c("origin", "hour")) |>
    dplyr::inner_join(bench_df, by = "origin") |>
    dplyr::mutate(
      user_count = dplyr::coalesce(user_count, 0),
      mpd_source = "MITMS",
      population = target / 24 # THE CRITICAL SCALING
    ) |>
    dplyr::select(-target)

  coverage_df
}

#' Measure bias metrics using debiasR
measure_bias_metrics <- function(merged_dataset_or_df) {
  # Accept either a list (from merge_datasets) or a raw data frame
  coverage_df <- if (is.data.frame(merged_dataset_or_df)) merged_dataset_or_df else merged_dataset_or_df$coverage

  # Helper to calculate relative error (Directional)
  calc_relative_error <- function(df) {
    res <- debiasR::measure_bias(df)
    return(res)
  }

  # If hourly, measure bias for each hour
  if ("hour" %in% colnames(coverage_df)) {
    bias_results <- coverage_df |>
      dplyr::group_by(hour) |>
      dplyr::group_modify(~ calc_relative_error(.x)) |>
      dplyr::ungroup()
    return(bias_results)
  } else {
    return(calc_relative_error(coverage_df))
  }
}

#' Measure coverage bias for every observed activity filter combination
measure_activity_combo_bias <- function(mpd_raw, benchmark_clean) {
  activity_flows <- mpd_raw |>
    dplyr::transmute(
      origin = as.character(id_origin),
      activity_origin,
      activity_destination,
      flow = as.numeric(flow)
    ) |>
    dplyr::summarise(
      user_count = sum(flow, na.rm = TRUE),
      .by = c(activity_origin, activity_destination, origin)
    )

  coverage_df <- benchmark_clean |>
    dplyr::transmute(origin = as.character(origin), population = target) |>
    dplyr::inner_join(activity_flows, by = "origin") |>
    dplyr::mutate(mpd_source = "MITMS")

  coverage_df |>
    dplyr::group_by(activity_origin, activity_destination) |>
    dplyr::group_modify(~ debiasR::measure_bias(.x)) |>
    dplyr::ungroup()
}

#' Summarize activity-combination bias across municipalities and days
summarize_activity_combo_bias <- function(activity_combo_bias_combined) {
  observed_summary <- activity_combo_bias_combined |>
    dplyr::summarise(
      median_bias = stats::median(coverage_bias, na.rm = TRUE),
      mean_bias = mean(coverage_bias, na.rm = TRUE),
      median_coverage_ratio = stats::median(coverage_score, na.rm = TRUE),
      municipalities = dplyr::n_distinct(origin),
      days = dplyr::n_distinct(day),
      observations = dplyr::n(),
      .by = c(activity_origin, activity_destination)
    ) |>
    dplyr::mutate(
      representation = dplyr::case_when(
        median_bias < 0 ~ "Overrepresented",
        median_bias > 0 ~ "Underrepresented",
        TRUE ~ "Matches benchmark"
      )
    )

  combination_grid <- expand.grid(
    activity_origin = sort(unique(activity_combo_bias_combined$activity_origin)),
    activity_destination = sort(unique(activity_combo_bias_combined$activity_destination)),
    stringsAsFactors = FALSE
  )

  combination_grid |>
    dplyr::left_join(
      observed_summary,
      by = c("activity_origin", "activity_destination")
    )
}

#' Merge and Calculate Population Coverage Bias (Vignette #4 Style)
merge_population_coverage <- function(mitms_pop_raw, census_population) {
  coverage_df <- census_population |>
    dplyr::inner_join(mitms_pop_raw, by = "origin") |>
    dplyr::rename(user_count = mitms_resident_count) |>
    dplyr::mutate(mpd_source = "MITMS")

  debiasR::measure_bias(coverage_df)
}
