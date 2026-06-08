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
    res$bias <- (res$population - res$user_count) / res$population
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

#' Merge and Calculate Population Coverage Bias (Vignette #4 Style)
merge_population_coverage <- function(mitms_pop_raw, census_population) {
  coverage_df <- census_population |>
    dplyr::inner_join(mitms_pop_raw, by = "origin") |>
    dplyr::rename(user_count = mitms_resident_count) |>
    dplyr::mutate(mpd_source = "MITMS")

  debiasR::measure_bias(coverage_df)
}
