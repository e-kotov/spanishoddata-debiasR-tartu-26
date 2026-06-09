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
measure_coverage_metrics <- function(merged_dataset_or_df) {
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

#' Measure coverage bias for every destination-purpose filter combination
measure_activity_combo_coverage <- function(mpd_raw, benchmark_clean) {
  purposes <- c("work_or_study", "infrequent_activity", "frequent_activity")
  purpose_sets <- unlist(
    lapply(
      seq_along(purposes),
      \(set_size) combn(purposes, set_size, simplify = FALSE)
    ),
    recursive = FALSE
  )

  benchmark <- benchmark_clean |>
    dplyr::transmute(origin = as.character(origin), population = target)

  dplyr::bind_rows(lapply(purpose_sets, function(purpose_set) {
    activity_flows <- mpd_raw |>
      dplyr::filter(
        activity_origin == "home",
        activity_destination %in% purpose_set
      ) |>
      dplyr::transmute(
        origin = as.character(id_origin),
        flow = as.numeric(flow)
      ) |>
      dplyr::summarise(
        user_count = sum(flow, na.rm = TRUE),
        .by = origin
      )

    benchmark |>
      dplyr::inner_join(activity_flows, by = "origin") |>
      dplyr::mutate(mpd_source = "MITMS") |>
      debiasR::measure_bias() |>
      dplyr::mutate(
        filter_id = paste(purpose_set, collapse = "+"),
        filter_label = paste(
          tools::toTitleCase(gsub("_", " ", purpose_set)),
          collapse = " + "
        ),
        filter_size = length(purpose_set)
      )
  }))
}

#' Summarize destination-purpose filter coverage across municipalities and days
summarize_activity_combo_coverage <- function(activity_combo_coverage_combined) {
  activity_combo_coverage_combined |>
    dplyr::summarise(
      median_coverage = stats::median(coverage_score, na.rm = TRUE),
      mean_coverage = mean(coverage_score, na.rm = TRUE),
      municipalities = dplyr::n_distinct(origin),
      days = dplyr::n_distinct(day),
      observations = dplyr::n(),
      .by = c(filter_id, filter_label, filter_size)
    ) |>
    dplyr::mutate(
      representation = dplyr::case_when(
        median_coverage > 1 ~ "Overrepresented",
        median_coverage < 1 ~ "Underrepresented",
        TRUE ~ "Matches benchmark"
      )
    ) |>
    dplyr::arrange(filter_size, filter_label)
}

#' Merge and Calculate Population Coverage (Vignette #4 Style)
merge_population_coverage <- function(mitms_pop_raw, census_population) {
  coverage_df <- census_population |>
    dplyr::inner_join(mitms_pop_raw, by = "origin") |>
    dplyr::rename(user_count = mitms_resident_count) |>
    dplyr::mutate(mpd_source = "MITMS")

  debiasR::measure_bias(coverage_df)
}

#' Measure Area-Level Generation Residuals
measure_generation_residuals <- function(merged_dataset_or_df) {
  # Accept either a list (from merge_datasets) or a raw data frame
  coverage_df <- if (is.data.frame(merged_dataset_or_df)) merged_dataset_or_df else merged_dataset_or_df$coverage

  # Use debiasR to calculate standardized residuals based on population and user_count
  # Here, population corresponds to expected total commuting generations from the benchmark
  res <- debiasR::validate_bias_residual_structure(
    coverage_df = coverage_df,
    coverage_area_col = "origin",
    population_col = "population",
    user_count_col = "user_count"
  )
  
  # Extract the area-level residual table
  res$area_level
}

#' Prep benchmark totals as a dummy OD table for debiasR
prep_benchmark_for_adjustment <- function(benchmark_clean) {
  # debiasR adjustment functions expect an OD matrix even for marginal calibration.
  # Since we only have origin totals, we create a dummy self-loop OD matrix
  # where destination = origin, which allows the package functions to function
  # correctly using 'origin' calibration.
  benchmark_clean |>
    dplyr::transmute(
      origin = as.character(origin),
      destination = as.character(origin), # Dummy destination
      flow = as.numeric(target)
    )
}
