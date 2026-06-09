library(targets)
library(tarchetypes)

# Increase download timeout for large files (e.g., 200MB census microdata)
options(timeout = 600)

# targets::tar_visnetwork()
# targets::tar_make()

# Source all custom functions
tar_source("R/")

# Set target options
tar_option_set(
  packages = c(
    "spanishoddata",
    "ineAtlas",
    "ineapir",
    "debiasR",
    "mapSpain",
    "dplyr",
    "dbplyr",
    "sf",
    "ggplot2",
    "ggdist",
    "tidyr"
  ),
  format = "rds"
)

# Helper for day-specific dates in March 2023
day_dates <- data.frame(
  day = c(
    "Monday",
    "Tuesday",
    "Wednesday",
    "Thursday",
    "Friday",
    "Saturday",
    "Sunday"
  ),
  date_start = c(
    "2023-03-06",
    "2023-03-07",
    "2023-03-08",
    "2023-03-09",
    "2023-03-10",
    "2023-03-11",
    "2023-03-12"
  ),
  date_end = c(
    "2023-03-06",
    "2023-03-07",
    "2023-03-08",
    "2023-03-09",
    "2023-03-10",
    "2023-03-11",
    "2023-03-12"
  ),
  stringsAsFactors = FALSE
)

# Define the targets
list(
  tar_target(target_zones_raw, fetch_zones()),
  tar_target(target_benchmark_raw, fetch_benchmarks()),
  tar_target(target_benchmark_clean, clean_benchmarks(target_benchmark_raw, target_zones_raw)),
  # Origin pseudo-OD (destination = origin) — required ONLY by adjust_selection_rate's
  # API, which needs an OD benchmark even when calibrating on origin margins.
  # NEVER validate OD-cell predictions against this; use validate_origin_margin().
  tar_target(target_benchmark_origin_pseudo_od, make_origin_pseudo_od(target_benchmark_clean)),
  tar_target(target_census_population, fetch_census_population()),
  tar_target(target_covariates, fetch_covariates()),
  tar_target(target_distances, calculate_distances(target_zones_raw)),

  daily_analysis <- tar_map(
    values = day_dates,
    names = day,
    
    # Raw Data Fetching
    tar_target(daily_mpd_raw, fetch_mpd(date_start, purposes = c("work_or_study", "infrequent_activity"))),
    tar_target(daily_mpd_clean, clean_mpd(daily_mpd_raw)),
    
    # Subset to Benchmark Origins (required for bias adjustment models)
    tar_target(daily_mpd_subset, daily_mpd_clean |> dplyr::filter(origin %in% target_benchmark_clean$origin)),
    
    # --- 1. COVERAGE-BIAS BRANCH (DAILY) ---
    tar_target(daily_merged_dataset, merge_datasets(daily_mpd_subset, target_benchmark_clean)),
    tar_target(daily_coverage_bias, measure_coverage_bias(daily_merged_dataset)),
    tar_target(daily_user_count_residuals, measure_user_count_residuals(daily_merged_dataset)),

    # --- 4. BIAS ADJUSTMENT BRANCH ---
    # Raking ratio against origin marginals (canonical for marginal-origin benchmarks)
    tar_target(
      daily_adj_raking_ratio,
      adjust_origin_flows_raking(daily_mpd_subset, target_benchmark_clean)
    ),

    # Inverse penetration using residential population coverage at origins
    tar_target(
      daily_adj_inverse_penetration,
      adjust_origin_flows_inverse_penetration(daily_mpd_subset, daily_pop_coverage)
    ),

    # Selection rate (calibrated against origin marginals via pseudo-OD)
    tar_target(
      daily_adj_selection_income,
      debiasR::adjust_selection_rate(
        mpd_od_df = daily_mpd_subset,
        coverage_df = daily_coverage_bias,
        covariates_df = target_covariates,
        covariate_col = "net_income_equiv",
        benchmark_od_df = target_benchmark_origin_pseudo_od,
        calibration_aggregate = "origin"
      )
    ),
    tar_target(
      daily_adj_selection_mean_age,
      debiasR::adjust_selection_rate(
        mpd_od_df = daily_mpd_subset,
        coverage_df = daily_coverage_bias,
        covariates_df = target_covariates,
        covariate_col = "mean_age",
        benchmark_od_df = target_benchmark_origin_pseudo_od,
        calibration_aggregate = "origin"
      )
    ),
    tar_target(
      daily_adj_selection_pct_under18,
      debiasR::adjust_selection_rate(
        mpd_od_df = daily_mpd_subset,
        coverage_df = daily_coverage_bias,
        covariates_df = target_covariates,
        covariate_col = "pct_under18",
        benchmark_od_df = target_benchmark_origin_pseudo_od,
        calibration_aggregate = "origin"
      )
    ),
    tar_target(
      daily_adj_selection_pct_over65,
      debiasR::adjust_selection_rate(
        mpd_od_df = daily_mpd_subset,
        coverage_df = daily_coverage_bias,
        covariates_df = target_covariates,
        covariate_col = "pct_over65",
        benchmark_od_df = target_benchmark_origin_pseudo_od,
        calibration_aggregate = "origin"
      )
    ),
    tar_target(
      daily_adj_selection_pct_spanish,
      debiasR::adjust_selection_rate(
        mpd_od_df = daily_mpd_subset,
        coverage_df = daily_coverage_bias,
        covariates_df = target_covariates,
        covariate_col = "pct_spanish",
        benchmark_od_df = target_benchmark_origin_pseudo_od,
        calibration_aggregate = "origin"
      )
    ),
    tar_target(
      daily_adj_selection_gini,
      debiasR::adjust_selection_rate(
        mpd_od_df = daily_mpd_subset,
        coverage_df = daily_coverage_bias,
        covariates_df = target_covariates,
        covariate_col = "gini",
        benchmark_od_df = target_benchmark_origin_pseudo_od,
        calibration_aggregate = "origin"
      )
    ),

    # Multilevel model (frequentist engine of adjust_multilevel_bayes)
    tar_target(
      daily_adj_multilevel_freq,
      debiasR::adjust_multilevel_bayes(
        mpd_od_df = daily_mpd_subset,
        coverage_df = daily_coverage_bias,
        covariates_df = target_covariates,
        distance_df = target_distances,
        model_engine = "frequentist",
        formula = flow ~ net_income_equiv_o + mean_age_o + log_distance + bias_e_origin
      )
    ),

    # --- 1B. COVERAGE-BIAS BRANCH (DAILY - WORK OR STUDY) ---
    tar_target(daily_mpd_raw_work, fetch_mpd(date_start, purposes = "work_or_study")),
    tar_target(daily_mpd_clean_work, clean_mpd(daily_mpd_raw_work)),
    tar_target(daily_merged_dataset_work, merge_datasets(daily_mpd_clean_work, target_benchmark_clean)),
    tar_target(daily_coverage_bias_work, measure_coverage_bias(daily_merged_dataset_work)),

    # --- 1C. COVERAGE-BIAS BRANCH (DAILY - ALL ACTIVITIES) ---
    tar_target(daily_mpd_raw_all, fetch_mpd(date_start, purposes = c("work_or_study", "infrequent_activity", "frequent_activity"), group_activities = TRUE)),
    tar_target(daily_mpd_clean_all, clean_mpd(daily_mpd_raw_all)),
    tar_target(daily_merged_dataset_all, merge_datasets(daily_mpd_clean_all, target_benchmark_clean)),
    tar_target(daily_coverage_bias_all, measure_coverage_bias(daily_merged_dataset_all)),

    # --- 2. ACTIVITY COMBO BRANCH ---
    tar_target(daily_activity_combo_coverage, measure_activity_combo_coverage(daily_mpd_raw_all, target_benchmark_clean)),

    # --- 3. POPULATION COVERAGE BRANCH ---
    tar_target(daily_pop_raw, fetch_mitms_population(date_start)),
    tar_target(daily_pop_coverage, merge_population_coverage(daily_pop_raw, target_census_population))
  ),

  # --- Synthesis & Visualization ---
  
  # Combine Coverage Results
  tar_combine(
    target_daily_coverage_score_combined,
    daily_analysis$daily_coverage_bias,
    command = dplyr::bind_rows(!!!.x, .id = "day_source") |>
      dplyr::mutate(day = gsub("daily_coverage_bias_", "", day_source))
  ),

  tar_combine(
    target_user_count_residuals_combined,
    daily_analysis$daily_user_count_residuals,
    command = dplyr::bind_rows(!!!.x, .id = "day_source") |>
      dplyr::mutate(day = gsub("daily_user_count_residuals_", "", day_source))
  ),

  tar_combine(
    target_daily_coverage_score_combined_work,
    daily_analysis$daily_coverage_bias_work,
    command = dplyr::bind_rows(!!!.x, .id = "day_source") |>
      dplyr::mutate(day = gsub("daily_coverage_bias_work_", "", day_source))
  ),

  tar_combine(
    target_daily_coverage_score_combined_all,
    daily_analysis$daily_coverage_bias_all,
    command = dplyr::bind_rows(!!!.x, .id = "day_source") |>
      dplyr::mutate(day = gsub("daily_coverage_bias_all_", "", day_source))
  ),

  tar_combine(
    target_activity_combo_coverage_combined,
    daily_analysis$daily_activity_combo_coverage,
    command = dplyr::bind_rows(!!!.x, .id = "day_source") |>
      dplyr::mutate(day = gsub("daily_activity_combo_coverage_", "", day_source))
  ),

  tar_target(
    target_activity_combo_coverage_summary,
    summarize_activity_combo_coverage(target_activity_combo_coverage_combined)
  ),

  # Combine Population Coverage Results
  tar_combine(
    target_pop_coverage_combined,
    daily_analysis$daily_pop_coverage,
    command = dplyr::bind_rows(!!!.x, .id = "day_source") |>
      dplyr::mutate(day = gsub("daily_pop_coverage_", "", day_source))
  ),

  # Combine Adjustment Results
  tar_combine(
    target_adj_raking_combined,
    daily_analysis$daily_adj_raking_ratio,
    command = dplyr::bind_rows(!!!.x, .id = "day_source") |>
      dplyr::mutate(day = gsub("daily_adj_raking_ratio_", "", day_source))
  ),

  tar_combine(
    target_adj_inv_pen_combined,
    daily_analysis$daily_adj_inverse_penetration,
    command = dplyr::bind_rows(!!!.x, .id = "day_source") |>
      dplyr::mutate(day = gsub("daily_adj_inverse_penetration_", "", day_source))
  ),

  tar_combine(
    target_adj_selection_income_combined,
    daily_analysis$daily_adj_selection_income,
    command = dplyr::bind_rows(!!!.x, .id = "day_source") |>
      dplyr::mutate(day = gsub("daily_adj_selection_income_", "", day_source))
  ),
  tar_combine(
    target_adj_selection_mean_age_combined,
    daily_analysis$daily_adj_selection_mean_age,
    command = dplyr::bind_rows(!!!.x, .id = "day_source") |>
      dplyr::mutate(day = gsub("daily_adj_selection_mean_age_", "", day_source))
  ),
  tar_combine(
    target_adj_selection_pct_under18_combined,
    daily_analysis$daily_adj_selection_pct_under18,
    command = dplyr::bind_rows(!!!.x, .id = "day_source") |>
      dplyr::mutate(day = gsub("daily_adj_selection_pct_under18_", "", day_source))
  ),
  tar_combine(
    target_adj_selection_pct_over65_combined,
    daily_analysis$daily_adj_selection_pct_over65,
    command = dplyr::bind_rows(!!!.x, .id = "day_source") |>
      dplyr::mutate(day = gsub("daily_adj_selection_pct_over65_", "", day_source))
  ),
  tar_combine(
    target_adj_selection_pct_spanish_combined,
    daily_analysis$daily_adj_selection_pct_spanish,
    command = dplyr::bind_rows(!!!.x, .id = "day_source") |>
      dplyr::mutate(day = gsub("daily_adj_selection_pct_spanish_", "", day_source))
  ),
  tar_combine(
    target_adj_selection_gini_combined,
    daily_analysis$daily_adj_selection_gini,
    command = dplyr::bind_rows(!!!.x, .id = "day_source") |>
      dplyr::mutate(day = gsub("daily_adj_selection_gini_", "", day_source))
  ),

  tar_combine(
    target_adj_multilevel_combined,
    daily_analysis$daily_adj_multilevel_freq,
    command = dplyr::bind_rows(!!!.x, .id = "day_source") |>
      dplyr::mutate(day = gsub("daily_adj_multilevel_freq_", "", day_source))
  ),
  
  # Combine Raw MPD for comparison
  tar_combine(
    target_mpd_raw_combined,
    daily_analysis$daily_mpd_subset,
    command = dplyr::bind_rows(!!!.x, .id = "day_source") |> 
      dplyr::mutate(day = gsub("daily_mpd_subset_", "", day_source))
  ),

  # Method Comparison — origin-marginal validation
  # Each method's adjusted OD flows are aggregated to origin margins, then
  # validated against the origin-marginal benchmark (the only fit scope the
  # marginal-origin benchmark supports). Reported Pearson/RMSE/MAE are
  # origin-margin fit metrics, NOT OD-cell fit metrics.
  tar_target(
    target_method_comparison,
    {
      drop_data <- function(v) as.data.frame(v[names(v) != "data"])
      dplyr::bind_rows(
        drop_data(validate_origin_margin(
          target_mpd_raw_combined |> dplyr::rename(flow_adj = flow),
          target_benchmark_clean,
          "Raw (Unadjusted)"
        )),
        drop_data(validate_origin_margin(
          target_adj_raking_combined,
          target_benchmark_clean,
          "Raking Ratio"
        )),
        drop_data(validate_origin_margin(
          target_adj_inv_pen_combined,
          target_benchmark_clean,
          "Inverse Pen."
        )),
        drop_data(validate_origin_margin(
          target_adj_selection_income_combined,
          target_benchmark_clean,
          "Sel. Rate (Net Income)"
        )),
        drop_data(validate_origin_margin(
          target_adj_selection_mean_age_combined,
          target_benchmark_clean,
          "Sel. Rate (Mean Age)"
        )),
        drop_data(validate_origin_margin(
          target_adj_selection_pct_under18_combined,
          target_benchmark_clean,
          "Sel. Rate (% <18)"
        )),
        drop_data(validate_origin_margin(
          target_adj_selection_pct_over65_combined,
          target_benchmark_clean,
          "Sel. Rate (% >65)"
        )),
        drop_data(validate_origin_margin(
          target_adj_selection_pct_spanish_combined,
          target_benchmark_clean,
          "Sel. Rate (% Native)"
        )),
        drop_data(validate_origin_margin(
          target_adj_selection_gini_combined,
          target_benchmark_clean,
          "Sel. Rate (Gini)"
        )),
        drop_data(validate_origin_margin(
          target_adj_multilevel_combined,
          target_benchmark_clean,
          "Multilevel"
        ))
      )
    }
  ),

  # Generate Plots
  tar_target(
    target_daily_coverage_score_plot,
    plot_daily_coverage_score(target_daily_coverage_score_combined) +
      ggplot2::labs(subtitle = "home -> work/study + infrequent")
  ),

  tar_target(
    target_user_count_residuals_plot,
    plot_user_count_residuals(target_user_count_residuals_combined)
  ),

  tar_target(
    target_method_comparison_plot,
    plot_method_comparison(target_method_comparison)
  ),

  tar_target(
    target_level3_heatmap_plot,
    plot_level3_heatmap(
      target_mpd_raw_combined, 
      target_adj_raking_combined, 
      target_adj_inv_pen_combined, 
      target_adj_selection_income_combined, 
      target_adj_selection_mean_age_combined,
      target_adj_selection_pct_under18_combined,
      target_adj_selection_pct_over65_combined,
      target_adj_selection_pct_spanish_combined,
      target_adj_selection_gini_combined,
      target_adj_multilevel_combined, 
      target_benchmark_clean
    )
  ),

  tar_target(
    target_margin_diagnostic_plot,
    plot_margin_diagnostic(
      target_mpd_raw_combined, 
      target_adj_selection_income_combined, 
      target_benchmark_clean
    )
  ),

  # Save PNG files
  tar_target(
    target_daily_coverage_score_plot_work,
    plot_daily_coverage_score(target_daily_coverage_score_combined_work) +
      ggplot2::labs(subtitle = "home -> work/study")
  ),

  tar_target(
    target_daily_coverage_score_plot_all,
    plot_daily_coverage_score(target_daily_coverage_score_combined_all, max_x = 4) +
      ggplot2::labs(subtitle = "home -> work/study + frequent + infrequent")
  ),

  tar_target(
    target_activity_combo_coverage_work_plot,
    plot_activity_combo_coverage(target_activity_combo_coverage_combined, highlight_filter = "Work or Study")
  ),

  tar_target(
    target_activity_combo_coverage_work_frequent_plot,
    plot_activity_combo_coverage(target_activity_combo_coverage_combined, highlight_filter = "Work or Study + Frequent Activity")
  ),

  tar_target(
    target_activity_combo_coverage_plot,
    plot_activity_combo_coverage(target_activity_combo_coverage_combined, highlight_filter = "Work or Study + Infrequent Activity")
  ),

  tar_target(
    target_pop_coverage_plot,
    plot_population_coverage(target_pop_coverage_combined)
  ),

  tar_target(
    target_pop_vs_coverage_plot,
    plot_pop_vs_coverage_score(target_pop_coverage_combined)
  ),

  # Maps of population-coverage-bias outliers (|peak b_i| > 0.3) — disabled
  # tar_target(
  #   target_outlier_map_weekday_plot,
  #   plot_outlier_map(target_pop_coverage_combined, target_zones_raw, day_type = "Weekday")
  # ),
  # tar_target(
  #   target_outlier_map_weekend_plot,
  #   plot_outlier_map(target_pop_coverage_combined, target_zones_raw, day_type = "Weekend")
  # ),
  # tar_target(
  #   target_outlier_map_weekday_png,
  #   {
  #     path <- "figures/outlier_map_weekday.png"
  #     ggplot2::ggsave(path, target_outlier_map_weekday_plot, width = 12, height = 8, dpi = 300)
  #     path
  #   },
  #   format = "file"
  # ),
  # tar_target(
  #   target_outlier_map_weekend_png,
  #   {
  #     path <- "figures/outlier_map_weekend.png"
  #     ggplot2::ggsave(path, target_outlier_map_weekend_plot, width = 12, height = 8, dpi = 300)
  #     path
  #   },
  #   format = "file"
  # ),

  # Save PNG files
  tar_target(
    target_daily_coverage_score_png,
    {
      path <- "figures/daily_coverage_score_march_2023.png"
      ggplot2::ggsave(path, target_daily_coverage_score_plot, width = 12, height = 8, dpi = 300)
      path
    },
    format = "file"
  ),

  tar_target(
    target_user_count_residuals_png,
    {
      path <- "figures/daily_user_count_residuals_march_2023.png"
      ggplot2::ggsave(path, target_user_count_residuals_plot, width = 12, height = 8, dpi = 300)
      path
    },
    format = "file"
  ),

  tar_target(
    target_method_comparison_png,
    {
      path <- "figures/adjustment_method_comparison_march_2023.png"
      ggplot2::ggsave(path, target_method_comparison_plot, width = 12, height = 8, dpi = 300)
      path
    },
    format = "file"
  ),

  tar_target(
    target_margin_diagnostic_png,
    {
      path <- "figures/margin_diagnostic_scatter.png"
      ggplot2::ggsave(path, target_margin_diagnostic_plot, width = 12, height = 8, dpi = 300)
      path
    },
    format = "file"
  ),


  tar_target(
    target_level3_heatmap_png,
    {
      path <- "figures/level3_residual_heatmap_march_2023.png"
      ggplot2::ggsave(path, target_level3_heatmap_plot, width = 12, height = 8, dpi = 300)
      path
    },
    format = "file"
  ),
  tar_target(
    target_level3_stacked_bar_plot,
    plot_level3_stacked_bar(
      target_mpd_raw_combined, 
      target_adj_raking_combined, 
      target_adj_inv_pen_combined, 
      target_adj_selection_income_combined, 
      target_adj_selection_mean_age_combined,
      target_adj_selection_pct_under18_combined,
      target_adj_selection_pct_over65_combined,
      target_adj_selection_pct_spanish_combined,
      target_adj_selection_gini_combined,
      target_adj_multilevel_combined, 
      target_benchmark_clean
    )
  ),

  tar_target(
    target_level3_stacked_bar_png,
    {
      path <- "figures/level3_residual_stacked_bar_march_2023.png"
      ggplot2::ggsave(path, target_level3_stacked_bar_plot, width = 12, height = 8, dpi = 300)
      path
    },
    format = "file"
  ),
  tar_target(
    target_level3_stacked_bar_hl1_plot,
    plot_level3_stacked_bar(
      target_mpd_raw_combined, target_adj_raking_combined, target_adj_inv_pen_combined, 
      target_adj_selection_income_combined, target_adj_selection_mean_age_combined,
      target_adj_selection_pct_under18_combined, target_adj_selection_pct_over65_combined,
      target_adj_selection_pct_spanish_combined, target_adj_selection_gini_combined,
      target_adj_multilevel_combined, target_benchmark_clean,
      highlight_methods = c("Unadjusted (Raw MPD)")
    )
  ),
  tar_target(
    target_level3_stacked_bar_hl1_png,
    {
      path <- "figures/level3_residual_stacked_bar_hl1_march_2023.png"
      ggplot2::ggsave(path, target_level3_stacked_bar_hl1_plot, width = 12, height = 8, dpi = 300)
      path
    },
    format = "file"
  ),

  tar_target(
    target_level3_stacked_bar_hl2_plot,
    plot_level3_stacked_bar(
      target_mpd_raw_combined, target_adj_raking_combined, target_adj_inv_pen_combined, 
      target_adj_selection_income_combined, target_adj_selection_mean_age_combined,
      target_adj_selection_pct_under18_combined, target_adj_selection_pct_over65_combined,
      target_adj_selection_pct_spanish_combined, target_adj_selection_gini_combined,
      target_adj_multilevel_combined, target_benchmark_clean,
      highlight_methods = c("Inverse Penetration", "Multilevel Model")
    )
  ),
  tar_target(
    target_level3_stacked_bar_hl2_png,
    {
      path <- "figures/level3_residual_stacked_bar_hl2_march_2023.png"
      ggplot2::ggsave(path, target_level3_stacked_bar_hl2_plot, width = 12, height = 8, dpi = 300)
      path
    },
    format = "file"
  ),


  tar_target(
    target_daily_coverage_score_work_png,
    {
      path <- "figures/daily_coverage_score_work_march_2023.png"
      ggplot2::ggsave(path, target_daily_coverage_score_plot_work, width = 12, height = 8, dpi = 300)
      path
    },
    format = "file"
  ),

  tar_target(
    target_daily_coverage_score_all_png,
    {
      path <- "figures/daily_coverage_score_all_march_2023.png"
      ggplot2::ggsave(path, target_daily_coverage_score_plot_all, width = 12, height = 8, dpi = 300)
      path
    },
    format = "file"
  ),

  tar_target(
    target_activity_combo_coverage_work_png,
    {
      path <- "figures/activity_combo_bias_work_march_2023.png"
      ggplot2::ggsave(path, target_activity_combo_coverage_work_plot, width = 12, height = 8, dpi = 300)
      path
    },
    format = "file"
  ),

  tar_target(
    target_activity_combo_coverage_work_frequent_png,
    {
      path <- "figures/activity_combo_bias_work_frequent_march_2023.png"
      ggplot2::ggsave(path, target_activity_combo_coverage_work_frequent_plot, width = 12, height = 8, dpi = 300)
      path
    },
    format = "file"
  ),

  tar_target(
    target_activity_combo_coverage_png,
    {
      path <- "figures/activity_combo_bias_march_2023.png"
      ggplot2::ggsave(path, target_activity_combo_coverage_plot, width = 12, height = 8, dpi = 300)
      path
    },
    format = "file"
  ),

  tar_target(
    target_activity_combo_coverage_best_png,
    {
      path <- "figures/activity_combo_bias_best_march_2023.png"
      ggplot2::ggsave(path, target_activity_combo_coverage_plot, width = 12, height = 8, dpi = 300)
      path
    },
    format = "file"
  ),

  tar_target(
    target_activity_combo_coverage_csv,
    {
      path <- "figures/activity_combo_bias_march_2023.csv"
      readr::write_csv(target_activity_combo_coverage_summary, path)
      path
    },
    format = "file"
  ),

  tar_target(
    target_pop_coverage_png,
    {
      path <- "figures/population_coverage_march_2023.png"
      ggplot2::ggsave(path, target_pop_coverage_plot, width = 12, height = 8, dpi = 300)
      path
    },
    format = "file"
  ),

  tar_target(
    target_pop_vs_coverage_png,
    {
      path <- "figures/pop_vs_coverage_score_march_2023.png"
      ggplot2::ggsave(path, target_pop_vs_coverage_plot, width = 12, height = 8, dpi = 300)
      path
    },
    format = "file"
  )

)
