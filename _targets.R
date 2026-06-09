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
  tar_target(target_benchmark_od, prep_benchmark_for_adjustment(target_benchmark_clean)),
  tar_target(target_census_population, fetch_census_population()),
  tar_target(target_covariates, fetch_covariates()),
  tar_target(target_distances, calculate_distances(target_zones_raw)),

  daily_analysis <- tar_map(
    values = day_dates,
    names = day,
    
    # Raw Data Fetching
    tar_target(daily_mpd_raw, fetch_mpd(date_start, hourly = TRUE, purposes = c("work_or_study", "infrequent_activity"))),
    tar_target(daily_mpd_clean, clean_mpd(daily_mpd_raw)),
    
    # Subset to Benchmark Origins (required for bias adjustment models)
    tar_target(daily_mpd_subset, daily_mpd_clean |> dplyr::filter(origin %in% target_benchmark_clean$origin)),
    
    # --- 1. FLOW ACCURACY BRANCH (DAILY) ---
    tar_target(daily_merged_hourly, merge_hourly_accuracy(daily_mpd_subset, target_benchmark_clean)),
    tar_target(daily_merged_dataset, merge_datasets(daily_mpd_subset, target_benchmark_clean)),
    tar_target(daily_coverage_total, measure_coverage_metrics(daily_merged_dataset)),
    tar_target(daily_coverage_metrics, measure_coverage_metrics(daily_merged_hourly)),
    tar_target(daily_generation_residuals, measure_generation_residuals(daily_merged_dataset)),

    # --- 4. BIAS ADJUSTMENT BRANCH ---
    # Simple Adjustment (Selection Rate)
    tar_target(
      daily_adj_selection_income,
      debiasR::adjust_selection_rate(
        mpd_od_df = daily_mpd_subset,
        coverage_df = daily_coverage_total,
        covariates_df = target_covariates,
        covariate_col = "net_income_equiv",
        benchmark_od_df = target_benchmark_od,
        calibration_aggregate = "origin"
      )
    ),
    
    # Advanced Adjustment (Multilevel Model - Frequentist)
    tar_target(
      daily_adj_multilevel_freq,
      debiasR::adjust_multilevel_bayes(
        mpd_od_df = daily_mpd_subset,
        coverage_df = daily_coverage_total,
        covariates_df = target_covariates,
        distance_df = target_distances,
        model_engine = "frequentist",
        formula = flow ~ net_income_equiv_o + mean_age_o + log_distance + bias_e_origin
      )
    ),

    # --- 1B. FLOW ACCURACY BRANCH (DAILY - WORK OR STUDY) ---
    tar_target(daily_mpd_raw_work, fetch_mpd(date_start, hourly = FALSE, purposes = "work_or_study")),
    tar_target(daily_mpd_clean_work, clean_mpd(daily_mpd_raw_work)),
    tar_target(daily_merged_dataset_work, merge_datasets(daily_mpd_clean_work, target_benchmark_clean)),
    tar_target(daily_coverage_total_work, measure_coverage_metrics(daily_merged_dataset_work)),

    # --- 1C. FLOW ACCURACY BRANCH (DAILY - ALL ACTIVITIES) ---
    tar_target(daily_mpd_raw_all, fetch_mpd(date_start, hourly = FALSE, purposes = c("work_or_study", "infrequent_activity", "frequent_activity"), group_activities = TRUE)),
    tar_target(daily_mpd_clean_all, clean_mpd(daily_mpd_raw_all)),
    tar_target(daily_merged_dataset_all, merge_datasets(daily_mpd_clean_all, target_benchmark_clean)),
    tar_target(daily_coverage_total_all, measure_coverage_metrics(daily_merged_dataset_all)),

    # --- 2. ACTIVITY COMBO BRANCH ---
    tar_target(daily_activity_combo_coverage, measure_activity_combo_coverage(daily_mpd_raw_all, target_benchmark_clean)),

    # --- 3. POPULATION COVERAGE BRANCH ---
    tar_target(daily_pop_raw, fetch_mitms_population(date_start)),
    tar_target(daily_pop_coverage, merge_population_coverage(daily_pop_raw, target_census_population))
  ),

  # --- Synthesis & Visualization ---
  
  # Combine Coverage Results
  tar_combine(
    target_hourly_coverage_score_combined,
    daily_analysis$daily_coverage_metrics,
    command = dplyr::bind_rows(!!!.x, .id = "day_source") |>
      dplyr::mutate(day = gsub("daily_coverage_metrics_", "", day_source))
  ),

  tar_combine(
    target_daily_coverage_score_combined,
    daily_analysis$daily_coverage_total,
    command = dplyr::bind_rows(!!!.x, .id = "day_source") |>
      dplyr::mutate(day = gsub("daily_coverage_total_", "", day_source))
  ),

  tar_combine(
    target_generation_residuals_combined,
    daily_analysis$daily_generation_residuals,
    command = dplyr::bind_rows(!!!.x, .id = "day_source") |>
      dplyr::mutate(day = gsub("daily_generation_residuals_", "", day_source))
  ),

  tar_combine(
    target_daily_coverage_score_combined_work,
    daily_analysis$daily_coverage_total_work,
    command = dplyr::bind_rows(!!!.x, .id = "day_source") |>
      dplyr::mutate(day = gsub("daily_coverage_total_work_", "", day_source))
  ),

  tar_combine(
    target_daily_coverage_score_combined_all,
    daily_analysis$daily_coverage_total_all,
    command = dplyr::bind_rows(!!!.x, .id = "day_source") |>
      dplyr::mutate(day = gsub("daily_coverage_total_all_", "", day_source))
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
    target_adj_selection_income_combined,
    daily_analysis$daily_adj_selection_income,
    command = dplyr::bind_rows(!!!.x, .id = "day_source") |>
      dplyr::mutate(day = gsub("daily_adj_selection_income_", "", day_source))
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

  # Method Comparison Target
  tar_target(
    target_method_comparison,
    {
      # Evaluate Raw (Unadjusted flows)
      raw_val <- debiasR::validate_flow_overall(
        adj_df = target_mpd_raw_combined |> dplyr::rename(flow_adj = flow),
        benchmark_od_df = target_benchmark_od,
        method_name = "Raw (Unadjusted)"
      )
      
      # Evaluate Selection Rate (Income)
      income_val <- debiasR::validate_flow_overall(
        adj_df = target_adj_selection_income_combined,
        benchmark_od_df = target_benchmark_od,
        method_name = "Selection Rate (Income)"
      )
      
      # Evaluate Multilevel (Frequentist)
      multi_val <- debiasR::validate_flow_overall(
        adj_df = target_adj_multilevel_combined,
        benchmark_od_df = target_benchmark_od,
        method_name = "Multilevel (Freq)"
      )
      
      # Combine into summary table
      dplyr::bind_rows(
        as.data.frame(raw_val[names(raw_val) != "data"]),
        as.data.frame(income_val[names(income_val) != "data"]),
        as.data.frame(multi_val[names(multi_val) != "data"])
      )
    }
  ),

  # Generate Plots
  tar_target(
    target_daily_coverage_score_plot,
    plot_daily_coverage_score(target_daily_coverage_score_combined) +
      ggplot2::labs(subtitle = expression(atop("Relative discrepancy between MITMS trips and census benchmarks", paste("Filters: ", bold("Work/Study & Infrequent Activity")))))
  ),

  tar_target(
    target_generation_residuals_plot,
    plot_daily_generation_residuals(target_generation_residuals_combined)
  ),

  tar_target(
    target_method_comparison_plot,
    plot_method_comparison(target_method_comparison)
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
      ggplot2::labs(subtitle = expression(atop("Relative discrepancy between MITMS trips and census benchmarks", paste("Filters: ", bold("Work or Study Only")))))
  ),

  tar_target(
    target_daily_coverage_score_plot_all,
    plot_daily_coverage_score(target_daily_coverage_score_combined_all, max_x = 10) +
      ggplot2::labs(subtitle = expression(atop("Relative discrepancy between MITMS trips and census benchmarks", paste("Filters: ", bold("No Activity Filter (All Trips)")))))
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
    target_generation_residuals_png,
    {
      path <- "figures/daily_generation_residuals_march_2023.png"
      ggplot2::ggsave(path, target_generation_residuals_plot, width = 12, height = 8, dpi = 300)
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
