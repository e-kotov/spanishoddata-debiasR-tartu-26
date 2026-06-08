library(targets)
library(tarchetypes)

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
    "ggdist"
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

# Define the mapped analysis targets
daily_analysis <- tar_map(
  values = day_dates,
  names = day,

  # --- FLOW ACCURACY BRANCH ---
  tar_target(daily_dates, c(start = date_start, end = date_end)),
  tar_target(daily_mpd_raw, fetch_mpd(daily_dates, hourly = TRUE)),

  tar_target(daily_mpd_clean, clean_mpd(daily_mpd_raw)),

  # --- 1. FLOW ACCURACY BRANCH (DAILY) ---
  tar_target(
    daily_merged_dataset,
    merge_datasets(
      daily_mpd_clean,
      target_benchmark_clean
    )
  ),

  tar_target(daily_bias_total, measure_bias_metrics(daily_merged_dataset)),

  # --- 1B. FLOW ACCURACY BRANCH (DAILY - WORK OR STUDY) ---
  tar_target(daily_mpd_raw_work, fetch_mpd(daily_dates, hourly = FALSE, purposes = "work_or_study")),
  tar_target(daily_mpd_clean_work, clean_mpd(daily_mpd_raw_work)),
  tar_target(
    daily_merged_dataset_work,
    merge_datasets(
      daily_mpd_clean_work,
      target_benchmark_clean
    )
  ),
  tar_target(daily_bias_total_work, measure_bias_metrics(daily_merged_dataset_work)),

  # --- 1C. FLOW ACCURACY BRANCH (DAILY - ALL) ---
  tar_target(
    daily_mpd_raw_all,
    fetch_mpd(
      daily_dates,
      hourly = FALSE,
      purposes = "all",
      group_activities = TRUE
    )
  ),
  tar_target(daily_mpd_clean_all, clean_mpd(daily_mpd_raw_all)),
  tar_target(
    daily_merged_dataset_all,
    merge_datasets(
      daily_mpd_clean_all,
      target_benchmark_clean
    )
  ),
  tar_target(daily_bias_total_all, measure_bias_metrics(daily_merged_dataset_all)),
  tar_target(
    daily_activity_combo_bias,
    measure_activity_combo_bias(daily_mpd_raw_all, target_benchmark_clean)
  ),


  # --- 2. HOURLY ACCURACY BRANCH (INDEPENDENT) ---
  tar_target(
    daily_merged_hourly,
    merge_hourly_accuracy(daily_mpd_clean, target_benchmark_clean)
  ),

  tar_target(daily_bias_metrics, measure_bias_metrics(daily_merged_hourly)),

  # --- 3. POPULATION COVERAGE BRANCH ---
  tar_target(daily_mitms_pop_raw, fetch_mitms_population(daily_dates)),
  tar_target(
    daily_pop_coverage,
    merge_population_coverage(daily_mitms_pop_raw, target_census_population)
  )
)

list(
  # --- Common Targets (Same for all days) ---
  tar_target(target_zones_raw, fetch_zones()),
  tar_target(target_census_population, fetch_census_population()),
  tar_target(target_benchmark_raw, fetch_benchmarks()),

  tar_target(
    target_benchmark_clean,
    clean_benchmarks(target_benchmark_raw, target_zones_raw)
  ),

  # --- Daily Analysis (the tar_map output) ---
  daily_analysis,

  # --- Synthesis & Visualization ---
  # Combine Flow Results
  tar_combine(
    target_hourly_bias_combined,
    daily_analysis$daily_bias_metrics,
    command = dplyr::bind_rows(!!!.x, .id = "day_source") |>
      dplyr::mutate(day = gsub("daily_bias_metrics_", "", day_source))
  ),

  tar_combine(
    target_daily_bias_combined,
    daily_analysis$daily_bias_total,
    command = dplyr::bind_rows(!!!.x, .id = "day_source") |>
      dplyr::mutate(day = gsub("daily_bias_total_", "", day_source))
  ),

  tar_combine(
    target_daily_bias_combined_work,
    daily_analysis$daily_bias_total_work,
    command = dplyr::bind_rows(!!!.x, .id = "day_source") |>
      dplyr::mutate(day = gsub("daily_bias_total_work_", "", day_source))
  ),

  tar_combine(
    target_daily_bias_combined_all,
    daily_analysis$daily_bias_total_all,
    command = dplyr::bind_rows(!!!.x, .id = "day_source") |>
      dplyr::mutate(day = gsub("daily_bias_total_all_", "", day_source))
  ),

  tar_combine(
    target_activity_combo_bias_combined,
    daily_analysis$daily_activity_combo_bias,
    command = dplyr::bind_rows(!!!.x, .id = "day_source") |>
      dplyr::mutate(day = gsub("daily_activity_combo_bias_", "", day_source))
  ),

  tar_target(
    target_activity_combo_bias_summary,
    summarize_activity_combo_bias(target_activity_combo_bias_combined)
  ),

  # Combine Population Coverage Results
  tar_combine(
    target_pop_coverage_combined,
    daily_analysis$daily_pop_coverage,
    command = dplyr::bind_rows(!!!.x, .id = "day_source") |>
      dplyr::mutate(day = gsub("daily_pop_coverage_", "", day_source))
  ),

  # Generate final plots
  # tar_target(
  #   target_hourly_bias_plot,
  #   plot_hourly_bias(target_hourly_bias_combined) +
  #     ggplot2::labs(subtitle = "Hourly capture intensity vs. census average\nFilters: Work/Study & Infrequent Activity")
  # ),

  tar_target(
    target_daily_bias_plot,
    plot_daily_bias(target_daily_bias_combined) +
      ggplot2::labs(subtitle = expression(atop("Relative discrepancy between MITMS trips and census benchmarks", paste("Filters: ", bold("Work/Study & Infrequent Activity")))))
  ),

  tar_target(
    target_daily_bias_plot_work,
    plot_daily_bias(target_daily_bias_combined_work) +
      ggplot2::labs(subtitle = expression(atop("Relative discrepancy between MITMS trips and census benchmarks", paste("Filters: ", bold("Work or Study Only")))))
  ),

  tar_target(
    target_daily_bias_plot_all,
    plot_daily_bias(target_daily_bias_combined_all) +
      ggplot2::labs(subtitle = expression(atop("Relative discrepancy between MITMS trips and census benchmarks", paste("Filters: ", bold("None")))))
  ),

  tar_target(
    target_activity_combo_bias_plot,
    plot_activity_combo_bias(
      target_activity_combo_bias_combined,
      highlight_filter = "Work or Study + Infrequent Activity"
    )
  ),

  tar_target(
    target_activity_combo_bias_work_plot,
    plot_activity_combo_bias(
      target_activity_combo_bias_combined,
      highlight_filter = "Work or Study"
    )
  ),

  tar_target(
    target_activity_combo_bias_work_frequent_plot,
    plot_activity_combo_bias(
      target_activity_combo_bias_combined,
      highlight_filter = "Work or Study + Frequent Activity"
    )
  ),

  tar_target(
    target_pop_coverage_plot,
    plot_population_coverage(target_pop_coverage_combined)
  ),

  tar_target(
    target_pop_vs_coverage_plot,
    plot_pop_vs_coverage_bias(target_pop_coverage_combined)
  ),

  # Save PNG files (Standardized to width=12, height=8)
  # tar_target(
  #   target_hourly_bias_png,
  #   {
  #     path <- "figures/hourly_bias_march_2023.png"
  #     ggplot2::ggsave(
  #       path,
  #       target_hourly_bias_plot,
  #       width = 12,
  #       height = 8,
  #       dpi = 300
  #     )
  #     path
  #   },
  #   format = "file"
  # ),

  tar_target(
    target_daily_bias_png,
    {
      path <- "figures/daily_bias_march_2023.png"
      ggplot2::ggsave(
        path,
        target_daily_bias_plot,
        width = 12,
        height = 8,
        dpi = 300
      )
      path
    },
    format = "file"
  ),

  tar_target(
    target_daily_bias_work_png,
    {
      path <- "figures/daily_bias_work_march_2023.png"
      ggplot2::ggsave(
        path,
        target_daily_bias_plot_work,
        width = 12,
        height = 8,
        dpi = 300
      )
      path
    },
    format = "file"
  ),

  tar_target(
    target_daily_bias_all_png,
    {
      path <- "figures/daily_bias_all_march_2023.png"
      ggplot2::ggsave(
        path,
        target_daily_bias_plot_all,
        width = 12,
        height = 8,
        dpi = 300
      )
      path
    },
    format = "file"
  ),

  tar_target(
    target_activity_combo_bias_csv,
    {
      path <- "figures/activity_combo_bias_march_2023.csv"
      utils::write.csv(target_activity_combo_bias_summary, path, row.names = FALSE)
      path
    },
    format = "file"
  ),

  tar_target(
    target_activity_combo_bias_png,
    {
      path <- "figures/activity_combo_bias_march_2023.png"
      ggplot2::ggsave(
        path,
        target_activity_combo_bias_plot,
        width = 12,
        height = 8,
        dpi = 300
      )
      path
    },
    format = "file"
  ),

  tar_target(
    target_activity_combo_bias_best_png,
    {
      path <- "figures/activity_combo_bias_best_march_2023.png"
      ggplot2::ggsave(
        path,
        target_activity_combo_bias_plot,
        width = 12,
        height = 8,
        dpi = 300
      )
      path
    },
    format = "file"
  ),

  tar_target(
    target_activity_combo_bias_work_png,
    {
      path <- "figures/activity_combo_bias_work_march_2023.png"
      ggplot2::ggsave(
        path,
        target_activity_combo_bias_work_plot,
        width = 12,
        height = 8,
        dpi = 300
      )
      path
    },
    format = "file"
  ),

  tar_target(
    target_activity_combo_bias_work_frequent_png,
    {
      path <- "figures/activity_combo_bias_work_frequent_march_2023.png"
      ggplot2::ggsave(
        path,
        target_activity_combo_bias_work_frequent_plot,
        width = 12,
        height = 8,
        dpi = 300
      )
      path
    },
    format = "file"
  ),

  tar_target(
    target_pop_coverage_png,
    {
      path <- "figures/population_coverage_march_2023.png"
      ggplot2::ggsave(
        path,
        target_pop_coverage_plot,
        width = 12,
        height = 8,
        dpi = 300
      )
      path
    },
    format = "file"
  ),

  tar_target(
    target_pop_vs_coverage_png,
    {
      path <- "figures/pop_vs_coverage_bias_march_2023.png"
      ggplot2::ggsave(
        path,
        target_pop_vs_coverage_plot,
        width = 12,
        height = 8,
        dpi = 300
      )
      path
    },
    format = "file"
  )
)
