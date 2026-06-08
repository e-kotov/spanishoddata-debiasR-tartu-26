library(ggplot2)
library(dplyr)
library(ggdist)

#' Plot Hourly Capture Rates across Days (Capture Score Ratio)
plot_hourly_bias <- function(hourly_bias_combined) {
  # Ensure day is a factor with correct order
  days_order <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
  hourly_bias_combined$day <- factor(hourly_bias_combined$day, levels = days_order)

  # Categorize days
  hourly_bias_combined <- hourly_bias_combined |>
    dplyr::mutate(day_type = ifelse(day %in% c("Saturday", "Sunday"), "Weekend", "Weekday"))

  # Calculate Capture Score: (Actual Hourly Trips) / (Average Hourly Census Benchmark)
  # 1.0 means exactly 1/24th of the daily census total was captured in this hour.
  hourly_profile <- hourly_bias_combined

  # Temporal Profile Plot (Projector-Ready)
  ggplot(hourly_profile, aes(x = hour, y = coverage_score, color = day_type, fill = day_type)) +
    # 0. Reference line for Census Average (1.0)
    geom_hline(yintercept = 1.0, linetype = "dashed", color = "gray50", linewidth = 1.0) +
    # 1. Distribution Ribbons (50%, 80%, 95% intervals)
    ggdist::stat_lineribbon(
      aes(fill_levels = ordered(after_stat(.width))),
      .width = c(.50, .80, .95),
      alpha = 0.5,
      linewidth = 2.0
    ) +
    geom_line(stat = "summary", fun = median, color = "black", linewidth = 1.2) +
    facet_wrap(~day, ncol = 4) +
    theme_minimal(base_size = 24) +
    scale_y_continuous(
      breaks = seq(0, 10, 1.0), # Fewer labels (every 1.0) to avoid vertical squishing
      limits = c(0, NA),
      expand = expansion(mult = c(0, 0.1))
    ) +
    scale_x_continuous(breaks = c(0, 6, 12, 18, 23)) + # More labels for better orientation
    scale_fill_manual(values = c("Weekday" = "#1F78B4", "Weekend" = "#E31A1C"), name = "Day Type") +
    scale_color_manual(values = c("Weekday" = "#1F78B4", "Weekend" = "#E31A1C"), guide = "none") +
    ggdist::scale_fill_ramp_discrete(name = "Zones Included", labels = c("95%", "80%", "50%"), range = c(0.2, 0.7)) +
    labs(
      title = "Temporal Capture Accuracy",
      subtitle = "Hourly capture intensity vs. census average\n(Dashed line = 1.0 benchmark)",
      x = "Hour of Day (0-23)",
      y = "Coverage Score\n(Ratio to Census)",
      caption = "Data Source: MITMS flows via {spanishoddata} vs. ECEPOV"
    ) +
    theme(
      legend.position = "top",
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(linewidth = 0.3, color = "gray92"),
      strip.background = element_blank(),
      strip.text = element_text(face = "bold", size = 20),
      plot.title = element_text(face = "bold", size = 32),
      plot.subtitle = element_text(size = 20, color = "gray40"),
      axis.title.y = element_text(angle = 0, vjust = 0.5, hjust = 0.5, size = 20, face = "bold", margin = margin(r = 15)),
      axis.title.x = element_text(size = 20, face = "bold"),
      axis.text = element_text(color = "black")
    )
}

#' Plot Daily Bias Distribution (Simple Violin + Boxplot)
plot_daily_bias <- function(daily_bias_combined) {
  days_order <- rev(c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
  daily_bias_combined$day <- factor(daily_bias_combined$day, levels = days_order)

  daily_bias_combined <- daily_bias_combined |>
    dplyr::mutate(day_type = ifelse(day %in% c("Saturday", "Sunday"), "Weekend", "Weekday"))

  max_abs_bias <- max(abs(daily_bias_combined$coverage_bias), na.rm = TRUE)

  ggplot(daily_bias_combined, aes(x = day, y = coverage_bias, fill = day_type)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray60", linewidth = 0.5) +
    # Full violin for simplicity
    geom_violin(alpha = 0.7, trim = FALSE, color = "black", linewidth = 0.4) +
    # Embedded boxplot
    geom_boxplot(width = 0.15, color = "black", fill = "white", alpha = 0.6, outlier.size = 2.5, outlier.alpha = 0.7) +
    coord_flip(ylim = c(-max_abs_bias, max_abs_bias)) +
    theme_minimal(base_size = 24) +
    scale_x_discrete(labels = rev(c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))) +
    scale_y_continuous(n.breaks = 8) +
    scale_fill_manual(values = c("Weekday" = "#1F78B4", "Weekend" = "#E31A1C")) +
    labs(
      title = "Daily Capture Accuracy",
      subtitle = "Relative discrepancy between MITMS trips and census benchmarks",
      y = expression("Coverage bias, " * b[i]),
      x = "Day of\nWeek",
      caption = "Data Source: MITMS flows via {spanishoddata} vs. ECEPOV"
    ) +
    theme(
      legend.position = "none",
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(linewidth = 0.3, color = "gray92"),
      plot.title = element_text(face = "bold", size = 32, vjust = 1, lineheight = 1.1),
      plot.subtitle = element_text(size = 22, color = "gray40", lineheight = 1.1),
      axis.title.y = element_text(angle = 0, vjust = 0.5, hjust = 0.5, size = 22, face = "bold", margin = margin(r = 15)),
      axis.title.x = element_text(size = 22, face = "bold"),
      axis.text = element_text(color = "black")
    )
}

#' Plot Population Coverage Bias (Simple Violin + Boxplot)
plot_population_coverage <- function(pop_coverage_combined) {
  days_order <- rev(c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
  pop_coverage_combined$day <- factor(pop_coverage_combined$day, levels = days_order)
  pop_coverage_combined <- pop_coverage_combined |>
    dplyr::mutate(day_type = ifelse(day %in% c("Saturday", "Sunday"), "Weekend", "Weekday"))

  max_abs_bias <- max(abs(pop_coverage_combined$coverage_bias), na.rm = TRUE)

  ggplot(pop_coverage_combined, aes(x = day, y = coverage_bias, fill = day_type)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray60", linewidth = 0.5) +
    geom_violin(alpha = 0.7, trim = FALSE, color = "black", linewidth = 0.4) +
    geom_boxplot(width = 0.15, color = "black", fill = "white", alpha = 0.6, outlier.size = 2.5, outlier.alpha = 0.7) +
    coord_flip(ylim = c(-max_abs_bias, max_abs_bias)) +
    theme_minimal(base_size = 24) +
    scale_x_discrete(labels = rev(c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))) +
    scale_y_continuous(n.breaks = 8) +
    scale_fill_manual(values = c("Weekday" = "#1F78B4", "Weekend" = "#E31A1C")) +
    labs(
      title = "Population Coverage Bias",
      subtitle = "Penetration gap (MITMS residents vs. census population)",
      y = expression("Coverage bias, " * b[i]),
      x = "Day of\nWeek",
      caption = "Data Source: MITMS overnight stays vs. ineAtlas"
    ) +
    theme(
      legend.position = "none",
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(linewidth = 0.3, color = "gray92"),
      plot.title = element_text(face = "bold", size = 32, vjust = 1, lineheight = 1.1),
      plot.subtitle = element_text(size = 22, color = "gray40", lineheight = 1.1),
      axis.title.y = element_text(angle = 0, vjust = 0.5, hjust = 0.5, size = 22, face = "bold", margin = margin(r = 15)),
      axis.title.x = element_text(size = 22, face = "bold"),
      axis.text = element_text(color = "black")
    )
}

#' Plot Population vs Coverage Bias
plot_pop_vs_coverage_bias <- function(pop_coverage_combined) {
  days_order <- rev(c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
  pop_coverage_combined$day <- factor(pop_coverage_combined$day, levels = days_order)
  pop_coverage_combined <- pop_coverage_combined |>
    dplyr::mutate(day_type = ifelse(day %in% c("Saturday", "Sunday"), "Weekend", "Weekday"))

  ggplot(pop_coverage_combined, aes(x = population, y = coverage_bias)) +
    geom_point(aes(color = day_type), alpha = 0.5) +
    geom_smooth(method = "loess", se = FALSE, color = "black", linewidth = 1.2) +
    scale_x_log10(labels = scales::comma) +
    facet_wrap(~day_type, ncol = 2) +
    theme_minimal(base_size = 24) +
    scale_color_manual(values = c("Weekday" = "#1F78B4", "Weekend" = "#E31A1C"), name = "Day Type") +
    labs(
      title = "Coverage bias by area population size",
      subtitle = "Does coverage bias vary with population size?",
      x = expression("Census population, " * P[i]),
      y = expression("Coverage bias, " * b[i]),
      caption = "Data Source: MITMS overnight stays vs. ineAtlas"
    ) +
    theme(
      legend.position = "none",
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(linewidth = 0.3, color = "gray92"),
      strip.background = element_blank(),
      strip.text = element_text(face = "bold", size = 20),
      plot.title = element_text(face = "bold", size = 32, vjust = 1, lineheight = 1.1),
      plot.subtitle = element_text(size = 22, color = "gray40", lineheight = 1.1),
      axis.title.y = element_text(angle = 0, vjust = 0.5, hjust = 0.5, size = 22, face = "bold", margin = margin(r = 15)),
      axis.title.x = element_text(size = 22, face = "bold"),
      axis.text = element_text(color = "black")
    )
}

#' Plot median coverage bias for every activity filter combination
plot_activity_combo_bias <- function(activity_combo_summary) {
  format_activity <- function(x) {
    tools::toTitleCase(gsub("_", " ", x))
  }

  activity_combo_summary |>
    dplyr::mutate(
      bias_label = dplyr::if_else(
        is.na(median_bias),
        "No trips",
        sprintf("%.2f", median_bias)
      )
    ) |>
    ggplot2::ggplot(
      ggplot2::aes(
        x = activity_destination,
        y = activity_origin,
        fill = median_bias
      )
    ) +
    ggplot2::geom_tile(color = "white", linewidth = 1.2) +
    ggplot2::geom_text(ggplot2::aes(label = bias_label), size = 6, fontface = "bold") +
    ggplot2::scale_x_discrete(labels = format_activity) +
    ggplot2::scale_y_discrete(labels = format_activity) +
    ggplot2::scale_fill_gradient2(
      low = "#D73027",
      mid = "white",
      high = "#4575B4",
      midpoint = 0,
      limits = c(-2, 1),
      oob = scales::squish,
      na.value = "gray90",
      name = "Median bias\n< 0 overrepresented\n> 0 underrepresented"
    ) +
    ggplot2::coord_equal() +
    ggplot2::labs(
      title = "Bias by MITMS Activity Pair",
      subtitle = "Each pair compared with the ECEPOV commuting benchmark",
      x = "Activity at destination",
      y = "Activity at origin",
      caption = "Red: overrepresented | Blue: underrepresented"
    ) +
    ggplot2::theme_minimal(base_size = 22) +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(angle = 30, hjust = 1, color = "black"),
      axis.text.y = ggplot2::element_text(color = "black"),
      axis.title = ggplot2::element_text(face = "bold"),
      plot.title = ggplot2::element_text(face = "bold", size = 28),
      plot.subtitle = ggplot2::element_text(size = 18, color = "gray40"),
      legend.title = ggplot2::element_text(size = 15),
      legend.text = ggplot2::element_text(size = 14)
    )
}
