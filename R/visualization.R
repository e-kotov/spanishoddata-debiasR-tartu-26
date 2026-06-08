library(ggplot2)
library(dplyr)
library(ggdist)

#' Plot Hourly Capture Rates across Days (Capture Score Ratio)
plot_hourly_coverage_score <- function(hourly_coverage_combined) {
  # Ensure day is a factor with correct order
  days_order <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
  hourly_coverage_combined$day <- factor(hourly_coverage_combined$day, levels = days_order)

  # Categorize days
  hourly_coverage_combined <- hourly_coverage_combined |>
    dplyr::mutate(day_type = ifelse(day %in% c("Saturday", "Sunday"), "Weekend", "Weekday"))

  # Calculate Capture Score: (Actual Hourly Trips) / (Average Hourly Census Benchmark)
  # 1.0 means exactly 1/24th of the daily census total was captured in this hour.
  hourly_profile <- hourly_coverage_combined

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

#' Plot Daily Coverage Score Distribution (Simple Violin + Boxplot)
plot_daily_coverage_score <- function(daily_coverage_combined, max_x = 2) {
  days_order <- rev(c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
  daily_coverage_combined$day <- factor(daily_coverage_combined$day, levels = days_order)

  daily_coverage_combined <- daily_coverage_combined |>
    dplyr::mutate(day_type = ifelse(day %in% c("Saturday", "Sunday"), "Weekend", "Weekday"))

  if (max_x <= 2) {
    x_breaks <- seq(0, max_x, by = 0.5)
  } else {
    x_breaks <- sort(unique(c(0, 1, seq(0, max_x, by = 2))))
  }

  ggplot(daily_coverage_combined, aes(x = day, y = coverage_score, fill = day_type)) +
    geom_hline(yintercept = 1, linetype = "dashed", color = "gray60", linewidth = 0.5) +
    # Full violin for simplicity
    geom_violin(alpha = 0.7, trim = FALSE, color = "black", linewidth = 0.4) +
    # Embedded boxplot
    geom_boxplot(width = 0.15, color = "black", fill = "white", alpha = 0.6, outlier.size = 2.5, outlier.alpha = 0.7) +
    coord_flip(ylim = c(0, max_x)) +
    theme_minimal(base_size = 24) +
    scale_x_discrete(labels = rev(c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))) +
    scale_y_continuous(breaks = x_breaks) +
    scale_fill_manual(values = c("Weekday" = "#1F78B4", "Weekend" = "#E31A1C")) +
    labs(
      title = "Daily Capture Accuracy",
      subtitle = "Coverage score: MITMS trips divided by census benchmark",
      y = "Coverage Score\n(Ratio to Census)",
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

#' Plot Population vs Coverage Score
plot_pop_vs_coverage_score <- function(pop_coverage_combined) {
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

#' Plot daily median coverage score for every destination-purpose filter set
plot_activity_combo_coverage <- function(activity_combo_coverage_combined, highlight_filter = NULL) {
  days_order <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")

  daily_summary <- activity_combo_coverage_combined |>
    dplyr::summarise(
      median_coverage = stats::median(coverage_score, na.rm = TRUE),
      .by = c(filter_label, filter_size, day)
    ) |>
    dplyr::mutate(
      day = factor(day, levels = days_order),
      filter_label = factor(
        filter_label,
        levels = unique(filter_label[order(filter_size, filter_label)])
      ),
      coverage_label = sprintf("%.2f", median_coverage),
      is_highlight = !is.null(highlight_filter) & filter_label == highlight_filter,
      text_color = ifelse(median_coverage > 1, "white", "black"),
      text_face = ifelse(is_highlight, "bold", "plain")
    )

  y_labels <- function(labels) {
    parse(text = vapply(labels, function(label) {
      quoted_label <- paste0("'", gsub("'", "\\\\'", label), "'")
      if (!is.null(highlight_filter) && label == highlight_filter) {
        paste0("bold(", quoted_label, ")")
      } else {
        quoted_label
      }
    }, character(1)))
  }

  daily_summary |>
    ggplot2::ggplot(
      ggplot2::aes(
        x = day,
        y = filter_label,
        fill = median_coverage
      )
    ) +
    ggplot2::geom_tile(color = "white", linewidth = 1.2) +
    ggplot2::geom_tile(
      data = dplyr::filter(daily_summary, is_highlight),
      fill = NA,
      color = "black",
      linewidth = 2.2
    ) +
    ggplot2::geom_text(
      ggplot2::aes(label = coverage_label, color = text_color, fontface = text_face),
      size = 4.3
    ) +
    ggplot2::scale_color_identity() +
    ggplot2::scale_x_discrete(labels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")) +
    ggplot2::scale_y_discrete(labels = y_labels) +
    ggplot2::scale_fill_gradient2(
      low = "#5E3C99",
      mid = "gray88",
      high = "#E66101",
      midpoint = 1,
      limits = c(0, 2),
      oob = scales::squish,
      na.value = "gray90",
      name = "Median Coverage\n< 1 underrepresented\n> 1 overrepresented"
    ) +
    ggplot2::labs(
      title = "Coverage by Purpose-Filter Combination",
      subtitle = "All destination-purpose subsets; activity at origin = Home",
      x = "Day of week",
      y = "Destination filters",
      caption = "Black outline marks the highlighted filter combination"
    ) +
    ggplot2::theme_minimal(base_size = 20) +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(color = "black"),
      axis.text.y = ggplot2::element_text(color = "black"),
      axis.title = ggplot2::element_text(face = "bold"),
      plot.title = ggplot2::element_text(face = "bold", size = 28),
      plot.subtitle = ggplot2::element_text(size = 18, color = "gray40"),
      plot.title.position = "plot",
      legend.title = ggplot2::element_text(size = 15),
      legend.text = ggplot2::element_text(size = 14)
    )
}

#' Plot Daily Generation Residuals (Standardized User Count)
plot_daily_generation_residuals <- function(generation_residuals_combined) {
  days_order <- rev(c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
  generation_residuals_combined$day <- factor(generation_residuals_combined$day, levels = days_order)

  generation_residuals_combined <- generation_residuals_combined |>
    dplyr::mutate(day_type = ifelse(day %in% c("Saturday", "Sunday"), "Weekend", "Weekday"))

  # Remove NAs
  plot_data <- generation_residuals_combined |>
    dplyr::filter(!is.na(standardized_user_count_residual))

  # Limit y-axis limits to 99th percentile to avoid massive outliers ruining the plot
  y_limit <- stats::quantile(abs(plot_data$standardized_user_count_residual), 0.99, na.rm = TRUE)
  
  ggplot(plot_data, aes(x = day, y = standardized_user_count_residual, fill = day_type)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray60", linewidth = 0.5) +
    # Full violin for simplicity
    geom_violin(alpha = 0.7, trim = FALSE, color = "black", linewidth = 0.4) +
    # Embedded boxplot
    geom_boxplot(width = 0.15, color = "black", fill = "white", alpha = 0.6, outlier.size = 2.5, outlier.alpha = 0.7) +
    coord_flip(ylim = c(-y_limit, y_limit)) +
    theme_minimal(base_size = 24) +
    scale_x_discrete(labels = rev(c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))) +
    scale_fill_manual(values = c("Weekday" = "#1F78B4", "Weekend" = "#E31A1C")) +
    labs(
      title = "Generation Residuals",
      subtitle = "Standardized User Count Residual (0 = Expected Generation)",
      y = "Standardized Residual",
      x = "Day of\nWeek",
      caption = "Data Source: MITMS flows vs. ECEPOV (Origin Totals)"
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
