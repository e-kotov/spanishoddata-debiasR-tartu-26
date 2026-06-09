library(ggplot2)
library(dplyr)
library(ggdist)

#' Plot Daily Flow Coverage Ratio Distribution (Simple Violin + Boxplot)
#'
#' Note: although the underlying column is `coverage_score` (debiasR API), the
#' input here is a *flow* coverage ratio — MPD origin outflows divided by the
#' Census commuter-outflow benchmark — not the population coverage score
#' $p_i = U_i / P_i$ defined in v04 of the debiasR vignettes.
plot_daily_coverage_score <- function(daily_coverage_combined, max_x = 4) {
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
    geom_hline(yintercept = 1, linetype = "dashed", color = "gray25", linewidth = 1.2) +
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
      title = "Daily Flow Coverage Ratio",
      subtitle = expression(r[i] == F[i] / T[i]),
      y = expression("Flow coverage ratio, " * r[i]),
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

  ggplot(pop_coverage_combined, aes(x = day, y = coverage_bias, fill = day_type)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray60", linewidth = 0.5) +
    geom_violin(alpha = 0.7, trim = FALSE, color = "black", linewidth = 0.4) +
    geom_boxplot(width = 0.15, color = "black", fill = "white", alpha = 0.6, outlier.size = 2.5, outlier.alpha = 0.7) +
    coord_flip(ylim = c(-1, 1)) +
    theme_minimal(base_size = 24) +
    scale_x_discrete(labels = rev(c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))) +
    scale_y_continuous(n.breaks = 8) +
    scale_fill_manual(values = c("Weekday" = "#1F78B4", "Weekend" = "#E31A1C")) +
    labs(
      title = "Population Coverage Bias",
      subtitle = NULL,
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

#' Plot daily median flow coverage ratio for every destination-purpose filter set
#'
#' The tile values are *flow* coverage ratios (MPD outflows / Census commuter
#' benchmark per origin), not the population coverage score $p_i$ from v04.
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
      name = "Median Flow Coverage Ratio\n< 1 underrepresented\n> 1 overrepresented"
    ) +
    ggplot2::labs(
      title = "Flow Coverage Ratio by Purpose-Filter Combination",
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

#' Plot Standardized User-Count Residuals (debiasR Level-2 marginal validation)
plot_user_count_residuals <- function(user_count_residuals_combined) {
  days_order <- rev(c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
  user_count_residuals_combined$day <- factor(user_count_residuals_combined$day, levels = days_order)

  user_count_residuals_combined <- user_count_residuals_combined |>
    dplyr::mutate(day_type = ifelse(day %in% c("Saturday", "Sunday"), "Weekend", "Weekday"))

  # Remove NAs
  plot_data <- user_count_residuals_combined |>
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
      title = "Standardized User-Count Residuals (Origin)",
      subtitle = "0 = expected origin coverage",
      y = "Standardized user-count residual",
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

#' Plot Adjustment Comparison (Method Performance)
plot_method_comparison <- function(comparison_df) {
  # Clean up column names if needed
  comparison_df <- as.data.frame(comparison_df)
  
  # Pivot to long format for ggplot
  plot_data <- comparison_df |>
    dplyr::select(method, mae, rmse) |>
    tidyr::pivot_longer(cols = c(mae, rmse), names_to = "metric", values_to = "value") |>
    dplyr::mutate(metric = toupper(metric))

  ggplot(plot_data, aes(x = reorder(method, value), y = value, fill = metric)) +
    geom_col(position = "dodge") +
    coord_flip() +
    theme_minimal(base_size = 20) +
    scale_fill_manual(values = c("MAE" = "#4DAF4A", "RMSE" = "#377EB8")) +
    labs(
      title = "Bias Adjustment Performance",
      subtitle = "Origin-marginal fit (MAE, RMSE)",
      x = "Adjustment Method",
      y = "Error (Trips)",
      fill = "Metric",
      caption = "Lower values indicate better alignment with origin commuter benchmark"
    ) +
    theme(
      legend.position = "top",
      axis.title.y = element_text(face = "bold"),
      axis.title.x = element_text(face = "bold"),
      plot.title = element_text(face = "bold", size = 24)
    )
}

#' Map of areas with |median coverage_bias| > threshold
#'
#' @param pop_coverage_combined Combined population-coverage table with `day`,
#'   `origin`, `coverage_bias`.
#' @param zones_raw sf object with `id` and geometry.
#' @param day_type "Weekday" or "Weekend".
#' @param threshold Absolute median coverage_bias cutoff.
plot_outlier_map <- function(pop_coverage_combined, zones_raw,
                             day_type = c("Weekday", "Weekend"),
                             threshold = 0.3) {
  day_type <- match.arg(day_type)
  weekend_days <- c("Saturday", "Sunday")

  # Spain-only zones (drop FR/PT cross-border municipalities included upstream).
  # esp_move_can() shifts every geometry in its input, so apply it only to the
  # Canary subset. Canary provinces use INE codes 35 (Las Palmas) and 38 (Santa
  # Cruz de Tenerife). Work in EPSG:4258 so the can_box inset stays rectilinear.
  zones_es_all <- zones_raw |>
    dplyr::filter(grepl("^[0-9]", id)) |>
    sf::st_transform(4258)
  is_canary <- grepl("^(35|38)", zones_es_all$id)
  zones_can <- mapSpain::esp_move_can(zones_es_all[is_canary, ])
  zones_es <- rbind(zones_es_all[!is_canary, ], zones_can)
  can_box <- mapSpain::esp_get_can_box()

  per_origin <- pop_coverage_combined |>
    dplyr::mutate(.dt = ifelse(day %in% weekend_days, "Weekend", "Weekday")) |>
    dplyr::filter(.dt == !!day_type) |>
    dplyr::summarise(
      peak_bias = coverage_bias[which.max(abs(coverage_bias))],
      .by = origin
    ) |>
    dplyr::filter(grepl("^[0-9]", origin))

  outliers <- per_origin |>
    dplyr::filter(abs(peak_bias) > !!threshold)

  zones_outliers <- zones_es |>
    dplyr::inner_join(outliers, by = c("id" = "origin"))

  ggplot() +
    geom_sf(data = zones_es, fill = "gray95", color = "gray80", linewidth = 0.05) +
    geom_sf(data = can_box, color = "gray40", linewidth = 0.3) +
    geom_sf(data = zones_outliers, aes(fill = peak_bias), color = "black", linewidth = 0.2) +
    scale_fill_gradient2(
      low = "#2166AC", mid = "white", high = "#B2182B",
      midpoint = 0, limits = c(-1, 1),
      name = expression("Peak " * b[i])
    ) +
    labs(
      title = paste0(day_type, " outliers"),
      subtitle = bquote("Areas with |peak " * b[i] * "| > " * .(threshold) *
                          " (" * .(nrow(outliers)) * " of " * .(nrow(per_origin)) * ")")
    ) +
    theme_minimal(base_size = 20) +
    theme(
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      panel.grid = element_blank(),
      plot.title = element_text(face = "bold", size = 28),
      plot.subtitle = element_text(size = 18, color = "gray40"),
      legend.position = "right"
    )
}

#' Plot Marginal Adjustment Diagnostic Scatter (Origin Totals)
plot_margin_diagnostic <- function(mpd_combined, adj_combined, benchmark_clean) {
  # Aggregate raw MPD to origin margins
  raw_margins <- mpd_combined |>
    dplyr::group_by(origin) |>
    dplyr::summarise(compared = sum(flow, na.rm = TRUE), .groups = "drop") |>
    dplyr::inner_join(benchmark_clean, by = "origin") |>
    dplyr::transmute(
      origin,
      reference = target,
      compared,
      type = "Raw vs Benchmark"
    )
    
  # Aggregate Adjusted to origin margins
  adj_margins <- adj_combined |>
    dplyr::group_by(origin) |>
    dplyr::summarise(compared = sum(flow_adj, na.rm = TRUE), .groups = "drop") |>
    dplyr::inner_join(benchmark_clean, by = "origin") |>
    dplyr::transmute(
      origin,
      reference = target,
      compared,
      type = "Adjusted vs Benchmark"
    )
    
  plot_data <- dplyr::bind_rows(raw_margins, adj_margins) |>
    dplyr::mutate(difference = compared - reference)
    
  # Determine limits for squared plot
  max_val <- max(c(plot_data$reference, plot_data$compared), na.rm = TRUE)
    
  ggplot(plot_data, aes(x = reference, y = compared, color = difference)) +
    geom_point(alpha = 0.6, size = 2) +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "#D95F0E", linewidth = 1) +
    facet_wrap(~type) +
    scale_color_gradient2(
      low = "#2166AC", mid = "gray88", high = "#B2182B", 
      midpoint = 0, name = "Residual\n(Trips)"
    ) +
    scale_x_continuous(labels = scales::comma, limits = c(0, max_val)) +
    scale_y_continuous(labels = scales::comma, limits = c(0, max_val)) +
    coord_fixed() +
    theme_minimal(base_size = 18) +
    labs(
      title = "Origin Marginal Validation",
      subtitle = "Total Generated Trips: Observed (Y) vs. Census Benchmark (X)",
      x = "Census Benchmark (Total Commuters)",
      y = "Mobile Data (Total Outbound Flow)",
      caption = "Diagonal line represents perfect match. Points below the line indicate under-coverage."
    ) +
    theme(strip.text = element_text(face = "bold", size = 16))
}

#' Plot Level 3 Residual Heatmap (Origin Marginals)
plot_level3_heatmap <- function(mpd_combined, target_adj_raking_combined, target_adj_inv_pen_combined, target_adj_selection_income_combined, target_adj_selection_mean_age_combined, target_adj_selection_pct_under18_combined, target_adj_selection_pct_over65_combined, target_adj_selection_pct_spanish_combined, target_adj_selection_gini_combined, target_adj_multilevel_combined, benchmark_clean) {
  
  aggregate_to_origins <- function(df, flow_col = "flow_adj") {
    df |>
      dplyr::group_by(origin) |>
      dplyr::summarise(flow_adj = sum(.data[[flow_col]], na.rm = TRUE), .groups = "drop") |>
      dplyr::mutate(origin = as.character(origin), destination = origin)
  }
  
  mpd_origin <- aggregate_to_origins(mpd_combined, "flow") |> dplyr::rename(flow_mpd = flow_adj)
  adj_raking <- aggregate_to_origins(target_adj_raking_combined)
  adj_inv_pen <- aggregate_to_origins(target_adj_inv_pen_combined)
  adj_sel_inc <- aggregate_to_origins(target_adj_selection_income_combined)
  adj_sel_age <- aggregate_to_origins(target_adj_selection_mean_age_combined)
  adj_sel_u18 <- aggregate_to_origins(target_adj_selection_pct_under18_combined)
  adj_sel_o65 <- aggregate_to_origins(target_adj_selection_pct_over65_combined)
  adj_sel_spa <- aggregate_to_origins(target_adj_selection_pct_spanish_combined)
  adj_sel_gin <- aggregate_to_origins(target_adj_selection_gini_combined)
  adj_multi <- aggregate_to_origins(target_adj_multilevel_combined)
  
  bench_origin <- benchmark_clean |>
    dplyr::transmute(
      origin = as.character(origin),
      destination = origin,
      flow_bench = as.numeric(target)
    )
    
  # Create a combined comparison dataset
  build_method_comparison <- function(adj_df, method_name) {
    adj_df |>
      dplyr::inner_join(mpd_origin, by = c("origin", "destination")) |>
      dplyr::inner_join(bench_origin, by = c("origin", "destination")) |>
      dplyr::mutate(method_label = method_name)
  }
  
  flow_comparison <- dplyr::bind_rows(
    build_method_comparison(adj_raking, "Raking Ratio"),
    build_method_comparison(adj_inv_pen, "Inverse Penetration"),
    build_method_comparison(adj_sel_inc, "Sel. Rate (Net Income)"),
    build_method_comparison(adj_sel_age, "Sel. Rate (Mean Age)"),
    build_method_comparison(adj_sel_u18, "Sel. Rate (% <18)"),
    build_method_comparison(adj_sel_o65, "Sel. Rate (% >65)"),
    build_method_comparison(adj_sel_spa, "Sel. Rate (% Native)"),
    build_method_comparison(adj_sel_gin, "Sel. Rate (Gini)"),
    build_method_comparison(adj_multi, "Multilevel Model")
  )
  
  # Calculate standard deviation of residuals
  adjusted_reference_sd <- flow_comparison |>
    dplyr::mutate(residual = flow_adj - flow_bench) |>
    dplyr::pull(residual) |>
    stats::sd(na.rm = TRUE)
    
  if (!is.finite(adjusted_reference_sd) || adjusted_reference_sd <= 0) {
    adjusted_reference_sd <- 1
  }

  raw_reference <- flow_comparison |>
    dplyr::select(origin, destination, flow_mpd, flow_bench) |>
    dplyr::distinct() |>
    dplyr::transmute(
      method_label = "Unadjusted (Raw MPD)",
      residual = flow_mpd - flow_bench
    )
    
  adj_reference <- flow_comparison |>
    dplyr::transmute(
      method_label = method_label,
      residual = flow_adj - flow_bench
    )
    
  residual_data <- dplyr::bind_rows(raw_reference, adj_reference) |>
    dplyr::mutate(
      residual_sd_score = abs(residual) / adjusted_reference_sd,
      residual_band = dplyr::case_when(
        residual_sd_score > 4 ~ "Greater than 4.0 SD",
        residual_sd_score > 3 ~ "3.0 to 4.0 SD",
        residual_sd_score > 2 ~ "2.0 to 3.0 SD",
        TRUE ~ "Less than 2.0 SD"
      )
    )

  method_levels <- c("Unadjusted (Raw MPD)", "Raking Ratio", "Inverse Penetration", "Sel. Rate (Net Income)", "Sel. Rate (Mean Age)", "Sel. Rate (% <18)", "Sel. Rate (% >65)", "Sel. Rate (% Native)", "Sel. Rate (Gini)", "Multilevel Model")
  band_levels <- c("Less than 2.0 SD", "2.0 to 3.0 SD", "3.0 to 4.0 SD", "Greater than 4.0 SD")

  heatmap_data <- residual_data |>
    dplyr::count(method_label, residual_band, name = "n") |>
    dplyr::group_by(method_label) |>
    dplyr::mutate(share = 100 * n / sum(n)) |>
    dplyr::ungroup()
    
  complete_grid <- expand.grid(
    method_label = method_levels,
    residual_band = band_levels,
    stringsAsFactors = FALSE
  )
  
  heatmap_data <- complete_grid |>
    dplyr::left_join(heatmap_data, by = c("method_label", "residual_band")) |>
    dplyr::mutate(
      n = dplyr::if_else(is.na(n), 0L, n),
      share = dplyr::if_else(is.na(share), 0, share),
      method_label = factor(method_label, levels = method_levels),
      residual_band = factor(residual_band, levels = band_levels),
      label = sprintf("%.2f", share)
    )

  ggplot2::ggplot(
    heatmap_data,
    ggplot2::aes(x = method_label, y = residual_band, fill = share)
  ) +
    ggplot2::geom_tile(width = 0.92, height = 0.92, colour = NA) +
    ggplot2::geom_text(ggplot2::aes(label = label), colour = "black", size = 4) +
    ggplot2::scale_fill_gradient(
      low = "#F7F7F7",
      high = "#08519C",
      name = "Share (%)"
    ) +
    ggplot2::scale_x_discrete(position = "bottom", guide = ggplot2::guide_axis(n.dodge = 2)) +
    ggplot2::labs(
      x = NULL,
      y = "Residual Standard Deviation Band",
      title = "Origin-Marginal Residual Heatmap",
      subtitle = "Percentage of origins in each residual standard deviation band"
    ) +
    ggplot2::theme_minimal(base_size = 14) +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      panel.border = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_text(colour = "grey45", face = "bold"),
      axis.text.x = ggplot2::element_text(colour = "grey20", face = "bold"),
      axis.ticks = ggplot2::element_blank(),
      legend.position = "none"
    )
}

#' Plot Level 3 Residual Stacked Bar (Origin Marginals)
plot_level3_stacked_bar <- function(mpd_combined, target_adj_raking_combined, target_adj_inv_pen_combined, target_adj_selection_income_combined, target_adj_selection_mean_age_combined, target_adj_selection_pct_under18_combined, target_adj_selection_pct_over65_combined, target_adj_selection_pct_spanish_combined, target_adj_selection_gini_combined, target_adj_multilevel_combined, benchmark_clean, highlight_methods = NULL) {
  
  aggregate_to_origins <- function(df, flow_col = "flow_adj") {
    df |>
      dplyr::group_by(origin) |>
      dplyr::summarise(flow_adj = sum(.data[[flow_col]], na.rm = TRUE), .groups = "drop") |>
      dplyr::mutate(origin = as.character(origin), destination = origin)
  }
  
  mpd_origin <- aggregate_to_origins(mpd_combined, "flow") |> dplyr::rename(flow_mpd = flow_adj)
  adj_raking <- aggregate_to_origins(target_adj_raking_combined)
  adj_inv_pen <- aggregate_to_origins(target_adj_inv_pen_combined)
  adj_sel_inc <- aggregate_to_origins(target_adj_selection_income_combined)
  adj_sel_age <- aggregate_to_origins(target_adj_selection_mean_age_combined)
  adj_sel_u18 <- aggregate_to_origins(target_adj_selection_pct_under18_combined)
  adj_sel_o65 <- aggregate_to_origins(target_adj_selection_pct_over65_combined)
  adj_sel_spa <- aggregate_to_origins(target_adj_selection_pct_spanish_combined)
  adj_sel_gin <- aggregate_to_origins(target_adj_selection_gini_combined)
  adj_multi <- aggregate_to_origins(target_adj_multilevel_combined)
  
  bench_origin <- benchmark_clean |>
    dplyr::transmute(
      origin = as.character(origin),
      destination = origin,
      flow_bench = as.numeric(target)
    )
    
  # Create a combined comparison dataset
  build_method_comparison <- function(adj_df, method_name) {
    adj_df |>
      dplyr::inner_join(mpd_origin, by = c("origin", "destination")) |>
      dplyr::inner_join(bench_origin, by = c("origin", "destination")) |>
      dplyr::mutate(method_label = method_name)
  }
  
  flow_comparison <- dplyr::bind_rows(
    build_method_comparison(adj_raking, "Raking Ratio"),
    build_method_comparison(adj_inv_pen, "Inverse Penetration"),
    build_method_comparison(adj_sel_inc, "Sel. Rate (Net Income)"),
    build_method_comparison(adj_sel_age, "Sel. Rate (Mean Age)"),
    build_method_comparison(adj_sel_u18, "Sel. Rate (% <18)"),
    build_method_comparison(adj_sel_o65, "Sel. Rate (% >65)"),
    build_method_comparison(adj_sel_spa, "Sel. Rate (% Native)"),
    build_method_comparison(adj_sel_gin, "Sel. Rate (Gini)"),
    build_method_comparison(adj_multi, "Multilevel Model")
  )
  
  # Calculate standard deviation of Raking Ratio residuals as reference SD
  adjusted_reference_sd <- flow_comparison |>
    dplyr::filter(method_label == "Raking Ratio") |>
    dplyr::mutate(residual = flow_adj - flow_bench) |>
    dplyr::pull(residual) |>
    stats::sd(na.rm = TRUE)
    
  if (!is.finite(adjusted_reference_sd) || adjusted_reference_sd <= 0) {
    adjusted_reference_sd <- 1
  }

  raw_reference <- flow_comparison |>
    dplyr::select(origin, destination, flow_mpd, flow_bench) |>
    dplyr::distinct() |>
    dplyr::transmute(
      method_label = "Unadjusted (Raw MPD)",
      residual = flow_mpd - flow_bench
    )
    
  adj_reference <- flow_comparison |>
    dplyr::transmute(
      method_label = method_label,
      residual = flow_adj - flow_bench
    )
    
  residual_data <- dplyr::bind_rows(raw_reference, adj_reference) |>
    dplyr::mutate(
      residual_sd_score = abs(residual) / adjusted_reference_sd,
      residual_band = dplyr::case_when(
        residual_sd_score > 4 ~ "Greater than 4.0 SD",
        residual_sd_score > 3 ~ "3.0 to 4.0 SD",
        residual_sd_score > 2 ~ "2.0 to 3.0 SD",
        TRUE ~ "Less than 2.0 SD"
      )
    )

  method_levels <- c("Unadjusted (Raw MPD)", "Raking Ratio", "Inverse Penetration", "Sel. Rate (Net Income)", "Sel. Rate (Mean Age)", "Sel. Rate (% <18)", "Sel. Rate (% >65)", "Sel. Rate (% Native)", "Sel. Rate (Gini)", "Multilevel Model")
  band_levels <- c("Less than 2.0 SD", "2.0 to 3.0 SD", "3.0 to 4.0 SD", "Greater than 4.0 SD")

  heatmap_data <- residual_data |>
    dplyr::count(method_label, residual_band, name = "n") |>
    dplyr::group_by(method_label) |>
    dplyr::mutate(share = 100 * n / sum(n)) |>
    dplyr::ungroup()
    
  complete_grid <- expand.grid(
    method_label = method_levels,
    residual_band = band_levels,
    stringsAsFactors = FALSE
  )
  
  heatmap_data <- complete_grid |>
    dplyr::left_join(heatmap_data, by = c("method_label", "residual_band")) |>
    dplyr::mutate(
      n = dplyr::if_else(is.na(n), 0L, n),
      share = dplyr::if_else(is.na(share), 0, share),
      method_label = factor(method_label, levels = method_levels),
      residual_band = factor(residual_band, levels = band_levels),
      label = sprintf("%.1f", share),
      is_highlighted = if (is.null(highlight_methods)) "no" else ifelse(method_label %in% highlight_methods, "yes", "no")
    )

  ggplot2::ggplot(
    heatmap_data,
    ggplot2::aes(x = method_label, y = share, fill = residual_band)
  ) +
    ggplot2::geom_col(ggplot2::aes(colour = is_highlighted), width = 0.8, linewidth = 1.2) +
    ggplot2::geom_text(
      ggplot2::aes(label = ifelse(residual_band == "Less than 2.0 SD", label, "")), 
      position = ggplot2::position_stack(vjust = 0.5), 
      size = 6,
      colour = "grey10"
    ) +
    ggplot2::scale_colour_manual(values = c("yes" = "black", "no" = NA), guide = "none") +
    ggplot2::scale_fill_manual(
      values = c(
        "Less than 2.0 SD" = "#A8D5BA",    # Soft Green (Good)
        "2.0 to 3.0 SD" = "#F9E79F",       # Soft Yellow (Warning)
        "3.0 to 4.0 SD" = "#F5B041",       # Orange (Bad)
        "Greater than 4.0 SD" = "#E74C3C"  # Red (Very Bad)
      ),
      name = "Residual SD Band"
    ) +
    ggplot2::scale_y_continuous(labels = function(x) paste0(x, "%")) +
    ggplot2::scale_x_discrete(limits = rev) + # Reverse so Unadjusted is at the top
    ggplot2::coord_flip() +
    ggplot2::labs(
      x = NULL,
      y = "Share of Municipalities",
      title = "Origin-Marginal Residuals by SD Band",
      subtitle = "Percentage of municipalities falling into each error magnitude category"
    ) +
    ggplot2::theme_minimal(base_size = 20) +
    ggplot2::theme(
      legend.position = "right",
      axis.text.y = ggplot2::element_text(colour = "grey20", face = "bold", size = 16),
      axis.text.x = ggplot2::element_text(colour = "grey20", size = 16),
      plot.title = ggplot2::element_text(face = "bold", size = 24),
      plot.subtitle = ggplot2::element_text(size = 18, colour = "grey30"),
      legend.title = ggplot2::element_text(face = "bold", size = 18),
      legend.text = ggplot2::element_text(size = 16),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_blank() # Clean up horizontal lines
    )
}
