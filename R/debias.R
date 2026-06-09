#' Merge MPD flows with origin-marginal benchmark for coverage-bias measurement
#'
#' The benchmark is ECEPOV "Total Commuters by Origin" — a flow marginal, not a
#' population count. `measure_bias()` requires the column to be named
#' `population`, so we satisfy that contract while keeping the math honest:
#' the resulting `coverage_score = U_i / P_i` is a **flow coverage ratio**
#' (MPD outflows / Census commuter outflow benchmark), not the canonical
#' population coverage score (which is computed separately in
#' `merge_population_coverage()`).
merge_datasets <- function(mpd_clean, benchmark_clean) {
  bench_df <- benchmark_clean |>
    dplyr::select(origin, target)

  user_df <- mpd_clean |>
    dplyr::group_by(origin) |>
    dplyr::summarise(user_count = sum(flow, na.rm = TRUE), .groups = "drop")

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

#' Measure coverage bias via debiasR
#'
#' Wraps `debiasR::measure_bias()`. Returns canonical columns
#' `coverage_score = U_i/P_i` and `coverage_bias = 1 - U_i/P_i`.
measure_coverage_bias <- function(merged_dataset_or_df) {
  coverage_df <- if (is.data.frame(merged_dataset_or_df)) merged_dataset_or_df else merged_dataset_or_df$coverage
  debiasR::measure_bias(coverage_df)
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

#' Merge and compute population coverage (debiasR Vignette v04)
merge_population_coverage <- function(mitms_pop_raw, census_population) {
  coverage_df <- census_population |>
    dplyr::inner_join(mitms_pop_raw, by = "origin") |>
    dplyr::rename(user_count = mitms_resident_count) |>
    dplyr::mutate(mpd_source = "MITMS")

  debiasR::measure_bias(coverage_df)
}

#' Standardized user-count residuals (origin-marginal, debiasR canonical name)
measure_user_count_residuals <- function(merged_dataset_or_df) {
  coverage_df <- if (is.data.frame(merged_dataset_or_df)) merged_dataset_or_df else merged_dataset_or_df$coverage
  res <- debiasR::validate_bias_residual_structure(
    coverage_df = coverage_df,
    coverage_area_col = "origin",
    population_col = "population",
    user_count_col = "user_count"
  )
  res$area_level
}

#' Build an origin pseudo-OD benchmark (destination = origin) for adjustment APIs
#'
#' The `adjust_selection_rate()` API requires a `benchmark_od_df` with origin AND
#' destination columns. With marginal-origin data only, we encode the benchmark
#' as a degenerate diagonal table; with `calibration_aggregate = "origin"` the
#' function collapses to origin margins and the destination column is unused.
#' This is **not** a real OD benchmark — never validate OD-cell predictions
#' against it. See `validate_origin_margin()` for the correct validation scope.
make_origin_pseudo_od <- function(benchmark_clean) {
  benchmark_clean |>
    dplyr::transmute(
      origin = as.character(origin),
      destination = as.character(origin),
      flow = as.numeric(target)
    )
}

#' Raking-ratio adjustment against origin-marginal targets
#'
#' Canonical debiasR method for marginal-origin-only benchmarks
#' (`adjust_raking_ratio()` accepts `origin_targets` directly; no OD benchmark
#' required). Returns an OD-level adjusted-flow table.
adjust_origin_flows_raking <- function(mpd_od_df, benchmark_clean) {
  origin_targets <- benchmark_clean |>
    dplyr::transmute(
      origin = as.character(origin),
      target = as.numeric(target)
    )
  debiasR::adjust_raking_ratio(
    mpd_od_df = mpd_od_df,
    origin_targets = origin_targets
  )
}

#' Inverse-penetration adjustment using residential coverage at origins
#'
#' Uses true population counts at the origin (residents vs. census population)
#' as the penetration source. Per v06 vignette: "useful first adjustment when
#' the main known source of bias is uneven population coverage."
adjust_origin_flows_inverse_penetration <- function(mpd_od_df, pop_coverage_df) {
  cov_df <- pop_coverage_df |>
    dplyr::select(origin, population, user_count) |>
    dplyr::mutate(
      origin = as.character(origin),
      mpd_source = "MITMS"
    )
  debiasR::adjust_inverse_penetration(
    mpd_od_df = mpd_od_df,
    coverage_df = cov_df,
    weight_by = "origin"
  )
}

#' Validate adjusted flows against the origin-marginal benchmark
#'
#' Aggregates adjusted OD flows to origin margins, then calls
#' `validate_flow_overall()` on origin-margin vs. origin-margin (encoded as a
#' diagonal table so the OD-shape API accepts it). The reported Pearson/RMSE/MAE
#' therefore reflect origin-marginal fit — the only fit scope this project's
#' benchmark supports.
validate_origin_margin <- function(adj_df, benchmark_clean, method_name,
                                   flow_col_adj = "flow_adj") {
  adj_origin <- adj_df |>
    dplyr::group_by(origin) |>
    dplyr::summarise(
      flow_adj = sum(.data[[flow_col_adj]], na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      origin = as.character(origin),
      destination = origin
    )

  bench_origin <- benchmark_clean |>
    dplyr::transmute(
      origin = as.character(origin),
      destination = origin,
      flow = as.numeric(target)
    )

  debiasR::validate_flow_overall(
    adj_df = adj_origin,
    benchmark_od_df = bench_origin,
    method_name = method_name
  )
}
