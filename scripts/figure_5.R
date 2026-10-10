suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
  library(patchwork)
})
source("scripts/common_data.R")

thresholds <- 79:98
output_directory <- "tables/frontier_ribbon_summary"
dir.create(output_directory, recursive = TRUE, showWarnings = FALSE)

curve_variables <- c(
  "climate", "ENFLOPCT", "PCRDGPCD", "WEBAGEFF", "WEBAGTR1",
  "WEBAUGPCT"
)

results <- read_primary_results(fields = "SINYRGWR", years = 2060)
stopifnot(
  nrow(results) == 24300L,
  uniqueN(results$scenario) == 24300L,
  all(results[, .N, by = scenario]$N == 1L)
)

results[, climate := fcase(
  COCLMADJ == 100L & SVCLMADJ == 105L, "Reference river flows",
  COCLMADJ == 70L & SVCLMADJ == 85L, "Reduced river flows",
  default = NA_character_
)]
stopifnot(!anyNA(results$climate))

# Join the exact regional POPUSED total for each experimental WEBPOPGR level.
# The growth_0 lookup record is excluded because it is not part of this experiment.
population_lookup <- read_population_lookup()
population_lookup <- population_lookup[WEBPOPGR %in% unique(results$WEBPOPGR)]
stopifnot(
  nrow(population_lookup) == 75L,
  uniqueN(population_lookup$WEBPOPGR) == 75L,
  identical(sort(population_lookup$WEBPOPGR), seq(2L, 150L, 2L))
)
results <- population_lookup[results, on = "WEBPOPGR"]
stopifnot(nrow(results) == 24300L, !anyNA(results$population))
setorderv(results, c(curve_variables, "WEBPOPGR"))

# Every climate-policy curve must contain the complete population experiment.
curve_completeness <- results[, .(
  rows = .N,
  population_levels = uniqueN(WEBPOPGR),
  complete = identical(WEBPOPGR, seq(2L, 150L, 2L))
), by = curve_variables]
stopifnot(
  nrow(curve_completeness) == 324L,
  all(curve_completeness$rows == 75L),
  all(curve_completeness$population_levels == 75L),
  all(curve_completeness$complete)
)

# Calculate one threshold frontier per complete curve. Censored observations retain
# their class but are never assigned a population boundary for quantile calculation.
frontiers <- results[, rbindlist(lapply(thresholds, function(threshold) {
  qualifying <- which(value >= threshold)
  below_range <- length(qualifying) == 0L
  above_range <- !below_range && value[.N] >= threshold
  frontier_index <- if (below_range) NA_integer_ else qualifying[[length(qualifying)]]
  data.table(
    gsi_threshold = threshold,
    frontier_population = if (below_range || above_range) {
      NA_real_
    } else {
      as.numeric(population[frontier_index])
    },
    boundary_class = if (below_range) {
      "Below tested range"
    } else if (above_range) {
      "Above tested range"
    } else {
      "Observed within tested range"
    }
  )
})), by = curve_variables]
stopifnot(nrow(frontiers) == 324L * length(thresholds))

factor_specifications <- list(
  list(variable = "climate", group = "Climate conditions", panel = "A",
       levels = c("Reference river flows", "Reduced river flows")),
  list(variable = "ENFLOPCT", group = "Environmental-flow allocation", panel = "B",
       levels = c("0", "50")),
  list(variable = "PCRDGPCD", group = "Municipal water-use efficiency", panel = "C",
       levels = c("0", "29", "58")),
  list(variable = "WEBAGEFF", group = "Agricultural water-use efficiency", panel = "D",
       levels = c("100", "90", "80")),
  list(variable = "WEBAGTR1", group = "Agricultural-to-urban water transfer", panel = "E",
       levels = c("0", "50", "100")),
  list(variable = "WEBAUGPCT", group = "Supply augmentation", panel = "F",
       levels = c("0", "10", "20"))
)

reader_label <- function(variable, value) {
  mappings <- list(
    climate = c(
      "Reference river flows" = "Reference river flows",
      "Reduced river flows" = "Reduced river flows"
    ),
    ENFLOPCT = c("0" = "0 AF", "50" = "80,000 AF"),
    PCRDGPCD = c("0" = "3.2% reduction", "29" = "25% reduction", "58" = "45% reduction"),
    WEBAGEFF = c("100" = "Current (2015)", "90" = "10% below 2015", "80" = "20% below 2015"),
    WEBAGTR1 = c("0" = "0%", "50" = "50%", "100" = "100%"),
    WEBAUGPCT = c("0" = "0%", "10" = "10%", "20" = "20%")
  )
  unname(mappings[[variable]][as.character(value)])
}

# R's default type-7 quantile uses h = 1 + (n - 1) * p. This function
# determines whether every required order statistic is an observed frontier.
quantile_status <- function(n_below, n_observed, n_above, probability) {
  n <- n_below + n_observed + n_above
  h <- 1 + (n - 1) * probability
  ranks <- unique(c(floor(h), ceiling(h)))
  categories <- vapply(ranks, function(rank) {
    if (rank <= n_below) return("below_tested_range")
    if (rank <= n_below + n_observed) return("exact")
    "above_tested_range"
  }, character(1))
  if (length(unique(categories)) == 1L) categories[[1]] else
    "not_exactly_identifiable"
}

# Calculate an exact type-7 quantile from its full-distribution ranks. The
# observed vector excludes censored values; n_below supplies the rank offset.
exact_quantile <- function(observed_values, n_below, n_above, probability) {
  observed_values <- sort(observed_values)
  n_observed <- length(observed_values)
  status <- quantile_status(n_below, n_observed, n_above, probability)
  if (status != "exact") return(NA_real_)

  n <- n_below + n_observed + n_above
  h <- 1 + (n - 1) * probability
  lower_rank <- floor(h)
  upper_rank <- ceiling(h)
  fraction <- h - lower_rank
  lower_value <- observed_values[[lower_rank - n_below]]
  upper_value <- observed_values[[upper_rank - n_below]]
  (1 - fraction) * lower_value + fraction * upper_value
}

long_frontiers <- rbindlist(lapply(seq_along(factor_specifications), function(index) {
  specification <- factor_specifications[[index]]
  variable <- specification$variable
  frontiers[, .(
    panel = specification$panel,
    factor_order = index,
    factor_group = specification$group,
    factor_level_raw = as.character(get(variable)),
    factor_level = reader_label(variable, get(variable)),
    level_order = match(as.character(get(variable)), specification$levels),
    gsi_threshold,
    frontier_population,
    boundary_class
  )]
}))

summary_data <- long_frontiers[, {
  observed <- frontier_population[boundary_class == "Observed within tested range"]
  n_below <- sum(boundary_class == "Below tested range")
  n_above <- sum(boundary_class == "Above tested range")
  n_observed <- length(observed)
  .(
    n_background_combinations = .N,
    n_observed_frontiers = n_observed,
    n_above_range = n_above,
    n_below_range = n_below,
    median_status = quantile_status(n_below, n_observed, n_above, 0.50),
    q25_status = quantile_status(n_below, n_observed, n_above, 0.25),
    q75_status = quantile_status(n_below, n_observed, n_above, 0.75),
    q25_population = exact_quantile(observed, n_below, n_above, 0.25),
    median_population = exact_quantile(observed, n_below, n_above, 0.50),
    q75_population = exact_quantile(observed, n_below, n_above, 0.75)
  )
}, by = .(
  panel, factor_order, factor_group, level_order, factor_level,
  factor_level_raw, gsi_threshold
)]

summary_data[, `:=`(
  q25_population_millions = q25_population / 1e6,
  median_population_millions = median_population / 1e6,
  q75_population_millions = q75_population / 1e6
)]

summary_data[, fully_identifiable :=
  median_status == "exact" & q25_status == "exact" & q75_status == "exact"]

stopifnot(
  nrow(summary_data) == 16L * length(thresholds),
  all(summary_data$fully_identifiable),
  !anyNA(summary_data$q25_population_millions),
  !anyNA(summary_data$median_population_millions),
  !anyNA(summary_data$q75_population_millions),
  all(summary_data$n_background_combinations %in% c(108L, 162L)),
  all(summary_data$n_observed_frontiers + summary_data$n_above_range +
        summary_data$n_below_range == summary_data$n_background_combinations)
)

# Confirm independently that 79:98 is precisely within the previously audited
# all-level fully identifiable interval.
all_threshold_diagnostic <- fread(
  "internal_validation/frontier_bounds/tables/frontier_identifiability_all_thresholds.csv"
)
prior_check <- all_threshold_diagnostic[
  GSI_threshold %in% thresholds,
  .(all_fully_observed = all(identifiability_class == "fully observed"), .N),
  by = GSI_threshold
]
stopifnot(
  nrow(prior_check) == length(thresholds),
  all(prior_check$N == 16L),
  all(prior_check$all_fully_observed)
)

setorder(summary_data, factor_order, level_order, gsi_threshold)

summary_export <- summary_data[, .(
  factor_group, factor_level, gsi_threshold,
  q25_population_millions, median_population_millions,
  q75_population_millions, n_background_combinations,
  n_observed_frontiers, n_above_range, n_below_range
)]
fwrite(summary_export, file.path(output_directory, "frontier_ribbon_summary.csv"))

diagnostic_export <- summary_data[, .(
  factor_group, factor_level, gsi_threshold,
  n_background_combinations, n_observed_frontiers, n_above_range,
  n_below_range, median_status, q25_status, q75_status,
  fully_identifiable
)]
fwrite(
  diagnostic_export,
  file.path(output_directory, "frontier_ribbon_identifiability.csv")
)

# Every curve and every exact quantile summary should be non-increasing as the
# minimum GSI threshold rises. Report any exception instead of smoothing it.
line_audit <- summary_data[order(gsi_threshold), .(
  median_non_increasing = all(diff(median_population) <= 0),
  q25_non_increasing = all(diff(q25_population) <= 0),
  q75_non_increasing = all(diff(q75_population) <= 0),
  maximum_median_increase = max(c(0, diff(median_population)))
), by = .(factor_group, factor_level)]
fwrite(line_audit, file.path(output_directory, "frontier_ribbon_line_audit.csv"))

if (any(!line_audit$median_non_increasing)) {
  warning("At least one factor-level median line increases with a stricter threshold.")
}

y_limits <- range(
  c(summary_data$q25_population_millions, summary_data$q75_population_millions)
)
y_breaks <- pretty(y_limits, n = 6)

make_panel <- function(panel_code) {
  plot_data <- copy(summary_data[panel == panel_code])
  level_labels <- unique(plot_data[order(level_order)]$factor_level)
  plot_data[, factor_level := factor(factor_level, levels = level_labels)]

  ggplot(
    plot_data,
    aes(
      x = gsi_threshold, y = median_population_millions,
      color = factor_level, fill = factor_level, group = factor_level
    )
  ) +
    geom_ribbon(
      aes(ymin = q25_population_millions, ymax = q75_population_millions),
      alpha = 0.12, color = NA
    ) +
    geom_line(linewidth = 1.3) +
    scale_color_viridis_d(option = "plasma", begin = 0.10, end = 0.80) +
    scale_fill_viridis_d(option = "plasma", begin = 0.10, end = 0.80) +
    scale_x_continuous(breaks = c(79, 85, 90, 95, 98), limits = c(79, 98)) +
    scale_y_continuous(limits = range(y_breaks), breaks = y_breaks) +
    labs(
      title = paste0(panel_code, ". ", plot_data$factor_group[[1]]),
      x = "Minimum GSI threshold (years)",
      y = "Maximum tested population meeting GSI threshold (millions)",
      color = NULL, fill = NULL
    ) +
    theme_classic(base_size = 14) +
    theme(
      plot.title = element_text(face = "plain", size = 14),
      axis.text = element_text(size = 12),
      axis.title.x = element_text(size = 14, margin = margin(t = 16)),
      axis.title.y = element_text(size = 14, margin = margin(r = 16)),
      legend.position = "inside",
      legend.position.inside = c(0.98, 0.98),
      legend.justification = c(1, 1),
      legend.direction = "vertical",
      legend.background = element_blank(),
      legend.box.background = element_blank(),
      legend.margin = margin(3, 5, 3, 5),
      legend.text = element_text(size = 12),
      legend.key.width = unit(0.42, "in"),
      legend.key.height = unit(0.18, "in"),
      panel.border = element_rect(
        fill = NA, color = "black", linewidth = 0.55
      )
    )
}

figure <- wrap_plots(lapply(LETTERS[1:6], make_panel), ncol = 2) +
  plot_layout(guides = "keep", axes = "collect", axis_titles = "collect")

ggsave(
  "figures/figure_5.png", figure,
  width = 12, height = 11.5, dpi = 300, bg = "white"
)

cat("\nFRONTIER RIBBON QA\n")
cat("Complete population-GSI curves:", nrow(curve_completeness), "\n")
cat("Plotted factor-level summaries:", nrow(summary_data), "\n")
cat("Fully identifiable summaries:", sum(summary_data$fully_identifiable), "\n")
cat("Factor-level median lines with an increase:",
    sum(!line_audit$median_non_increasing), "\n")
print(summary_data[, .(
  observed = sum(n_observed_frontiers),
  above_range = sum(n_above_range),
  below_range = sum(n_below_range)
), by = gsi_threshold])
