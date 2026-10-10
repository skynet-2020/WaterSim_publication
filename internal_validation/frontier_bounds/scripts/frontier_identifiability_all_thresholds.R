suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
})
source("scripts/common_data.R")

output_directory <- "internal_validation/frontier_bounds/tables"
dir.create(output_directory, recursive = TRUE, showWarnings = FALSE)

results <- read_primary_results(fields = "SINYRGWR", years = 2060)
thresholds <- seq(min(results$value), max(results$value))
curve_variables <- c(
  "climate", "ENFLOPCT", "PCRDGPCD", "WEBAGEFF", "WEBAGTR1",
  "WEBAUGPCT"
)

stopifnot(
  identical(thresholds, 45:100),
  nrow(results) == 24300L,
  uniqueN(results$scenario) == 24300L
)

results[, climate := fcase(
  COCLMADJ == 100L & SVCLMADJ == 105L, "Reference river flows",
  COCLMADJ == 70L & SVCLMADJ == 85L, "Reduced river flows",
  default = NA_character_
)]
stopifnot(!anyNA(results$climate))

population_lookup <- read_population_lookup()
population_lookup <- population_lookup[WEBPOPGR %in% unique(results$WEBPOPGR)]
stopifnot(
  nrow(population_lookup) == 75L,
  identical(sort(population_lookup$WEBPOPGR), seq(2L, 150L, 2L))
)
results <- population_lookup[results, on = "WEBPOPGR"]
stopifnot(!anyNA(results$population))
setorderv(results, c(curve_variables, "WEBPOPGR"))

curve_check <- results[, .(
  rows = .N,
  complete = identical(WEBPOPGR, seq(2L, 150L, 2L))
), by = curve_variables]
stopifnot(
  nrow(curve_check) == 324L,
  all(curve_check$rows == 75L),
  all(curve_check$complete)
)

frontiers <- results[, rbindlist(lapply(thresholds, function(threshold) {
  qualifying <- which(value >= threshold)
  below_range <- length(qualifying) == 0L
  above_range <- !below_range && value[.N] >= threshold
  frontier_index <- if (below_range) NA_integer_ else qualifying[[length(qualifying)]]
  data.table(
    GSI_threshold = threshold,
    frontier_population = if (below_range) NA_real_ else
      as.numeric(population[frontier_index]),
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
  list(variable = "climate", factor = "Climate", levels = c(
    "Reference river flows", "Reduced river flows"
  )),
  list(variable = "ENFLOPCT", factor = "Environmental-flow allocation", levels = c("0", "50")),
  list(variable = "PCRDGPCD", factor = "MWUE", levels = c("0", "29", "58")),
  list(variable = "WEBAGEFF", factor = "AWUE", levels = c("100", "90", "80")),
  list(variable = "WEBAGTR1", factor = "AWT", levels = c("0", "50", "100")),
  list(variable = "WEBAUGPCT", factor = "Supply augmentation", levels = c("0", "10", "20"))
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

# Determine whether an R type-7 quantile uses known observed order statistics.
# Censored values contribute their known position below or above the tested range,
# but no numerical boundary value is assigned to them.
quantile_status <- function(n_below, n_observed, n_above, probability) {
  n <- n_below + n_observed + n_above
  h <- 1 + (n - 1) * probability
  ranks <- unique(c(floor(h), ceiling(h)))
  categories <- vapply(ranks, function(rank) {
    if (rank <= n_below) return("below")
    if (rank <= n_below + n_observed) return("observed")
    "above"
  }, character(1))
  if (length(unique(categories)) == 1L && categories[[1]] == "observed") {
    "exact"
  } else if (length(unique(categories)) == 1L) {
    paste0(categories[[1]], "_tested_range")
  } else {
    "not_exactly_identifiable"
  }
}

long_frontiers <- rbindlist(lapply(seq_along(factor_specifications), function(index) {
  specification <- factor_specifications[[index]]
  variable <- specification$variable
  frontiers[, .(
    factor_order = index,
    factor = specification$factor,
    factor_level_raw = as.character(get(variable)),
    factor_level = reader_label(variable, get(variable)),
    level_order = match(as.character(get(variable)), specification$levels),
    GSI_threshold,
    boundary_class
  )]
}))

diagnostic <- long_frontiers[, .(
  total_background_combinations = .N,
  n_below_range = sum(boundary_class == "Below tested range"),
  n_observed = sum(boundary_class == "Observed within tested range"),
  n_above_range = sum(boundary_class == "Above tested range")
), by = .(factor_order, factor, level_order, factor_level, factor_level_raw, GSI_threshold)]

diagnostic[, `:=`(
  pct_below_range = 100 * n_below_range / total_background_combinations,
  pct_observed = 100 * n_observed / total_background_combinations,
  pct_above_range = 100 * n_above_range / total_background_combinations
)]

diagnostic[, `:=`(
  median_detail = mapply(
    quantile_status, n_below_range, n_observed, n_above_range,
    MoreArgs = list(probability = 0.50)
  ),
  q25_detail = mapply(
    quantile_status, n_below_range, n_observed, n_above_range,
    MoreArgs = list(probability = 0.25)
  ),
  q75_detail = mapply(
    quantile_status, n_below_range, n_observed, n_above_range,
    MoreArgs = list(probability = 0.75)
  )
)]

diagnostic[, identifiability_class := fcase(
  median_detail != "exact", "median itself outside tested population range",
  q25_detail == "exact" & q75_detail == "exact", "fully observed",
  default = "median observed but IQR partly outside tested population range"
)]

stopifnot(
  nrow(diagnostic) == 16L * length(thresholds),
  all(diagnostic$total_background_combinations %in% c(108L, 162L)),
  all(diagnostic$n_below_range + diagnostic$n_observed + diagnostic$n_above_range ==
        diagnostic$total_background_combinations)
)

setorder(diagnostic, factor_order, level_order, GSI_threshold)
setcolorder(diagnostic, c(
  "factor", "factor_level", "GSI_threshold", "identifiability_class",
  "total_background_combinations", "n_below_range", "pct_below_range",
  "n_observed", "pct_observed", "n_above_range", "pct_above_range",
  "median_detail", "q25_detail", "q75_detail", "factor_level_raw",
  "factor_order", "level_order"
))
fwrite(diagnostic, file.path(output_directory, "frontier_identifiability_all_thresholds.csv"))

threshold_summary <- diagnostic[, .(
  factor_level_groups = .N,
  fully_observed = sum(identifiability_class == "fully observed"),
  median_observed_IQR_partly_outside = sum(
    identifiability_class ==
      "median observed but IQR partly outside tested population range"
  ),
  median_outside_tested_range = sum(
    identifiability_class == "median itself outside tested population range"
  )
), by = GSI_threshold]
fwrite(threshold_summary, file.path(output_directory, "identifiability_by_threshold.csv"))

cat("\nALL-THRESHOLD IDENTIFIABILITY SUMMARY\n")
print(diagnostic[, .N, by = identifiability_class][order(identifiability_class)])
cat("\nTHRESHOLDS WHERE ALL 16 FACTOR LEVELS ARE FULLY OBSERVED\n")
print(threshold_summary[fully_observed == factor_level_groups])
cat("\nTRANSITION SUMMARY BY THRESHOLD\n")
print(threshold_summary)

heatmap_data <- copy(diagnostic)
heatmap_data[, row_label := paste(factor, factor_level, sep = ": ")]
heatmap_data[, row_label := factor(row_label, levels = rev(unique(row_label)))]
heatmap_data[, identifiability_class := factor(
  identifiability_class,
  levels = c(
    "fully observed",
    "median observed but IQR partly outside tested population range",
    "median itself outside tested population range"
  )
)]

heatmap <- ggplot(
  heatmap_data,
  aes(x = GSI_threshold, y = row_label, fill = identifiability_class)
) +
  geom_tile() +
  scale_x_continuous(breaks = seq(45, 100, 5), expand = c(0, 0)) +
  scale_fill_manual(values = c(
    "fully observed" = "#2A9D8F",
    "median observed but IQR partly outside tested population range" = "#E9C46A",
    "median itself outside tested population range" = "#E76F51"
  ), drop = FALSE) +
  labs(
    x = "Minimum GSI threshold (years)", y = NULL,
    fill = "Median and IQR identifiability"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    panel.grid = element_blank(),
    axis.text.y = element_text(size = 8),
    legend.position = "bottom",
    legend.text = element_text(size = 8)
  )

ggsave(
  "internal_validation/frontier_bounds/figures/frontier_identifiability_all_thresholds.png",
  heatmap, width = 12, height = 7.5, dpi = 300, bg = "white"
)
