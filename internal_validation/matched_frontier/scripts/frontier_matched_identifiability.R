suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
})
source("scripts/common_data.R")

thresholds <- 45:100
output_directory <- "internal_validation/matched_frontier/tables"
dir.create(output_directory, recursive = TRUE, showWarnings = FALSE)

curve_variables <- c(
  "climate", "ENFLOPCT", "PCRDGPCD", "WEBAGEFF", "WEBAGTR1",
  "WEBAUGPCT"
)

results <- read_primary_results(fields = "SINYRGWR", years = 2060)
stopifnot(
  nrow(results) == 24300L,
  uniqueN(results$scenario) == 24300L,
  identical(range(results$value), c(45L, 100L))
)

results[, climate := fcase(
  COCLMADJ == 100L & SVCLMADJ == 105L, "Reference river flows",
  COCLMADJ == 70L & SVCLMADJ == 85L, "Reduced river flows",
  default = NA_character_
)]
stopifnot(!anyNA(results$climate))

# Join exact regional population values to preserve the established frontier
# definition. No interpolated or censored boundary values are introduced.
population_lookup <- read_population_lookup()
population_lookup <- population_lookup[WEBPOPGR %in% unique(results$WEBPOPGR)]
stopifnot(
  nrow(population_lookup) == 75L,
  identical(sort(population_lookup$WEBPOPGR), seq(2L, 150L, 2L))
)
results <- population_lookup[results, on = "WEBPOPGR"]
stopifnot(nrow(results) == 24300L, !anyNA(results$population))
setorderv(results, c(curve_variables, "WEBPOPGR"))

# Verify the complete 324 x 75 factorial curve structure before classification.
curve_completeness <- results[, .(
  rows = .N,
  complete_population_grid = identical(WEBPOPGR, seq(2L, 150L, 2L))
), by = curve_variables]
stopifnot(
  nrow(curve_completeness) == 324L,
  all(curve_completeness$rows == 75L),
  all(curve_completeness$complete_population_grid)
)

# One classified frontier per climate-policy curve and threshold. Population is
# recorded only for an observed crossing; censored cases remain numeric NA.
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
    frontier_class = if (below_range) {
      "below_range"
    } else if (above_range) {
      "above_range"
    } else {
      "observed"
    }
  )
})), by = curve_variables]

stopifnot(
  nrow(frontiers) == 324L * length(thresholds),
  all(is.na(frontiers[frontier_class != "observed"]$frontier_population)),
  !anyNA(frontiers[frontier_class == "observed"]$frontier_population)
)

factor_specifications <- list(
  list(variable = "climate", factor = "Climate", levels = 2L,
       matched_backgrounds = 162L),
  list(variable = "ENFLOPCT", factor = "Environmental flow", levels = 2L,
       matched_backgrounds = 162L),
  list(variable = "PCRDGPCD", factor = "MWUE", levels = 3L,
       matched_backgrounds = 108L),
  list(variable = "WEBAGEFF", factor = "AWUE", levels = 3L,
       matched_backgrounds = 108L),
  list(variable = "WEBAGTR1", factor = "AWT", levels = 3L,
       matched_backgrounds = 108L),
  list(variable = "WEBAUGPCT", factor = "Supply augmentation", levels = 3L,
       matched_backgrounds = 108L)
)

summarize_factor <- function(specification) {
  focal_variable <- specification$variable
  background_variables <- setdiff(curve_variables, focal_variable)

  # Each row below is one exactly matched background at one GSI threshold.
  matched_sets <- frontiers[, .(
    number_of_factor_levels = .N,
    unique_factor_levels = uniqueN(get(focal_variable)),
    all_levels_observed = all(frontier_class == "observed"),
    has_above_range = any(frontier_class == "above_range"),
    has_below_range = any(frontier_class == "below_range")
  ), by = c(background_variables, "gsi_threshold")]

  stopifnot(
    nrow(matched_sets) == specification$matched_backgrounds * length(thresholds),
    all(matched_sets$number_of_factor_levels == specification$levels),
    all(matched_sets$unique_factor_levels == specification$levels),
    all(matched_sets[, .N, by = gsi_threshold]$N == specification$matched_backgrounds)
  )

  # Incomplete categories are mutually exclusive so they sum with complete sets
  # to the total number of matched backgrounds.
  matched_sets[, incomplete_class := fcase(
    all_levels_observed, "complete",
    has_above_range & !has_below_range, "above_only",
    has_below_range & !has_above_range, "below_only",
    default = "both_or_other"
  )]

  summary <- matched_sets[, .(
    total_matched_backgrounds = .N,
    n_complete_matched_sets = sum(incomplete_class == "complete"),
    n_sets_with_above_range = sum(incomplete_class == "above_only"),
    n_sets_with_below_range = sum(incomplete_class == "below_only"),
    n_sets_with_both_or_other_incomplete = sum(incomplete_class == "both_or_other")
  ), by = gsi_threshold]
  summary[, `:=`(
    factor = specification$factor,
    pct_complete_matched_sets = 100 * n_complete_matched_sets /
      total_matched_backgrounds
  )]

  stopifnot(
    all(summary$total_matched_backgrounds == specification$matched_backgrounds),
    all(summary$n_complete_matched_sets + summary$n_sets_with_above_range +
          summary$n_sets_with_below_range +
          summary$n_sets_with_both_or_other_incomplete ==
          summary$total_matched_backgrounds)
  )
  summary
}

matched_summary <- rbindlist(lapply(factor_specifications, summarize_factor))
factor_levels <- vapply(factor_specifications, `[[`, character(1), "factor")
matched_summary[, factor_order := match(factor, factor_levels)]
setorder(matched_summary, factor_order, gsi_threshold)
setcolorder(matched_summary, c(
  "factor", "gsi_threshold", "total_matched_backgrounds",
  "n_complete_matched_sets", "pct_complete_matched_sets",
  "n_sets_with_above_range", "n_sets_with_below_range",
  "n_sets_with_both_or_other_incomplete", "factor_order"
))

fwrite(
  matched_summary,
  file.path(output_directory, "matched_identifiability_by_factor_threshold.csv")
)

threshold_summary <- dcast(
  matched_summary,
  gsi_threshold ~ factor,
  value.var = "pct_complete_matched_sets"
)
setnames(
  threshold_summary,
  c("Climate", "Environmental flow", "MWUE", "AWUE", "AWT", "Supply augmentation"),
  c(
    "climate_pct_complete", "ef_pct_complete", "mwue_pct_complete",
    "awue_pct_complete", "awt_pct_complete", "sa_pct_complete"
  )
)
percentage_columns <- setdiff(names(threshold_summary), "gsi_threshold")
threshold_summary[, all_factors_100pct_complete :=
  rowSums(.SD == 100) == length(percentage_columns), .SDcols = percentage_columns]
setorder(threshold_summary, gsi_threshold)

# Locate every contiguous TRUE run, then select the longest. Ties are resolved
# toward the lower threshold interval to keep the result deterministic.
threshold_summary[, run_id := rleid(all_factors_100pct_complete)]
complete_intervals <- threshold_summary[all_factors_100pct_complete == TRUE, .(
  start_threshold = min(gsi_threshold),
  end_threshold = max(gsi_threshold),
  number_of_thresholds = .N
), by = run_id][order(-number_of_thresholds, start_threshold)]

if (nrow(complete_intervals)) {
  longest_interval <- complete_intervals[1L]
} else {
  longest_interval <- data.table(
    run_id = NA_integer_, start_threshold = NA_integer_,
    end_threshold = NA_integer_, number_of_thresholds = 0L
  )
}
threshold_summary[, run_id := NULL]

fwrite(
  threshold_summary,
  file.path(output_directory, "matched_identifiability_by_threshold.csv")
)
fwrite(
  longest_interval[, .(start_threshold, end_threshold, number_of_thresholds)],
  file.path(output_directory, "longest_fully_matchable_interval.csv")
)

heatmap_data <- copy(matched_summary)
heatmap_data[, factor := base::factor(factor, levels = rev(factor_levels))]

heatmap <- ggplot(
  heatmap_data,
  aes(x = gsi_threshold, y = factor, fill = pct_complete_matched_sets)
) +
  geom_tile() +
  geom_tile(
    data = heatmap_data[pct_complete_matched_sets == 100],
    fill = NA, color = "black", linewidth = 0.18
  ) +
  scale_x_continuous(breaks = seq(45, 100, 5), expand = c(0, 0)) +
  scale_fill_viridis_c(
    option = "magma", limits = c(0, 100),
    breaks = c(0, 25, 50, 75, 100),
    labels = paste0(c(0, 25, 50, 75, 100), "%"),
    name = "Complete matched sets",
    guide = guide_colorbar(
      title.position = "top",
      barwidth = grid::unit(5, "in"),
      barheight = grid::unit(0.22, "in")
    )
  ) +
  labs(x = "Minimum GSI threshold (years)", y = NULL) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid = element_blank(),
    axis.text.y = element_text(size = 10),
    legend.position = "bottom"
  )

ggsave(
  "internal_validation/matched_frontier/figures/frontier_matched_identifiability.png",
  heatmap, width = 11, height = 4.8, dpi = 300, bg = "white"
)

cat("\nMATCHED FRONTIER IDENTIFIABILITY QA\n")
print(unique(matched_summary[, .(factor, total_matched_backgrounds)]))
cat("\nLongest interval with 100% complete matching for all six factors:\n")
print(longest_interval[, .(start_threshold, end_threshold, number_of_thresholds)])
cat("\nThresholds with 100% completeness for all factors:\n")
print(threshold_summary[all_factors_100pct_complete == TRUE, .(gsi_threshold)])
