suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
  library(ragg)
})
source("scripts/common_data.R")

results <- read_primary_results(fields = "SINYRGWR", years = 2060)
population_lookup <- read_population_lookup()
population_labels <- setNames(
  sprintf("%.1f", population_lookup$population / 1e6),
  population_lookup$WEBPOPGR
)

stopifnot(
  nrow(results) == 24300L,
  uniqueN(results$scenario) == 24300L
)

population_levels <- as.character(seq(2, 150, 2))
population_breaks <- c("2", "28", "56", "86", "116", "150")
threshold_levels <- as.character(60:100)
threshold_breaks <- as.character(c(60, 70, 80, 90, 100))

# At each tested population, all 324 climate-policy combinations contribute.
heatmap_data <- rbindlist(lapply(60:100, function(threshold) {
  results[, .(
    scenarios_meeting = sum(value >= threshold),
    total_scenarios = .N
  ), by = WEBPOPGR][, gsi_threshold := threshold]
}))
heatmap_data[, percent_meeting := 100 * scenarios_meeting / total_scenarios]

stopifnot(
  nrow(heatmap_data) == 75L * 41L,
  all(heatmap_data$total_scenarios == 324L),
  all(heatmap_data$percent_meeting >= 0),
  all(heatmap_data$percent_meeting <= 100)
)

figure <- ggplot(
  heatmap_data,
  aes(factor(gsi_threshold), factor(WEBPOPGR), fill = percent_meeting)
) +
  geom_tile() +
  annotate(
    "text", x = 2, y = 2, label = "All scenario combinations",
    hjust = 0, vjust = 0, size = 20 / 2.845276,
    color = "black"
  ) +
  scale_fill_viridis_c(
    option = "magma", limits = c(0, 100),
    breaks = c(0, 25, 50, 75, 100),
    labels = paste0(c(0, 25, 50, 75, 100), "%"),
    name = "Scenario combinations meeting GSI threshold (%)",
    guide = guide_colorbar(
      barwidth = unit(6.5, "in"), barheight = unit(0.36, "in"),
      title.position = "top"
    )
  ) +
  scale_x_discrete(
    limits = threshold_levels, breaks = threshold_breaks, drop = FALSE
  ) +
  scale_y_discrete(
    limits = population_levels, breaks = population_breaks,
    labels = unname(population_labels[population_breaks]), drop = FALSE
  ) +
  labs(
    x = "Minimum GSI threshold (years)",
    y = "Regional population in 2060 (millions)",
    title = "Overall scenario feasibility"
  ) +
  theme_classic(base_size = 18) +
  theme(
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 20),
    axis.title.x = element_text(margin = margin(t = 18)),
    axis.title.y = element_text(margin = margin(r = 18)),
    axis.text.x = element_text(size = 18, margin = margin(t = 8)),
    axis.text.y = element_text(size = 18, margin = margin(r = 8)),
    panel.grid = element_blank(),
    panel.border = element_rect(fill = NA, color = "black", linewidth = 0.3),
    axis.line = element_blank(),
    legend.position = "bottom",
    legend.title = element_text(size = 18),
    legend.text = element_text(size = 18),
    legend.box.margin = margin(t = 28),
    plot.margin = margin(12, 18, 12, 12)
  )

agg_png(
  "additional_analysis/overall_feasibility/figures/figure_overall_feasibility.png",
  width = 3000, height = 3000, units = "px", res = 300,
  background = "white"
)
print(figure)
dev.off()

cat("Overall heatmap cells:", nrow(heatmap_data), "\n")
cat("Scenario combinations per population-threshold cell:",
    unique(heatmap_data$total_scenarios), "\n")
