suppressPackageStartupMessages({
  library(ggplot2)
  library(patchwork)
  library(grid)
  library(gridExtra)
  library(ragg)
})
source("scripts/common_data.R")

salt_verde_capacity_af <- 2500000
storage_thresholds <- seq(59.5, 69.5, by = 0.25)

results <- read_primary_results(fields = "SVRSTORE", years = 2060)
stopifnot(
  nrow(results) == 24300L,
  uniqueN(results$scenario) == 24300L,
  !anyNA(results$value)
)
results[, percent_capacity := 100 * value / salt_verde_capacity_af]

observed_range <- range(results$percent_capacity)
stopifnot(
  min(storage_thresholds) >= floor(observed_range[1]),
  max(storage_thresholds) <= ceiling(observed_range[2])
)

population_lookup <- read_population_lookup()
population_labels <- setNames(
  sprintf("%.1f", population_lookup$population / 1e6),
  population_lookup$WEBPOPGR
)

population_levels <- as.character(seq(2, 150, by = 2))
population_breaks <- c("2", "28", "56", "86", "116", "150")
threshold_levels <- sprintf("%.1f", storage_thresholds)
threshold_breaks <- sprintf("%.1f", seq(59.5, 69.5, by = 0.5))

make_heatmap <- function(filter_variable, filter_level, title,
                         show_y = FALSE, show_x = FALSE) {
  filtered <- results[get(filter_variable) == filter_level]
  scenario_counts <- filtered[, .N, by = WEBPOPGR]
  stopifnot(
    nrow(scenario_counts) == 75L,
    all(scenario_counts$N == 108L)
  )

  heatmap_data <- rbindlist(lapply(storage_thresholds, function(threshold) {
    filtered[, .(
      scenarios_meeting = sum(percent_capacity >= threshold),
      total_scenarios = .N
    ), by = WEBPOPGR][, storage_threshold := threshold]
  }))
  heatmap_data[, `:=`(
    percent_meeting = 100 * scenarios_meeting / total_scenarios,
    storage_threshold_label = sprintf("%.1f", storage_threshold)
  )]

  stopifnot(
    nrow(heatmap_data) == 75L * length(storage_thresholds),
    all(heatmap_data$total_scenarios == 108L),
    all(heatmap_data$percent_meeting >= 0),
    all(heatmap_data$percent_meeting <= 100)
  )

  ggplot(
    heatmap_data,
    aes(
      factor(storage_threshold_label, levels = threshold_levels),
      factor(WEBPOPGR),
      fill = percent_meeting
    )
  ) +
    geom_tile() +
    scale_fill_viridis_c(
      option = "cividis", limits = c(0, 100),
      breaks = c(0, 25, 50, 75, 100),
      labels = paste0(c(0, 25, 50, 75, 100), "%"),
      name = "Scenarios meeting the minimum Salt-Verde storage threshold (%)",
      guide = guide_colorbar(
        barwidth = unit(3, "in"), barheight = unit(0.22, "in"),
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
      x = if (show_x) "Minimum Salt-Verde storage in 2060 (% capacity)" else NULL,
      y = NULL,
      title = title
    ) +
    theme_minimal(base_size = 9) +
    theme(
      plot.title = element_text(size = 9, face = "bold"),
      axis.text.x = if (show_x) {
        element_text(size = 8, angle = 90, hjust = 1)
      } else {
        element_blank()
      },
      axis.text.y = if (show_y) element_text(size = 8) else element_blank(),
      axis.ticks.x = if (show_x) element_line() else element_blank(),
      axis.ticks.y = if (show_y) element_line() else element_blank(),
      axis.title.x = element_text(margin = margin(t = 14)),
      panel.grid = element_blank(),
      axis.line = element_line(color = "black", linewidth = 0.3),
      legend.position = "bottom"
    )
}

plots <- list(
  make_heatmap("PCRDGPCD", 0, "Municipal water-use reduction: 3.2%", TRUE),
  make_heatmap("PCRDGPCD", 29, "Municipal water-use reduction: 25%"),
  make_heatmap("PCRDGPCD", 58, "Municipal water-use reduction: 45%"),
  make_heatmap("WEBAGEFF", 100, "Agricultural efficiency: current (2015)", TRUE),
  make_heatmap("WEBAGEFF", 90, "Agricultural efficiency: 10% below 2015"),
  make_heatmap("WEBAGEFF", 80, "Agricultural efficiency: 20% below 2015"),
  make_heatmap("WEBAGTR1", 0, "Agricultural water transfer to cities: 0%", TRUE),
  make_heatmap("WEBAGTR1", 50, "Agricultural water transfer to cities: 50%"),
  make_heatmap("WEBAGTR1", 100, "Agricultural water transfer to cities: 100%"),
  make_heatmap("WEBAUGPCT", 0, "Supply augmentation: 0%", TRUE, TRUE),
  make_heatmap("WEBAUGPCT", 10, "Supply augmentation: 10%", FALSE, TRUE),
  make_heatmap("WEBAUGPCT", 20, "Supply augmentation: 20%", FALSE, TRUE)
)

combined <- wrap_plots(plots, ncol = 3) +
  plot_layout(guides = "collect", axes = "collect", axis_titles = "collect") &
  theme(legend.position = "bottom")

figure <- arrangeGrob(
  patchworkGrob(combined),
  left = textGrob(
    "Projected population in 2060 (millions)", rot = 90,
    gp = gpar(fontsize = 9)
  ),
  padding = unit(2, "mm")
)

agg_png(
  "supplementary/figures/supplementary_figure_2.png",
  width = 3600, height = 3600, units = "px", res = 360,
  background = "white"
)
grid.draw(figure)
dev.off()

if (file.exists("Rplots.pdf")) unlink("Rplots.pdf")
