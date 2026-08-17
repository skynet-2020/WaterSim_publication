suppressPackageStartupMessages({
  library(ggplot2)
  library(patchwork)
  library(grid)
  library(gridExtra)
  library(ragg)
})
source("scripts/common_data.R")

results <- read_primary_results(fields = "SINYRGWR", years = 2060)
population_lookup <- read_population_lookup()
population_labels <- setNames(
  sprintf("%.1f", population_lookup$population / 1e6),
  population_lookup$WEBPOPGR
)

population_levels <- as.character(seq(2, 150, 2))
population_breaks <- c("2", "28", "56", "86", "116", "150")
threshold_levels <- as.character(60:100)
threshold_breaks <- as.character(seq(60, 100, 2))

make_heatmap <- function(filter_variable, filter_level, title,
                         show_y = FALSE, show_x = FALSE) {
  filtered <- results[get(filter_variable) == filter_level]
  heatmap_data <- rbindlist(lapply(60:100, function(threshold) {
    filtered[, .(
      scenarios_meeting = sum(value >= threshold),
      total_scenarios = .N
    ), by = WEBPOPGR][, groundwater_threshold := threshold]
  }))
  heatmap_data[, percent_meeting := 100 * scenarios_meeting / total_scenarios]
  stopifnot(all(heatmap_data$total_scenarios == 108L))

  ggplot(
    heatmap_data,
    aes(factor(groundwater_threshold), factor(WEBPOPGR), fill = percent_meeting)
  ) +
    geom_tile() +
    scale_fill_viridis_c(
      option = "magma", limits = c(0, 100),
      breaks = c(0, 25, 50, 75, 100),
      labels = paste0(c(0, 25, 50, 75, 100), "%"),
      name = "Scenarios meeting the minimum pumping duration (%)",
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
      x = if (show_x) "Minimum groundwater supply remaining in 2060 (years)" else NULL,
      y = NULL,
      title = title
    ) +
    theme_minimal(base_size = 9) +
    theme(
      plot.title = element_text(size = 9, face = "bold"),
      axis.text.x = if (show_x) element_text(size = 8, angle = 90, hjust = 1) else element_blank(),
      axis.text.y = if (show_y) element_text(size = 8) else element_blank(),
      axis.ticks.x = if (show_x) element_line() else element_blank(),
      axis.ticks.y = if (show_y) element_line() else element_blank(),
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

agg_png("figures/figure_7.png", width = 3600, height = 3600, units = "px",
        res = 360, background = "white")
grid.draw(figure)
dev.off()

# Some graphics-device configurations create an unused default PDF while
# constructing the combined grob. It is not a publication output.
if (file.exists("Rplots.pdf")) unlink("Rplots.pdf")
