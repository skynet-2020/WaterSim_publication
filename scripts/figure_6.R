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

stopifnot(
  nrow(results) == 24300L,
  uniqueN(results$scenario) == 24300L
)

population_levels <- as.character(seq(2, 150, 2))
population_breaks <- c("2", "28", "56", "86", "116", "150")
threshold_levels <- as.character(60:100)
threshold_breaks <- as.character(c(60, 70, 80, 90, 100))

make_heatmap <- function(filtered, title, expected_scenarios,
                         show_y = FALSE, show_x = FALSE,
                         x_breaks = threshold_breaks,
                         x_labels = x_breaks) {
  heatmap_data <- rbindlist(lapply(60:100, function(threshold) {
    filtered[, .(
      scenarios_meeting = sum(value >= threshold),
      total_scenarios = .N
    ), by = WEBPOPGR][, groundwater_threshold := threshold]
  }))
  heatmap_data[, percent_meeting := 100 * scenarios_meeting / total_scenarios]

  stopifnot(
    nrow(heatmap_data) == 75L * 41L,
    all(heatmap_data$total_scenarios == expected_scenarios),
    all(heatmap_data$percent_meeting >= 0),
    all(heatmap_data$percent_meeting <= 100)
  )

  ggplot(
    heatmap_data,
    aes(factor(groundwater_threshold), factor(WEBPOPGR), fill = percent_meeting)
  ) +
    geom_tile() +
    scale_fill_viridis_c(
      option = "magma", limits = c(0, 100),
      breaks = c(0, 25, 50, 75, 100),
      labels = paste0(c(0, 25, 50, 75, 100), "%"),
      name = "Scenario combinations meeting GSI threshold (%)",
      guide = guide_colorbar(
        barwidth = unit(12, "in"), barheight = unit(0.36, "in"),
        title.position = "top"
      )
    ) +
    scale_x_discrete(
      limits = threshold_levels, breaks = x_breaks, labels = x_labels,
      drop = FALSE
    ) +
    scale_y_discrete(
      limits = population_levels, breaks = population_breaks,
      labels = unname(population_labels[population_breaks]), drop = FALSE
    ) +
    labs(x = NULL, y = NULL, title = title) +
    theme_classic(base_size = 22) +
    theme(
      plot.title = element_text(
        size = 22, face = "plain", hjust = 0.5,
        margin = margin(b = 3)
      ),
      axis.text.x = if (show_x) {
        element_text(size = 22, margin = margin(t = 8))
      } else {
        element_blank()
      },
      axis.text.y = if (show_y) {
        element_text(size = 22, margin = margin(r = 8))
      } else {
        element_blank()
      },
      axis.ticks.x = if (show_x) element_line() else element_blank(),
      axis.ticks.y = if (show_y) element_line() else element_blank(),
      panel.grid = element_blank(),
      panel.border = element_rect(fill = NA, color = "black", linewidth = 0.3),
      axis.line = element_blank(),
      legend.position = "bottom",
      legend.title = element_text(size = 22),
      legend.text = element_text(size = 22),
      legend.box.margin = margin(t = 18),
      plot.margin = margin(2, 3, 2, 3)
    )
}

factor_label <- function(label) {
  wrap_elements(full = textGrob(
    label,
    x = unit(0.015, "npc"), y = unit(0.5, "npc"),
    just = c("center", "bottom"), rot = -90,
    gp = gpar(fontsize = 22, fontface = "plain", lineheight = 0.95)
  ))
}

plots <- list(
  # Climate-related river-flow conditions: centered two-panel row.
  make_heatmap(
    results[COCLMADJ == 100 & SVCLMADJ == 105],
    "Reference flows", 162L, show_y = TRUE
  ),
  make_heatmap(
    results[COCLMADJ == 70 & SVCLMADJ == 85],
    "Reduced flows", 162L
  ),
  factor_label("Climate"),

  # Environmental-flow allocation: centered two-panel row.
  make_heatmap(
    results[ENFLOPCT == 0], "0 AF", 162L, show_y = TRUE
  ),
  make_heatmap(
    results[ENFLOPCT == 50], "80,000 AF", 162L
  ),
  factor_label("Environmental\nflow"),

  # Three-panel policy rows.
  make_heatmap(
    results[PCRDGPCD == 0], "3.2% reduction", 108L, show_y = TRUE
  ),
  make_heatmap(results[PCRDGPCD == 29], "25% reduction", 108L),
  make_heatmap(results[PCRDGPCD == 58], "45% reduction", 108L),
  factor_label("MWUE"),

  make_heatmap(
    results[WEBAGEFF == 100], "Current (2015)", 108L, show_y = TRUE
  ),
  make_heatmap(results[WEBAGEFF == 90], "10% below 2015", 108L),
  make_heatmap(results[WEBAGEFF == 80], "20% below 2015", 108L),
  factor_label("AWUE"),

  make_heatmap(
    results[WEBAGTR1 == 0], "0%", 108L, show_y = TRUE
  ),
  make_heatmap(results[WEBAGTR1 == 50], "50%", 108L),
  make_heatmap(results[WEBAGTR1 == 100], "100%", 108L),
  factor_label("AWT"),

  make_heatmap(
    results[WEBAUGPCT == 0], "0%", 108L,
    show_y = TRUE, show_x = TRUE,
    x_breaks = as.character(c(60, 70, 80, 90))
  ),
  make_heatmap(
    results[WEBAUGPCT == 10], "10%", 108L, show_x = TRUE,
    x_breaks = as.character(c(60, 70, 80, 90, 100)),
    x_labels = c("60", "70", "80", "90", "100   ")
  ),
  make_heatmap(
    results[WEBAUGPCT == 20], "20%", 108L, show_x = TRUE,
    x_breaks = as.character(c(70, 80, 90, 100))
  ),
  factor_label("Supply\naugmentation"),

  wrap_elements(full = textGrob(
    "Minimum GSI threshold (years)",
    gp = gpar(fontsize = 22)
  )),
  guide_area()
)

# Columns 1:6 define the heatmap region. Two-panel rows occupy columns 2:5,
# so their labels begin in column 6. Three-panel rows occupy columns 1:6,
# so their labels begin in column 7. This deliberately avoids a shared label
# column and keeps each factor name close to its own panel group.
design <- c(
  area(1, 2, 1, 3), area(1, 4, 1, 5), area(1, 6, 1, 8),
  area(2, 2, 2, 3), area(2, 4, 2, 5), area(2, 6, 2, 8),
  area(3, 1, 3, 2), area(3, 3, 3, 4), area(3, 5, 3, 6), area(3, 7, 3, 8),
  area(4, 1, 4, 2), area(4, 3, 4, 4), area(4, 5, 4, 6), area(4, 7, 4, 8),
  area(5, 1, 5, 2), area(5, 3, 5, 4), area(5, 5, 5, 6), area(5, 7, 5, 8),
  area(6, 1, 6, 2), area(6, 3, 6, 4), area(6, 5, 6, 6), area(6, 7, 6, 8),
  area(8, 1, 8, 6),
  area(10, 1, 10, 6)
)

combined <- wrap_plots(plots, design = design) +
  plot_layout(
    guides = "collect",
    widths = rep(1, 8),
    heights = c(rep(1, 6), 0.04, 0.20, 0.35, 0.50)
  ) &
  theme(legend.position = "bottom")

figure <- arrangeGrob(
  patchworkGrob(combined),
  left = textGrob(
    "Regional population in 2060 (millions)", rot = 90,
    gp = gpar(fontsize = 22)
  ),
  padding = unit(12, "mm")
)

agg_png(
  "figures/figure_6.png",
  width = 5700, height = 5400, units = "px", res = 300,
  background = "white"
)
grid.draw(figure)
dev.off()

if (file.exists("Rplots.pdf")) unlink("Rplots.pdf")
