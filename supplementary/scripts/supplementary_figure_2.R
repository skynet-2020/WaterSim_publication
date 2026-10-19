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
threshold_breaks <- sprintf("%.1f", c(59.5, 62.0, 64.5, 67.0, 69.5))

make_heatmap <- function(filtered, title, expected_scenarios,
                         show_y = FALSE, show_x = FALSE,
                         x_breaks = threshold_breaks,
                         x_labels = x_breaks) {
  scenario_counts <- filtered[, .N, by = WEBPOPGR]
  stopifnot(
    nrow(scenario_counts) == 75L,
    all(scenario_counts$N == expected_scenarios)
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
    all(heatmap_data$total_scenarios == expected_scenarios),
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
  # Environmental-flow allocation: centered two-panel row.
  make_heatmap(results[ENFLOPCT == 0], "0 AF", 162L, show_y = TRUE),
  make_heatmap(results[ENFLOPCT == 50], "80,000 AF", 162L),
  factor_label("Environmental\nflow"),

  # Three-panel policy rows.
  make_heatmap(results[PCRDGPCD == 0], "3.2% reduction", 108L, show_y = TRUE),
  make_heatmap(results[PCRDGPCD == 29], "25% reduction", 108L),
  make_heatmap(results[PCRDGPCD == 58], "45% reduction", 108L),
  factor_label("MWUE"),

  make_heatmap(results[WEBAGEFF == 100], "0% change (2015)", 108L, show_y = TRUE),
  make_heatmap(results[WEBAGEFF == 90], "10% decrease in efficiency", 108L),
  make_heatmap(results[WEBAGEFF == 80], "20% decrease in efficiency", 108L),
  factor_label("AWUE"),

  make_heatmap(results[WEBAGTR1 == 0], "0%", 108L, show_y = TRUE),
  make_heatmap(results[WEBAGTR1 == 50], "50%", 108L),
  make_heatmap(results[WEBAGTR1 == 100], "100%", 108L),
  factor_label("AWT"),

  make_heatmap(
    results[WEBAUGPCT == 0], "0%", 108L,
    show_y = TRUE, show_x = TRUE,
    x_breaks = sprintf("%.1f", c(59.5, 62.0, 64.5, 67.0))
  ),
  make_heatmap(
    results[WEBAUGPCT == 10], "10%", 108L, show_x = TRUE,
    x_breaks = sprintf("%.1f", c(59.5, 62.0, 64.5, 67.0))
  ),
  make_heatmap(
    results[WEBAUGPCT == 20], "20%", 108L, show_x = TRUE,
    x_breaks = sprintf("%.1f", c(62.0, 64.5, 67.0, 69.5))
  ),
  factor_label("Supply\naugmentation"),

  wrap_elements(full = textGrob(
    "Minimum Salt-Verde storage in 2060 (% capacity)",
    gp = gpar(fontsize = 22)
  )),
  guide_area()
)

# Columns 1:6 define the heatmap region. The centered two-panel EF row occupies
# columns 2:5, while each three-panel policy row occupies columns 1:6. As in
# Figure 6, each vertical factor label begins immediately after its own row.
design <- c(
  area(1, 2, 1, 3), area(1, 4, 1, 5), area(1, 6, 1, 8),
  area(2, 1, 2, 2), area(2, 3, 2, 4), area(2, 5, 2, 6), area(2, 7, 2, 8),
  area(3, 1, 3, 2), area(3, 3, 3, 4), area(3, 5, 3, 6), area(3, 7, 3, 8),
  area(4, 1, 4, 2), area(4, 3, 4, 4), area(4, 5, 4, 6), area(4, 7, 4, 8),
  area(5, 1, 5, 2), area(5, 3, 5, 4), area(5, 5, 5, 6), area(5, 7, 5, 8),
  area(7, 1, 7, 6),
  area(9, 1, 9, 6)
)

combined <- wrap_plots(plots, design = design) +
  plot_layout(
    guides = "collect",
    widths = rep(1, 8),
    heights = c(rep(1, 5), 0.04, 0.20, 0.35, 0.50)
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
  "supplementary/figures/supplementary_figure_2.png",
  width = 5700, height = 4800, units = "px", res = 300,
  background = "white"
)
grid.draw(figure)
dev.off()

if (file.exists("Rplots.pdf")) unlink("Rplots.pdf")
