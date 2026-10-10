suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
  library(ggtext)
  library(patchwork)
})
source("scripts/common_data.R")

results <- read_primary_results(fields = "SINYRGWR", years = 2060)

add_climate_position <- function(data, variable, positions) {
  data[, climate := fifelse(
    COCLMADJ == 70 & SVCLMADJ == 85,
    "Colorado River: 30% reduction / Salt-Verde: 15% reduction",
    "Colorado River: no reduction / Salt-Verde: 5% increase"
  )]
  data[, base_position := positions[as.character(get(variable))]]
  data[, x_position := base_position + fifelse(COCLMADJ == 70, -0.1, 0.1)]
  data
}

prepare_panel <- function(threshold) {
  base <- results[value == threshold]

  environmental_flow <- add_climate_position(
    copy(base), "ENFLOPCT", c("0" = 1, "50" = 2)
  )
  municipal <- add_climate_position(
    base[ENFLOPCT == 0], "PCRDGPCD", c("0" = 3.5, "29" = 4, "58" = 4.5)
  )
  agricultural_efficiency <- add_climate_position(
    base[ENFLOPCT == 0 & PCRDGPCD == 58],
    "WEBAGEFF", c("80" = 6, "90" = 6.5, "100" = 7)
  )
  transfer <- add_climate_position(
    base[ENFLOPCT == 0 & PCRDGPCD == 58 & WEBAGEFF == 100],
    "WEBAGTR1", c("0" = 8.5, "50" = 9, "100" = 9.5)
  )
  augmentation <- add_climate_position(
    base[
      ENFLOPCT == 0 & PCRDGPCD == 58 & WEBAGEFF == 100 & WEBAGTR1 == 100
    ],
    "WEBAUGPCT", c("0" = 11, "10" = 11.5, "20" = 12)
  )

  rbindlist(list(
    environmental_flow, municipal, agricultural_efficiency, transfer,
    augmentation
  ), use.names = TRUE)
}

x_breaks <- c(1, 2, 3.5, 4, 4.5, 6, 6.5, 7, 8.5, 9, 9.5, 11, 11.5, 12)
x_labels <- c(
  "0 AF", "80,000 AF", "-3.2%", "-25%", "-45%", "-20%", "-10%", "0%",
  "0%", "50%", "100%", "0%", "10%", "20%"
)

make_panel <- function(threshold, show_x = FALSE) {
  panel_data <- prepare_panel(threshold)

  p <- ggplot(
    panel_data,
    aes(x = x_position, y = WEBPOPGR, group = x_position, fill = climate)
  ) +
    geom_violin(scale = "width", alpha = 0.7, linewidth = 0.5) +
    geom_boxplot(width = 0.1, outlier.shape = NA, alpha = 0.55) +
    scale_fill_manual(values = c(
      "Colorado River: 30% reduction / Salt-Verde: 15% reduction" = "#F8766D",
      "Colorado River: no reduction / Salt-Verde: 5% increase" = "#00BFC4"
    )) +
    scale_x_continuous(
      limits = c(0, 13), breaks = x_breaks,
      labels = if (show_x) x_labels else NULL
    ) +
    scale_y_continuous(
      limits = if (show_x) c(-58, 150) else c(0, 150),
      breaks = seq(0, 150, 10),
      labels = sprintf("%.2f", population_from_webpopgr(seq(0, 150, 10)) / 1e6)
    ) +
    labs(
      x = if (show_x) "Policy choices" else NULL,
      y = "Population (million)",
      fill = NULL
    ) +
    theme_classic(base_size = 10) +
    theme(
      legend.position = if (threshold == 70) "inside" else "none",
      legend.position.inside = c(0.67, 0.36),
      axis.text.x = if (show_x) element_text(size = 8) else element_blank(),
      axis.ticks.x = if (show_x) element_line() else element_blank(),
      axis.title.x = element_text(margin = margin(t = 12)),
      axis.title.y = element_text(margin = margin(r = 8))
    )

  if (show_x) {
    p <- p +
      annotate("text", x = 1.5, y = -19, label = "Environmental Flows (EF)",
               fontface = "bold", size = 3.2) +
      annotate("text", x = 4, y = -25,
               label = "Municipal Water-Use Efficiency (MWUE)\nCondition: EF = 0",
               fontface = "bold", size = 3.0) +
      annotate("text", x = 6.5, y = -25,
               label = "Agricultural Water-Use Efficiency (AWUE)\nConditions: EF = 0; MWUE = -45%",
               fontface = "bold", size = 3.0) +
      annotate("text", x = 9, y = -31,
               label = "Agricultural Water Transfer (AWT)\nConditions: EF = 0; MWUE = -45%; AWUE = 0%",
               fontface = "bold", size = 3.0) +
      annotate("text", x = 11.5, y = -31,
               label = "Supply Augmentation\nConditions: EF = 0; MWUE = -45%; AWUE = 0%; AWT = 100%",
               fontface = "bold", size = 3.0)
  }

  p
}

figure <- make_panel(70) / make_panel(80) / make_panel(90) / make_panel(100, TRUE) +
  plot_layout(heights = c(1, 1, 1, 1.35))

ggsave("figures/figure_5.png", figure, width = 10, height = 10, dpi = 300,
       scale = 1.5, bg = "white")
