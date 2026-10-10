suppressPackageStartupMessages({
  library(ggplot2)
  library(patchwork)
})
source("scripts/common_data.R")

capacities <- c(MEDSTORE = 30000000, POWSTORE = 30000000, SVRSTORE = 2500000)
results <- read_primary_results(fields = names(capacities))
results[, percent_capacity := 100 * value / capacities[field]]

make_storage_panel <- function(field_name, y_label, show_x = FALSE,
                               show_legend = FALSE) {
  field_data <- results[field == field_name]
  spin_up <- unique(field_data[year <= 2016, .(year, percent_capacity)], by = "year")
  projections <- field_data[year >= 2017, .(
    percent_capacity = mean(percent_capacity)
  ), by = .(year, COCLMADJ, SVCLMADJ)]
  projections[, climate := fifelse(
    COCLMADJ == 70 & SVCLMADJ == 85,
    "Severe climate impact", "No climate impact"
  )]

  bridge <- rbindlist(lapply(unique(projections$climate), function(label) {
    data.table(
      year = 2016,
      COCLMADJ = NA_integer_,
      SVCLMADJ = NA_integer_,
      percent_capacity = spin_up[year == 2016, percent_capacity][1],
      climate = label
    )
  }))
  projections <- rbind(projections, bridge, fill = TRUE)
  setorder(projections, climate, year)

  ggplot() +
    geom_area(
      data = spin_up, aes(year, percent_capacity),
      fill = "grey", alpha = 0.4
    ) +
    geom_line(
      data = spin_up, aes(year, percent_capacity, color = "Spin-Up (Observed)"),
      linewidth = 1
    ) +
    geom_area(
      data = projections,
      aes(year, percent_capacity, group = climate, fill = climate),
      alpha = 0.3, position = "identity"
    ) +
    geom_line(
      data = projections,
      aes(year, percent_capacity, group = climate, color = climate),
      linewidth = 1
    ) +
    scale_color_manual(values = c(
      "Spin-Up (Observed)" = "black",
      "Severe climate impact" = "#F8766D",
      "No climate impact" = "#00BFC4"
    )) +
    scale_fill_manual(values = c(
      "Severe climate impact" = "#F8766D",
      "No climate impact" = "#00BFC4"
    ), guide = "none") +
    scale_x_continuous(breaks = seq(2000, 2060, 10)) +
    scale_y_continuous(limits = c(0, 105), breaks = seq(0, 100, 10)) +
    labs(
      x = if (show_x) "Year" else NULL,
      y = y_label,
      color = if (show_legend) "Scenario" else NULL,
      fill = if (show_legend) "Scenario" else NULL
    ) +
    theme_classic(base_size = 12) +
    theme(
      legend.position = if (show_legend) "inside" else "none",
      legend.position.inside = c(0.42, 0.32),
      axis.text.x = if (show_x) element_text() else element_blank(),
      axis.ticks.x = if (show_x) element_line() else element_blank(),
      axis.title.x = element_text(margin = margin(t = 10)),
      axis.title.y = element_text(margin = margin(r = 10))
    )
}

figure <-
  make_storage_panel("MEDSTORE", "Lake Mead (Percent Capacity)") /
  make_storage_panel("POWSTORE", "Lake Powell (Percent Capacity)") /
  make_storage_panel(
    "SVRSTORE", "Salt-Verde (Percent Capacity)",
    show_x = TRUE, show_legend = TRUE
  ) +
  plot_annotation(tag_levels = "a")

ggsave("figures/figure_7.png", figure, width = 8, height = 6, dpi = 300,
       scale = 1.6, bg = "white")
