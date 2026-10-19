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
    "Reduced flows", "Reference flows"
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
  projections[, climate := factor(
    climate,
    levels = c("Reference flows", "Reduced flows")
  )]
  setorder(projections, climate, year)

  p <- ggplot() +
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
      "Reduced flows" = "#F8766D",
      "Reference flows" = "#00BFC4"
    ), guide = "none") +
    scale_fill_manual(values = c(
      "Reduced flows" = "#F8766D",
      "Reference flows" = "#00BFC4"
    ), guide = "none") +
    scale_x_continuous(breaks = seq(2000, 2060, 10)) +
    scale_y_continuous(limits = c(0, 105), breaks = seq(0, 100, 20)) +
    labs(
      x = if (show_x) "Year" else NULL,
      y = y_label,
      color = NULL,
      fill = NULL
    ) +
    theme_classic(base_size = 18) +
    theme(
      legend.position = "none",
      axis.text = element_text(size = 17),
      axis.text.x = if (show_x) element_text(size = 17) else element_blank(),
      axis.ticks.x = if (show_x) element_line() else element_blank(),
      axis.title = element_text(size = 19),
      axis.title.x = element_text(size = 19, margin = margin(t = 12)),
      axis.title.y = element_text(size = 19, margin = margin(r = 12))
    )

  if (show_legend) {
    p <- p +
      annotate(
        "rect", xmin = 2003.0, xmax = 2020.5, ymin = 73, ymax = 104.5,
        fill = "white", color = "grey55", linewidth = 0.35
      ) +
      annotate(
        "segment", x = 2004.0, xend = 2006.2, y = 99.5, yend = 99.5,
        color = "#00BFC4", linewidth = 1.1
      ) +
      annotate(
        "text", x = 2007.0, y = 99.5, label = "Reference flows",
        hjust = 0, size = 5.2
      ) +
      annotate(
        "segment", x = 2004.0, xend = 2006.2, y = 88.5, yend = 88.5,
        color = "#F8766D", linewidth = 1.1
      ) +
      annotate(
        "text", x = 2007.0, y = 88.5, label = "Reduced flows",
        hjust = 0, size = 5.2
      ) +
      annotate(
        "segment", x = 2004.0, xend = 2006.2, y = 77.5, yend = 77.5,
        color = "black", linewidth = 1.1
      ) +
      annotate(
        "text", x = 2007.0, y = 77.5, label = "Spin-Up (Observed)",
        hjust = 0, size = 5.2
      )
  }

  p
}

figure <-
  make_storage_panel(
    "MEDSTORE", "Lake Mead\n(Percent Capacity)"
  ) /
  make_storage_panel(
    "POWSTORE", "Lake Powell\n(Percent Capacity)", show_legend = TRUE
  ) /
  make_storage_panel(
    "SVRSTORE", "Salt-Verde\n(Percent Capacity)",
    show_x = TRUE
  ) +
  plot_annotation(
    tag_levels = "a",
    theme = theme(plot.tag = element_text(size = 20))
  )

ggsave("figures/figure_7.png", figure, width = 8, height = 6, dpi = 300,
       scale = 1.6, bg = "white")
