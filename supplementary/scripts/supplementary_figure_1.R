suppressPackageStartupMessages({
  library(ggplot2)
  library(scales)
})
source("scripts/common_data.R")

tested_levels <- seq(2L, 150L, by = 2L)
regional_population <- read_population_lookup()[WEBPOPGR %in% tested_levels]

stopifnot(
  nrow(regional_population) == length(tested_levels),
  identical(regional_population$WEBPOPGR, tested_levels),
  !anyNA(regional_population$population)
)

p <- ggplot(
  regional_population,
  aes(x = WEBPOPGR, y = population, color = WEBPOPGR)
) +
  geom_line(color = "grey70", linewidth = 0.7) +
  geom_point(size = 3.8) +
  scale_color_viridis_c(
    option = "viridis",
    limits = c(2, 150),
    breaks = c(2, 25, 50, 75, 100, 125, 150),
    name = "WEBPOPGR value"
  ) +
  scale_x_continuous(
    breaks = c(2, 25, 50, 75, 100, 125, 150),
    limits = c(0, 152),
    expand = expansion(mult = c(0.01, 0.01))
  ) +
  scale_y_continuous(
    breaks = seq(4e6, 9.5e6, by = 5e5),
    labels = label_number(scale = 1e-6, accuracy = 0.1),
    limits = c(3.9e6, 9.5e6),
    expand = expansion(mult = c(0, 0.01))
  ) +
  labs(
    x = "WaterSim5 population growth input",
    y = "Regional population in 2060 (millions of people)"
  ) +
  theme_classic(base_size = 13) +
  theme(
    axis.title = element_text(size = 13),
    axis.title.x = element_text(margin = margin(t = 14)),
    axis.title.y = element_text(margin = margin(r = 14)),
    axis.text = element_text(size = 12),
    legend.text = element_text(size = 12),
    legend.position = "inside",
    legend.position.inside = c(0.82, 0.28),
    legend.background = element_rect(
      fill = alpha("white", 0.85),
      color = "grey50"
    )
  )

ggsave(
  "supplementary/figures/supplementary_figure_1.png",
  plot = p,
  width = 10,
  height = 6.5,
  dpi = 300,
  bg = "white"
)
