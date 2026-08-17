suppressPackageStartupMessages({
  library(ggplot2)
})
source("scripts/common_data.R")

results <- read_primary_results(fields = "SINYRGWR", years = 2060)
stopifnot(nrow(results) == 24300L)

count_100 <- results[value >= 100, .N]

p <- ggplot(results, aes(x = value)) +
  geom_histogram(binwidth = 1, boundary = 0, fill = "skyblue", color = "black") +
  annotate(
    "segment", x = 85, xend = 98.8, y = 2500, yend = count_100 + 35,
    arrow = arrow(length = unit(0.15, "in")), color = "#17658A"
  ) +
  annotate(
    "text", x = 78, y = 2610,
    label = "Scenarios reaching\nor exceeding 100 years", size = 5.3
  ) +
  scale_x_continuous(limits = c(40, 100), breaks = seq(40, 100, 10)) +
  scale_y_continuous(limits = c(0, 3100), breaks = seq(0, 3000, 1000)) +
  labs(
    x = "Groundwater supply remaining in 2060 (years)",
    y = "Number of experimental scenarios"
  ) +
  theme_classic(base_size = 15) +
  theme(
    axis.title.x = element_text(margin = margin(t = 12)),
    axis.title.y = element_text(margin = margin(r = 12))
  )

ggsave("figures/figure_3.png", p, width = 10, height = 5, dpi = 300, bg = "white")
