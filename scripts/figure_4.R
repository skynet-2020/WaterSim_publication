suppressPackageStartupMessages({
  library(ggplot2)
})
source("scripts/common_data.R")

results <- read_primary_results(fields = "SINYRGWR", years = 2060)
population_lookup <- read_population_lookup()

summary_data <- results[, .(
  minimum = as.numeric(min(WEBPOPGR)),
  lower_quartile = as.numeric(quantile(WEBPOPGR, 0.25)),
  median = as.numeric(median(WEBPOPGR)),
  upper_quartile = as.numeric(quantile(WEBPOPGR, 0.75)),
  maximum = as.numeric(max(WEBPOPGR))
), by = .(groundwater_years = value)]

for (column in c("minimum", "lower_quartile", "median", "upper_quartile", "maximum")) {
  summary_data[, (column) := population_from_webpopgr(get(column), population_lookup)]
}
setorder(summary_data, groundwater_years)

p <- ggplot(summary_data, aes(x = groundwater_years)) +
  geom_ribbon(
    aes(ymin = minimum, ymax = maximum, fill = "Full Range (Min-Max)"),
    alpha = 0.3
  ) +
  geom_ribbon(
    aes(ymin = lower_quartile, ymax = upper_quartile,
        fill = "Interquartile Range (25-75%)"),
    alpha = 0.5
  ) +
  geom_line(aes(y = median, color = "Median"), linewidth = 1) +
  scale_fill_manual(values = c(
    "Full Range (Min-Max)" = "lightblue",
    "Interquartile Range (25-75%)" = "blue"
  )) +
  scale_color_manual(values = c("Median" = "red")) +
  scale_x_continuous(breaks = seq(45, 100, 5)) +
  scale_y_continuous(
    limits = c(4e6, 10e6), breaks = seq(4e6, 10e6, 1e6),
    labels = 4:10
  ) +
  labs(
    x = "Groundwater supply remaining in 2060 (years)",
    y = "Projected population in 2060 (millions)"
  ) +
  theme_classic(base_size = 14) +
  theme(
    legend.title = element_blank(),
    legend.position = "inside",
    legend.position.inside = c(0.25, 0.25),
    axis.title.x = element_text(margin = margin(t = 12)),
    axis.title.y = element_text(margin = margin(r = 12))
  )

ggsave("figures/figure_4.png", p, width = 10, height = 6, dpi = 300, bg = "white")
