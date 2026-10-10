suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
  library(patchwork)
  library(ragg)
})

input_path <- file.path(
  "internal_validation", "matched_frontier", "tables",
  "matched_frontier_pairwise_contrasts.csv"
)
output_path <- file.path(
  "internal_validation", "matched_frontier", "figures",
  "figure_matched_frontier_contrasts.png"
)

matched <- fread(input_path)

factor_order <- c(
  "Climate", "Environmental flow", "MWUE", "AWUE", "AWT",
  "Supply augmentation"
)

contrast_order <- c(
  "Reduced river flows - Reference river flows",
  "80,000 AF - 0 AF",
  "25% reduction - 3.2% reduction",
  "45% reduction - 3.2% reduction",
  "45% reduction - 25% reduction",
  "10% below 2015 - Current (2015)",
  "20% below 2015 - Current (2015)",
  "20% below 2015 - 10% below 2015",
  "50% - 0%",
  "100% - 0%",
  "100% - 50%",
  "10% - 0%",
  "20% - 0%",
  "20% - 10%"
)

contrast_labels <- c(
  "Reduced flows - reference flows",
  "80,000 AF - 0 AF",
  "25% - 3.2% reduction",
  "45% - 3.2% reduction",
  "45% - 25% reduction",
  "10% below - current",
  "20% below - current",
  "20% below - 10% below",
  "50% - 0%",
  "100% - 0%",
  "100% - 50%",
  "10% - 0%",
  "20% - 0%",
  "20% - 10%"
)
names(contrast_labels) <- contrast_order

stopifnot(
  nrow(matched) == 14L * 11L,
  identical(sort(unique(matched$gsi_threshold)), 83:93),
  setequal(unique(matched$focal_factor), factor_order),
  setequal(unique(matched$contrast), contrast_order),
  !anyNA(matched$median_difference),
  all(abs(
    matched$pct_positive + matched$pct_negative + matched$pct_zero - 100
  ) < 1e-8),
  all(matched[focal_factor %in% c("Climate", "Environmental flow")]$n == 162L),
  all(matched[!focal_factor %in% c("Climate", "Environmental flow")]$n == 108L)
)

matched[, focal_factor := factor(focal_factor, levels = factor_order)]
matched[, contrast := factor(contrast, levels = contrast_order)]
matched[, contrast_label := factor(
  unname(contrast_labels[as.character(contrast)]),
  levels = rev(unname(contrast_labels))
)]

# For each contrast and threshold, identify the most common matched response:
# expansion, contraction, or no change. This remains meaningful when a median
# of zero results from a mixture of positive and negative shifts.
response_shares <- as.matrix(
  matched[, .(pct_positive, pct_negative, pct_zero)]
)
dominant_index <- max.col(response_shares, ties.method = "first")
dominant_names <- c("Expansion", "Contraction", "No change")
dominant_symbols <- c("+", "-", "0")
matched[, dominant_response := dominant_names[dominant_index]]
matched[, dominant_symbol := dominant_symbols[dominant_index]]
matched[, dominant_pct := apply(response_shares, 1, max)]

matched[, median_label := fifelse(
  abs(median_difference) < 0.005,
  "0.00",
  sprintf("%+.2f", median_difference)
)]
matched[, dominant_label := sprintf(
  "%s %.0f%%", dominant_symbol, dominant_pct
)]

signed_limit <- ceiling(max(abs(matched$median_difference)) * 10) / 10
matched[, median_text_color := fifelse(
  abs(median_difference) >= 0.58 * signed_limit, "white", "black"
)]
matched[, dominant_text_color := fifelse(
  dominant_pct >= 82, "white", "black"
)]

base_theme <- theme_minimal(base_size = 11) +
  theme(
    panel.grid = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_text(size = 9),
    axis.text.y = element_text(size = 8.6, color = "black"),
    axis.title.x = element_text(size = 11, margin = margin(t = 8)),
    strip.placement = "outside",
    strip.text.y.left = element_text(
      angle = 0, face = "bold", size = 9, hjust = 1
    ),
    strip.background = element_blank(),
    panel.spacing.y = unit(0.12, "in"),
    legend.position = "right",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9),
    plot.margin = margin(5, 9, 5, 5)
  )

panel_a <- ggplot(
  matched,
  aes(x = gsi_threshold, y = contrast_label, fill = median_difference)
) +
  geom_tile(color = "white", linewidth = 0.45) +
  geom_text(
    aes(label = median_label, color = median_text_color),
    size = 3.05, fontface = "bold"
  ) +
  scale_color_identity() +
  scale_fill_gradient2(
    low = "#2166AC", mid = "#F7F7F7", high = "#B2182B",
    midpoint = 0, limits = c(-signed_limit, signed_limit),
    name = "Median matched frontier shift\n(millions of people)",
    guide = guide_colorbar(
      title.position = "top", barwidth = unit(0.20, "in"),
      barheight = unit(3.7, "in")
    )
  ) +
  scale_x_continuous(breaks = 83:93, expand = c(0, 0)) +
  facet_grid(
    rows = vars(focal_factor), scales = "free_y", space = "free_y",
    switch = "y"
  ) +
  labs(x = "Minimum GSI threshold (years)") +
  base_theme

panel_b <- ggplot(
  matched,
  aes(x = gsi_threshold, y = contrast_label, fill = dominant_pct)
) +
  geom_tile(color = "white", linewidth = 0.45) +
  geom_text(
    aes(label = dominant_label, color = dominant_text_color),
    size = 3.05, fontface = "bold"
  ) +
  scale_color_identity() +
  scale_fill_viridis_c(
    option = "magma", direction = -1, limits = c(33, 100),
    breaks = c(40, 50, 60, 70, 80, 90, 100),
    labels = paste0(c(40, 50, 60, 70, 80, 90, 100), "%"),
    name = "Matched backgrounds with most\ncommon response (%)",
    guide = guide_colorbar(
      title.position = "top", barwidth = unit(0.20, "in"),
      barheight = unit(3.7, "in")
    )
  ) +
  scale_x_continuous(breaks = 83:93, expand = c(0, 0)) +
  facet_grid(
    rows = vars(focal_factor), scales = "free_y", space = "free_y",
    switch = "y"
  ) +
  labs(x = "Minimum GSI threshold (years)") +
  base_theme

figure <- panel_a / panel_b +
  plot_layout(heights = c(1, 1))

agg_png(
  output_path, width = 4800, height = 5700, units = "px", res = 360,
  background = "white"
)
print(figure)
dev.off()

cat("Matched contrast cells:", nrow(matched), "\n")
cat("GSI thresholds:", paste(range(matched$gsi_threshold), collapse = "-"), "\n")
cat("Contrasts:", uniqueN(matched$contrast), "\n")
cat("Dominant-response range:",
    sprintf("%.1f-%.1f%%", min(matched$dominant_pct), max(matched$dominant_pct)),
    "\n")
