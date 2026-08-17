suppressPackageStartupMessages({
  library(sf)
  library(ggplot2)
  library(patchwork)
  library(ggspatial)
  library(cowplot)
})

# Rebuild and retain the detailed provider-area map as the main panel.
source("scripts/figure_1_provider_map.R")
provider_map <- map

states_path <- "data/spatial/us_states_2024_cartographic.rds"
ama_path <- "data/spatial/phoenix_ama/AMA_and_INA.shp"
output_dir <- "figures"

states <- readRDS(states_path)
lower_48 <- states[states$STUSPS %in% setdiff(state.abb, c("AK", "HI")), ] |>
  st_transform(5070)
lower_48$highlight <- ifelse(lower_48$STUSPS == "AZ", "Arizona", "Other states")

locator_theme <- theme_minimal(base_size = 9) +
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(color = "#222222", fill = NA, linewidth = 0.6),
    axis.title.x = element_text(size = 11, margin = margin(t = 5)),
    axis.title.y = element_text(size = 11, margin = margin(r = 5)),
    axis.text = element_text(size = 9, color = "#333333"),
    plot.title = element_text(size = 11, face = "bold", hjust = 0.5, margin = margin(b = 4)),
    plot.margin = margin(5, 5, 5, 5)
  )

us_locator <- ggplot(lower_48) +
  geom_sf(aes(fill = highlight), color = "#555555", linewidth = 0.18) +
  annotation_scale(
    location = "bl", width_hint = 0.24, text_cex = 0.48,
    pad_x = grid::unit(0.08, "in"), pad_y = grid::unit(0.07, "in")
  ) +
  annotation_north_arrow(
    location = "tr", which_north = "true",
    height = grid::unit(0.34, "in"), width = grid::unit(0.34, "in"),
    pad_x = grid::unit(0.08, "in"), pad_y = grid::unit(0.07, "in"),
    style = north_arrow_fancy_orienteering(
      line_width = 0.6, text_size = 6, text_face = "bold"
    )
  ) +
  scale_fill_manual(values = c("Arizona" = "#303030", "Other states" = "#D9EAF2")) +
  coord_sf(crs = st_crs(5070), expand = FALSE, datum = st_crs(4326)) +
  labs(title = NULL, x = NULL, y = "Latitude") +
  guides(fill = "none") +
  locator_theme

arizona <- states[states$STUSPS == "AZ", ] |>
  st_transform(26912)
phoenix_ama <- st_read(ama_path, quiet = TRUE) |>
  subset(BASIN_NAME == "PHOENIX AMA") |>
  st_transform(26912)

provider_footprint <- provider_areas_projected |>
  st_transform(26912) |>
  st_union() |>
  st_as_sf()

provider_areas_az <- provider_areas_projected |>
  st_transform(26912)
provider_areas_az$area_type <- ifelse(
  provider_areas_az$boundary_type == "incorporated_city_boundary",
  "City provider areas",
  "Other water-provider areas"
)

# Exact geographic extent used by the detailed provider-area map, including
# its small plotting margin, for nesting the regional panel in the AZ locator.
region_extent <- st_as_sfc(st_bbox(c(
  xmin = unname(bbox["xmin"] - x_padding),
  ymin = unname(bbox["ymin"] - y_padding),
  xmax = unname(bbox["xmax"] + x_padding),
  ymax = unname(bbox["ymax"] + y_padding)
), crs = st_crs(4326))) |>
  st_transform(26912)

az_bbox <- st_bbox(arizona)
az_x_padding <- unname((az_bbox["xmax"] - az_bbox["xmin"]) * 0.035)
az_y_padding <- unname((az_bbox["ymax"] - az_bbox["ymin"]) * 0.035)

ama_bbox <- st_bbox(phoenix_ama)
ama_width <- unname(ama_bbox["xmax"] - ama_bbox["xmin"])
ama_height <- unname(ama_bbox["ymax"] - ama_bbox["ymin"])
ama_label <- data.frame(
  x = unname(ama_bbox["xmax"] + 0.08 * ama_width),
  y = unname(ama_bbox["ymax"] + 0.04 * ama_height),
  xend = unname(ama_bbox["xmax"] - 0.12 * ama_width),
  yend = unname(ama_bbox["ymax"] - 0.04 * ama_height),
  label = "Phoenix AMA"
)

provider_label <- st_point_on_surface(provider_footprint)
provider_label_xy <- st_coordinates(provider_label)
provider_label_df <- data.frame(
  x = unname((ama_bbox["xmin"] + ama_bbox["xmax"]) / 2),
  y = unname(ama_bbox["ymin"] - 0.13 * ama_height),
  xend = provider_label_xy[, 1],
  yend = provider_label_xy[, 2],
  label = "Phoenix Metropolitan\nArea (PMA)"
)

az_locator <- ggplot() +
  geom_sf(data = arizona, fill = NA, color = "#555555", linewidth = 0.45) +
  geom_sf(
    data = provider_areas_az,
    aes(fill = area_type),
    color = "#555555",
    linewidth = 0.12
  ) +
  geom_sf(
    data = region_extent,
    fill = NA,
    color = "#111111",
    linewidth = 0.55,
    linetype = "solid"
  ) +
  annotate(
    "text",
    x = unname((az_bbox["xmin"] + az_bbox["xmax"]) / 2),
    y = unname(az_bbox["ymax"] - 0.17 * (az_bbox["ymax"] - az_bbox["ymin"])),
    label = "Arizona",
    fontface = "bold",
    size = 4.2,
    color = "#222222"
  ) +
  annotation_scale(
    location = "bl", width_hint = 0.24, text_cex = 0.48,
    pad_x = grid::unit(0.08, "in"), pad_y = grid::unit(0.07, "in")
  ) +
  annotation_north_arrow(
    location = "tr", which_north = "true",
    height = grid::unit(0.34, "in"), width = grid::unit(0.34, "in"),
    pad_x = grid::unit(0.08, "in"), pad_y = grid::unit(0.07, "in"),
    style = north_arrow_fancy_orienteering(
      line_width = 0.6, text_size = 6, text_face = "bold"
    )
  ) +
  scale_fill_manual(
    values = c(
      "City provider areas" = "#6BAED6",
      "Other water-provider areas" = "#BDBDBD"
    ),
    guide = "none"
  ) +
  coord_sf(
    crs = st_crs(26912),
    xlim = c(az_bbox["xmin"] - az_x_padding, az_bbox["xmax"] + az_x_padding),
    ylim = c(az_bbox["ymin"] - az_y_padding, az_bbox["ymax"] + az_y_padding),
    expand = FALSE,
    datum = st_crs(4326)
  ) +
  labs(x = "Longitude", y = "Latitude") +
  locator_theme +
  theme(axis.title.x = element_text(size = 11, margin = margin(t = 12)))

ggsave(
  file.path(output_dir, "locator_contiguous_us_arizona.png"),
  us_locator,
  width = 5.2, height = 3.2, units = "in", dpi = 400, bg = "white"
)

ggsave(
  file.path(output_dir, "locator_arizona_phoenix_ama.png"),
  az_locator,
  width = 4.0, height = 4.2, units = "in", dpi = 400, bg = "white"
)

us_locator_composite <- us_locator +
  theme(plot.margin = margin(-7, 5, 5, 5))
az_locator_composite <- az_locator +
  theme(plot.margin = margin(5, 5, -7, 5))
provider_map_composite <- provider_map +
  theme(plot.margin = margin(19, 10, 15, 0))

combined <- ggdraw() +
  draw_plot(us_locator_composite, x = 0.06, y = 0.641, width = 0.28, height = 0.334) +
  draw_plot(az_locator_composite, x = 0.06, y = 0.0397, width = 0.28, height = 0.558) +
  draw_plot(provider_map_composite, x = 0.32, y = 0, width = 0.68, height = 1)

ggsave(
  file.path(output_dir, "figure_1.png"),
  combined,
  width = 13.2, height = 7.2, units = "in", dpi = 400, bg = "white"
)

message("Saved locator maps and combined nested-geography figure in: ", output_dir)
