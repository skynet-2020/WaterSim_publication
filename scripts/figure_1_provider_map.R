suppressPackageStartupMessages({
  library(sf)
  library(ggplot2)
  library(shadowtext)
  library(ggspatial)
})

input_path <- "data/spatial/ICB_provider_areas_transformed.rds"
output_path <- "figures/figure_1_provider_map.png"

provider_areas_projected <- readRDS(input_path)
provider_areas <- st_transform(provider_areas_projected, 4326)

provider_areas$area_type <- ifelse(
  provider_areas$boundary_type == "incorporated_city_boundary",
  "City provider areas",
  "Other water-provider areas"
)

# WaterSim calls the provider associated with Fountain Hills "Chaparral City";
# the map uses the familiar municipal name stored in the spatial dataset.
city_areas_projected <- provider_areas_projected[
  provider_areas_projected$boundary_type == "incorporated_city_boundary",
]
label_points <- st_transform(st_point_on_surface(city_areas_projected), 4326)
label_xy <- st_coordinates(label_points)
labels <- cbind(st_drop_geometry(label_points), label_xy)

bbox <- st_bbox(provider_areas)
x_padding <- unname((bbox["xmax"] - bbox["xmin"]) * 0.015)
y_padding <- unname((bbox["ymax"] - bbox["ymin"]) * 0.015)

map <- ggplot() +
  geom_sf(
    data = provider_areas,
    aes(fill = area_type),
    color = "white",
    linewidth = 0.28
  ) +
  geom_sf(
    data = provider_areas,
    fill = NA,
    color = "#303030",
    linewidth = 0.18
  ) +
  geom_shadowtext(
    data = labels,
    aes(x = X, y = Y, label = name),
    size = 4.2,
    color = "#111111",
    bg.color = "white",
    bg.r = 0.12,
    fontface = "bold"
  ) +
  annotation_scale(
    location = "bl",
    width_hint = 0.18,
    text_cex = 0.65,
    pad_x = grid::unit(0.16, "in"),
    pad_y = grid::unit(0.15, "in")
  ) +
  annotation_north_arrow(
    location = "tr",
    which_north = "true",
    height = grid::unit(0.46, "in"),
    width = grid::unit(0.46, "in"),
    pad_x = grid::unit(0.15, "in"),
    pad_y = grid::unit(0.15, "in"),
    style = north_arrow_fancy_orienteering(
      line_width = 0.7,
      text_size = 7,
      text_face = "bold"
    )
  ) +
  scale_fill_manual(
    values = c(
      "City provider areas" = "#6BAED6",
      "Other water-provider areas" = "#BDBDBD"
    ),
    breaks = c(
      "City provider areas",
      "Other water-provider areas"
    ),
    name = NULL
  ) +
  coord_sf(
    xlim = c(bbox["xmin"] - x_padding, bbox["xmax"] + x_padding),
    ylim = c(bbox["ymin"] - y_padding, bbox["ymax"] + y_padding),
    expand = FALSE,
    datum = st_crs(4326)
  ) +
  labs(x = "Longitude", y = NULL) +
  guides(fill = guide_legend(override.aes = list(color = "#303030", linewidth = 0.3))) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "#222222", fill = NA, linewidth = 0.6),
    axis.title.x = element_text(size = 11, color = "#222222", margin = margin(t = 12)),
    axis.title.y = element_text(size = 11, color = "#222222", margin = margin(r = 12)),
    axis.text = element_text(size = 9, color = "#333333"),
    legend.position = "inside",
    legend.position.inside = c(0.018, 0.982),
    legend.justification = c(0, 1),
    legend.direction = "vertical",
    legend.background = element_rect(
      fill = scales::alpha("white", 0.88),
      color = "#666666",
      linewidth = 0.3
    ),
    legend.margin = margin(5, 7, 5, 7),
    legend.text = element_text(size = 10),
    legend.key.width = grid::unit(0.7, "in"),
    plot.margin = margin(8, 10, 4, 8)
  )

ggsave(
  output_path,
  plot = map,
  width = 9,
  height = 7.2,
  units = "in",
  dpi = 400,
  bg = "white"
)

message("Saved: ", output_path)
