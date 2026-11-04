# ============================================================
# Figure: Global distribution of field studies
#
# Description:
# Generates a world map showing the geographical distribution
# of field surveys included in the systematic review.
#
# Data:
# Survey data are embedded directly in this script.
# Geographical boundaries are retrieved using rnaturalearth.
#
# Output:
# All_studies_map_with_inset.png
# All_studies_map_with_inset.pdf
#
# ============================================================

# Install required packages (run once)
install.packages(c(
  "tidyverse",
  "sf",
  "rnaturalearth",
  "cowplot",
  "lwgeom"
))

# =========================
# LIBRARIES
# =========================
library(tidyverse)
library(sf)
library(rnaturalearth)
library(cowplot)
library(lwgeom)

sf_use_s2(FALSE)

# =========================
# DATA
# =========================
survey_data <- tibble(
  country = c(
    "Taiwan","China","Indonesia","Pakistan","Nepal","India","Japan","Brunei","Thailand",
    "Sweden","Finland","Russia","Czechia","Austria","Poland","Denmark","Norway",
    "Romania","United Kingdom","Germany","Italy","Israel","Netherlands",
    "Canada","United States of America",
    "Argentina","Brazil","Colombia",
    "Zimbabwe"
  ),
  continent = c(
    rep("Asia",9),
    rep("Europe",14),
    rep("North America",2),
    rep("South America",3),
    "Africa"
  ),
  surveys = c(
    1,12,1,1,2,9,1,1,1,
    9,3,4,2,1,4,3,5,
    1,3,8,5,1,1,
    24,19,
    3,3,1,
    1
  )
)

# =========================
# WORLD MAP (REMOVE PROBLEM AREAS)
# =========================
world <- ne_countries(scale = "medium", returnclass = "sf") %>%
  filter(!name %in% c("Antarctica","Greenland"))

# =========================
# JOIN
# =========================
survey_map <- world %>%
  inner_join(survey_data, by = c("name" = "country"))

# =========================
# FIX MULTIPOLYGONS
# =========================
survey_geom <- survey_map %>%
  st_cast("MULTIPOLYGON")

# keep largest polygon per country
largest_geom <- survey_geom %>%
  mutate(area = st_area(geometry)) %>%
  group_by(name) %>%
  slice_max(area, n = 1) %>%
  ungroup() %>%
  select(name, geometry)

# re-attach attributes CLEANLY
survey_map_clean <- largest_geom %>%
  left_join(survey_data, by = c("name" = "country"))

# =========================
# CENTROIDS
# =========================
centroids <- st_point_on_surface(survey_map_clean)

coords <- st_coordinates(centroids)

survey_points <- survey_map_clean %>%
  st_drop_geometry() %>%
  bind_cols(as.data.frame(coords)) %>%
  rename(lon = X, lat = Y)

# =========================
# HARD FIX FOR PROBLEM COUNTRIES
# =========================
survey_points <- survey_points %>%
  mutate(
    lon = case_when(
      name == "Norway" ~ 10.0,     # mainland Norway
      name == "Japan" ~ 138.0,     # central Honshu
      TRUE ~ lon
    ),
    lat = case_when(
      name == "Norway" ~ 62.0,
      name == "Japan" ~ 37.5,
      TRUE ~ lat
    )
  )

# =========================
# LIGHT EUROPE JITTER
# =========================
set.seed(42)

survey_points <- survey_points %>%
  mutate(
    lon = ifelse(continent == "Europe", lon + rnorm(n(), 0, 0.4), lon),
    lat = ifelse(continent == "Europe", lat + rnorm(n(), 0, 0.25), lat)
  )

# =========================
# MICRO ADJUSTMENTS
# =========================
survey_points <- survey_points %>%
  mutate(
    lon = case_when(
      name == "United Kingdom" ~ lon - 1,
      name == "Italy" ~ lon + 0.8,
      TRUE ~ lon
    )
  )

# =========================
# COLORS
# =========================
continent_cols <- c(
  "Asia" = "#F4A3A3",
  "Europe" = "#C7A6FF",
  "North America" = "#FFBE7A",
  "South America" = "#FFF2A6",
  "Africa" = "#088F8F"
)

# =========================
# FINAL MAP
# =========================
map_plot <- ggplot() +
  
  theme_void() +
  
  theme(
    panel.background = element_rect(fill = "#D3E2ED", colour = NA),
    plot.background = element_rect(fill = "#D3E2ED", colour = NA),
    plot.margin = margin(10, 20, 10, 20),
    
    legend.position = "right",
    legend.title = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 12),
    legend.key = element_rect(fill = "#D3E2ED", colour = NA)
  ) +
  
  # LAND
  geom_sf(
    data = world,
    fill = "#D8E4BC",
    color = "grey40",
    linewidth = 0.25
  ) +
  
  # POINTS
  geom_point(
    data = survey_points,
    aes(x = lon, y = lat,
        size = surveys,
        fill = continent),
    shape = 21,
    colour = "black",
    stroke = 0.6
  ) +
  
  scale_fill_manual(values = continent_cols, name = "Continent") +
  
  guides(
    fill = guide_legend(
      override.aes = list(size = 5)
    ),
    size = guide_legend()
  )+
  
  scale_size_continuous(
    range = c(3, 12),
    breaks = c(1,3,5,10,20),
    name = "Number of surveys"
  ) +
  
  coord_sf(
    ylim = c(-60, 90),
    expand = FALSE
  )

# =========================
# EUROPE BOUNDING BOX (for map highlight)
# =========================
europe_bbox <- st_bbox(c(
  xmin = -15, xmax = 40,
  ymin = 35, ymax = 72
), crs = st_crs(world))

europe_box_sf <- st_as_sfc(europe_bbox)

# =========================
# CROP DATA FOR INSET
# =========================
world_europe <- st_crop(world, europe_bbox)

points_europe <- survey_points %>%
  filter(continent == "Europe")

# =========================
# EUROPE INSET (CLEAN STYLE)
# =========================
europe_inset <- ggplot() +
  
  theme_void() +
  
  theme(
    panel.background = element_rect(fill = "#D3E2ED", colour = "black", linewidth = 0.4),
    plot.background = element_rect(fill = "#D3E2ED", colour = NA)
  ) +
  
  geom_sf(
    data = world_europe,
    fill = "#D8E4BC",
    color = "grey40",
    linewidth = 0.2
  ) +
  
  geom_point(
    data = points_europe,
    aes(x = lon, y = lat,
        size = surveys,
        fill = continent),
    shape = 21,
    colour = "black",
    stroke = 0.5
  ) +
  
  scale_fill_manual(values = continent_cols) +
  scale_size(range = c(2, 8)) +
  
  coord_sf(
    xlim = c(-15, 40),
    ylim = c(35, 72),
    expand = FALSE
  ) +
  
  guides(size = "none", fill = "none")

# =========================
# ADD BOX TO MAIN MAP
# =========================
map_with_box <- map_plot +
  geom_sf(
    data = europe_box_sf,
    fill = NA,
    colour = "black",
    linewidth = 0.4,
    linetype = "dashed"
  )

# =========================
# COMBINE (LEFT-SIDE INSET)
# =========================
final_plot <- ggdraw() +
  draw_plot(map_with_box) +
  draw_plot(
    europe_inset,
    x = -0.03, 
    y = 0.16,   
    width = 0.25,
    height = 0.42
  )

final_plot

# =========================
# EXPORT FIGURE
# =========================

ggsave(
  filename = "All_studies_map_with_inset.png",
  plot = final_plot,
  width = 12,
  height = 6,
  units = "in",
  dpi = 300,
  bg = "#D3E2ED"
)