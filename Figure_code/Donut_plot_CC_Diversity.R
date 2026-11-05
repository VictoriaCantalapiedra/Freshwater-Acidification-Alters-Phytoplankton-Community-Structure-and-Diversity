# ============================================================
# Figure: Influence of pH and DIC on community composition and diversity
# Cantalapiedra Mateo et al. (2026, submitted)
# Author: Victoria Cantalapiedra Mateo      Licence: MIT (see Figure_code/LICENSE_MIT)
#
# Description:
#   Composition donut records the number of times each driver was found to be a significant influencing factor (YES/NO). 
#   Diversity donut shows positive (+) and negative (-) effects of each driver on the measured diversity 
#   Both donut plots are first created separately and then combined into a single figure.
#   Drivers: reduced pH, increased DIC, and both combined
#   
#
# Data:
#   The count data are embedded in this script, so it needs no input files.
#   Counts are observations per driver and response, 
#   derived from the studies listed in
#   Data/Cantalapiedra_et_al_full_text_screening.csv, and match
#   Data/Cantalapiedra_et_al_Community_composition.csv (which also includes data for abundance counts for separate plots found in Figure_code/Donut_plot_abundances.R)  
#
# Output (written to the working directory):
#   community_composition_donut.pdf
#   diversity_donut.pdf 
#   composition_diversity_figure.pdf
#
# ============================================================

# Install required packages (run once if needed)
#install.packages(c(
# "tidyverse",
# "ggnewscale",
# "patchwork"
# ))

# =========================
# LIBRARIES
# =========================  

library(ggplot2)
library(dplyr)
library(tibble)
library(ggnewscale)
library(patchwork)

# =========================
# COMMUNITY COMPOSITION
# =========================

# ----------------------------
# Data
# ----------------------------
data <- tribble(
  ~Composition, ~Driver, ~Count,
  "Yes", "pH", 82,
  "Yes", "DIC", 19,
  "Yes", "DIC + pH", 12,
  "No",  "pH", 5,
  "No",  "DIC", 2,
  "No",  "DIC + pH", 16
)

# ----------------------------
# Global settings
# ----------------------------
composition_levels <- c("Yes", "No")
driver_levels <- c("pH", "DIC", "DIC + pH")

driver_colors <- c(
  "pH"       = "#D9B44A",
  "DIC"      = "#2F9FB3",
  "DIC + pH" = "#8A8A8A"
)

composition_labels <- c(
  "Yes" = "YES",
  "No" = "NO"
)

composition_fills <- c(
  "Yes" = "white",
  "No" = "black"
)

composition_text_cols <- c(
  "Yes" = "black",
  "No" = "white"
)

# ----------------------------
# Donut function
# ----------------------------
make_composition_donut <- function(df) {
  long_data <- df %>%
    mutate(
      Composition = factor(Composition, levels = composition_levels),
      Driver = factor(Driver, levels = driver_levels)
    ) %>%
    filter(Count > 0) %>%
    arrange(Composition, Driver) %>%
    group_by(Composition) %>%
    mutate(
      ymax = cumsum(Count),
      ymin = ymax - Count
    ) %>%
    ungroup()
  
  composition_totals <- long_data %>%
    group_by(Composition) %>%
    summarise(total = sum(Count), .groups = "drop") %>%
    mutate(offset = lag(cumsum(total), default = 0))
  
  long_data <- long_data %>%
    left_join(composition_totals, by = "Composition") %>%
    mutate(
      ymin = ymin + offset,
      ymax = ymax + offset
    )
  
  inner_data <- composition_totals %>%
    mutate(
      ymin = offset,
      ymax = offset + total,
      label = composition_labels[as.character(Composition)],
      fill = composition_fills[as.character(Composition)],
      text_col = composition_text_cols[as.character(Composition)]
    )
  
  outer_labels <- long_data %>%
    mutate(
      ymid = (ymin + ymax) / 2,
      label = Count
    )
  
  ggplot() +
    geom_rect(
      data = long_data,
      aes(
        ymin = ymin, ymax = ymax,
        xmin = 1.05, xmax = 2.35,
        fill = Driver
      ),
      color = "white",
      linewidth = 0.7,
      show.legend = TRUE
    ) +
    scale_fill_manual(
      values = driver_colors,
      limits = driver_levels,
      drop = FALSE,
      name = NULL
    ) +
    guides(fill = guide_legend(nrow = 1, byrow = TRUE)) +
    ggnewscale::new_scale_fill() +
    geom_rect(
      data = inner_data,
      aes(
        ymin = ymin, ymax = ymax,
        xmin = 0, xmax = 1.05,
        fill = fill
      ),
      color = "black",
      linewidth = 0.7,
      show.legend = FALSE
    ) +
    scale_fill_identity() +
    geom_text(
      data = outer_labels,
      aes(
        x = 2.68,
        y = ymid,
        label = label
      ),
      size = 5
    ) +
    geom_text(
      data = inner_data,
      aes(
        x = 0.52,
        y = (ymin + ymax) / 2,
        label = label
      ),
      color = inner_data$text_col,
      size = 3.8,
      fontface = "bold"
    ) +
    geom_rect(
      data = inner_data,
      aes(
        ymin = ymin, ymax = ymax,
        xmin = 0, xmax = 2.35
      ),
      fill = NA,
      color = "grey20",
      linewidth = 1.2,
      inherit.aes = FALSE
    ) +
    coord_polar(theta = "y", clip = "off") +
    xlim(0, 2.95) +
    theme_void() +
    theme(
      plot.title = element_text(
        hjust = 0.5,
        size = 16,
        face = "bold",
        margin = margin(b = 6)
      ),
      legend.position = "bottom",
      legend.text = element_text(size = 11),
      legend.key = element_blank(),
      legend.margin = margin(t = -4, b = 0),
      legend.box.margin = margin(t = -12, b = 0),
      plot.margin = margin(10, 20, 2, 20)
    ) +
    labs(title = "Community composition")
}

# ----------------------------
# Build and export
# ----------------------------
community_composition_plot <- make_composition_donut(data)

community_composition_plot

ggsave(
  filename = "community_composition_donut.pdf",
  plot = community_composition_plot,
  width = 5,
  height = 5,
  units = "in"
)

# =========================
# DIVERSITY
# =========================

# ----------------------------
# Diversity data
# ----------------------------
diversity_data <- tribble(
  ~Response, ~Driver, ~Count,
  "Positive", "pH", 1,
  "Positive", "DIC", 1,
  "Negative", "pH", 27,
  "Negative", "DIC", 3
)

# ----------------------------
# Diversity settings
# ----------------------------
diversity_response_levels <- c("Positive", "Negative")
diversity_driver_levels <- c("pH", "DIC")

diversity_driver_colors <- c(
  "pH" = "#D9B44A",
  "DIC" = "#2F9FB3"
)

diversity_response_labels <- c(
  "Positive" = "+",
  "Negative" = "-"
)

diversity_response_fills <- c(
  "Positive" = "white",
  "Negative" = "black"
)

diversity_response_text_cols <- c(
  "Positive" = "black",
  "Negative" = "white"
)

# ----------------------------
# Diversity donut function
# ----------------------------
make_diversity_donut <- function(df) {
  diversity_long_data <- df %>%
    mutate(
      Response = factor(Response, levels = diversity_response_levels),
      Driver = factor(Driver, levels = diversity_driver_levels)
    ) %>%
    filter(Count > 0) %>%
    arrange(Response, Driver) %>%
    group_by(Response) %>%
    mutate(
      ymax = cumsum(Count),
      ymin = ymax - Count
    ) %>%
    ungroup()
  
  diversity_response_totals <- diversity_long_data %>%
    group_by(Response) %>%
    summarise(total = sum(Count), .groups = "drop") %>%
    mutate(offset = lag(cumsum(total), default = 0))
  
  diversity_long_data <- diversity_long_data %>%
    left_join(diversity_response_totals, by = "Response") %>%
    mutate(
      ymin = ymin + offset,
      ymax = ymax + offset
    )
  
  diversity_inner_data <- diversity_response_totals %>%
    mutate(
      ymin = offset,
      ymax = offset + total,
      label = diversity_response_labels[as.character(Response)],
      fill = diversity_response_fills[as.character(Response)],
      text_col = diversity_response_text_cols[as.character(Response)]
    )
  
  diversity_outer_labels <- diversity_long_data %>%
    mutate(
      ymid = (ymin + ymax) / 2,
      label = Count
    )
  
  ggplot() +
    geom_rect(
      data = diversity_long_data,
      aes(
        ymin = ymin, ymax = ymax,
        xmin = 1.05, xmax = 2.35,
        fill = Driver
      ),
      color = "white",
      linewidth = 0.7,
      show.legend = TRUE
    ) +
    scale_fill_manual(
      values = diversity_driver_colors,
      limits = diversity_driver_levels,
      drop = FALSE,
      name = NULL
    ) +
    guides(fill = guide_legend(nrow = 1, byrow = TRUE)) +
    ggnewscale::new_scale_fill() +
    geom_rect(
      data = diversity_inner_data,
      aes(
        ymin = ymin, ymax = ymax,
        xmin = 0, xmax = 1.05,
        fill = fill
      ),
      color = "black",
      linewidth = 0.7,
      show.legend = FALSE
    ) +
    scale_fill_identity() +
    geom_text(
      data = diversity_outer_labels,
      aes(
        x = 2.68,
        y = ymid,
        label = label
      ),
      size = 5
    ) +
    geom_text(
      data = diversity_inner_data,
      aes(
        x = 0.52,
        y = (ymin + ymax) / 2,
        label = label
      ),
      color = diversity_inner_data$text_col,
      size = 4.2,
      fontface = "bold"
    ) +
    geom_rect(
      data = diversity_inner_data,
      aes(
        ymin = ymin, ymax = ymax,
        xmin = 0, xmax = 2.35
      ),
      fill = NA,
      color = "grey20",
      linewidth = 1.2,
      inherit.aes = FALSE
    ) +
    coord_polar(theta = "y", clip = "off") +
    xlim(0, 2.95) +
    theme_void() +
    theme(
      plot.title = element_text(
        hjust = 0.5,
        size = 16,
        face = "bold",
        margin = margin(b = 6)
      ),
      legend.position = "bottom",
      legend.text = element_text(size = 11),
      legend.key = element_blank(),
      legend.margin = margin(t = -4, b = 0),
      legend.box.margin = margin(t = -12, b = 0),
      plot.margin = margin(10, 20, 2, 20)
    ) +
    labs(title = "Diversity")
}

# ----------------------------
# Build diversity plot
# ----------------------------
diversity_plot <- make_diversity_donut(diversity_data)

diversity_plot

ggsave(
  filename = "diversity_donut.pdf",
  plot = diversity_plot,
  width = 5,
  height = 5,
  units = "in"
)

# ----------------------------
# Combined composition + diversity figure
# ----------------------------
combined_driver_legend_data <- tibble(
  Driver = factor(driver_levels, levels = driver_levels),
  x = seq_along(driver_levels),
  y = 1
)

combined_driver_legend <- ggplot(
  combined_driver_legend_data,
  aes(x = x, y = y, colour = Driver)
) +
  geom_point(alpha = 0, size = 6) +
  scale_colour_manual(
    values = driver_colors,
    limits = driver_levels,
    drop = FALSE,
    name = NULL
  ) +
  guides(colour = guide_legend(
    nrow = 1,
    byrow = TRUE,
    override.aes = list(alpha = 1, size = 7)
  )) +
  theme_void() +
  theme(
    legend.position = "top",
    legend.text = element_text(size = 12),
    legend.key = element_blank(),
    legend.margin = margin(t = 0, b = 0),
    legend.box.margin = margin(t = -20, b = 0),
    plot.margin = margin(-45, 0, 0, 0)
  )

community_composition_plot_combined <- community_composition_plot +
  theme(
    legend.position = "none",
    plot.title = element_text(
      hjust = 0.5,
      size = 14,
      face = "bold",
      margin = margin(b = -8)
    ),
    plot.margin = margin(-30, -35, -45, -35)
  )

diversity_plot_combined <- diversity_plot +
  theme(
    legend.position = "none",
    plot.title = element_text(
      hjust = 0.5,
      size = 14,
      face = "bold",
      margin = margin(b = -8)
    ),
    plot.margin = margin(-30, -35, -45, -35)
  )

composition_diversity_figure <-
  (
    community_composition_plot_combined +
      diversity_plot_combined +
      plot_layout(ncol = 2, widths = c(1, 1))
  ) /
  combined_driver_legend +
  plot_layout(heights = c(1, 0.08))

composition_diversity_figure


ggsave(
  filename = "composition_diversity_figure.pdf",
  plot = composition_diversity_figure,
  width = 8,
  height = 4.3,
  units = "in"
)

