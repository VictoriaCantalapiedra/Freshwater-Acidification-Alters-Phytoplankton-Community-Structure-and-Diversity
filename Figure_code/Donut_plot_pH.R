# ============================================================
# Figure: Phytoplankton responses to reduced pH
# Cantalapiedra Mateo et al. (2026, submitted)
# Author:Victoria Cantalapiedra Mateo      Licence: MIT (see Figure_code/LICENSE_MIT)
#
# Description:
#   Donut plots of the number of positive (+), negative (-) and null (0)
#   responses of each algal taxon to a reduction in pH, for each measured
#   response variable. CCM = carbon-concentrating mechanism;
#   CA = carbonic anhydrase.
#
# Data:
#   The count data are embedded in this script, so it needs no input files.
#   Counts are observations per taxon and response, 
#   derived from the studies listed in
#   Data/Cantalapiedra_et_al_included_studies.csv, and match
#   Data/Cantalapiedra_et_al_Donut_plot_pH.csv (which also lists glycolysis and gvp gene responses, not plotted here)
#
# Output (written to the working directory):
#   measured_responses_decreased_pH.pdf / .tiff
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
library(tidyr)
library(ggnewscale)
library(patchwork)
library(tibble)

# ----------------------------
# Data
# ----------------------------
data <- tribble(
  ~Taxa, ~Parameter, ~Positive, ~Negative, ~Null,
  "Cyanobacteria", "Growth", 0, 8, 1,
  "Diatoms", "Growth", 0, 1, 0,
  "Green algae", "Growth", 1, 4, 5,
  "Cryptophytes", "Growth", 0, 0, 1,
  "Mixed algae", "Growth", 0, 0, 0,
  "Red algae", "Growth", 0, 1, 0,
  
  "Cyanobacteria", "Biomass", 1, 1, 0,
  "Diatoms", "Biomass", 1, 1, 0,
  "Green algae", "Biomass", 1, 3, 0,
  "Cryptophytes", "Biomass", 1, 0, 0,
  "Mixed algae", "Biomass", 0, 2, 1,
  "Red algae", "Biomass", 0, 0, 0, 
  
  "Cyanobacteria", "Photosynthesis", 0, 2, 0,
  "Diatoms", "Photosynthesis", 0, 1, 0,
  "Green algae", "Photosynthesis", 0, 1, 1,
  "Cryptophytes", "Photosynthesis", 0, 0, 0,
  "Mixed algae", "Photosynthesis", 0, 2, 0,
  "Red algae", "Photosynthesis", 0, 0, 0, 
  
  "Cyanobacteria", "Toxins", 3, 3, 1,
  "Diatoms", "Toxins", 0, 0, 0,
  "Green algae", "Toxins", 0, 0, 0,
  "Cryptophytes", "Toxins", 0, 0, 0,
  "Mixed algae", "Toxins", 0, 0, 0,
  "Red algae", "Toxins", 0, 0, 0, 
  
  "Cyanobacteria", "Antioxidant genes", 1, 0, 0,
  "Diatoms", "Antioxidant genes", 0, 0, 0,
  "Green algae", "Antioxidant genes", 1, 0, 0,
  "Cryptophytes", "Antioxidant genes", 0, 0, 0,
  "Mixed algae", "Antioxidant genes", 0, 0, 0,
  "Red algae", "Antioxidant genes", 0, 0, 0, 
  
  "Cyanobacteria", "CCM", 0, 1, 0,
  "Diatoms", "CCM", 0, 0, 0,
  "Green algae", "CCM", 0, 0, 0,
  "Cryptophytes", "CCM", 0, 0, 0,
  "Mixed algae", "CCM", 0, 0, 0,
  "Red algae", "CCM", 0, 0, 0, 
  
  "Cyanobacteria", "Bicarb transporter genes", 0, 1, 0,
  "Diatoms", "Bicarb transporter genes", 0, 0, 0,
  "Green algae", "Bicarb transporter genes", 0, 0, 0,
  "Cryptophytes", "Bicarb transporter genes", 0, 0, 0,
  "Mixed algae", "Bicarb transporter genes", 0, 0, 0,
  "Red algae", "Bicarb transporter genes", 0, 0, 0, 
  
  "Cyanobacteria", "CA", 0, 2, 0,
  "Diatoms", "CA", 0, 0, 0,
  "Green algae", "CA", 0, 2, 0,
  "Cryptophytes", "CA", 0, 0, 0,
  "Mixed algae", "CA", 0, 1, 0,
  "Red algae", "CA", 0, 0, 0
)

# ----------------------------
# Global settings
# ----------------------------
taxa_levels <- c(
  "Cyanobacteria", "Diatoms", "Green algae",
  "Cryptophytes", "Mixed algae", "Red algae"
)

response_levels <- c("Positive", "Negative", "Null")

taxa_colors <- c(
  "Cyanobacteria"   = "#4E79A7",
  "Diatoms"         = "#7B6FB6",
  "Green algae"     = "#59A14F",
  "Mixed algae"     = "#C47A6D",
  "Cryptophytes"    = "#C49A44",
  "Red algae"       = "#C44E52"
)

response_labels <- c(
  "Positive" = "+",
  "Negative" = "-",
  "Null" = "0"
)

response_fills <- c(
  "Positive" = "white",
  "Negative" = "black",
  "Null" = "grey70"
)

response_text_cols <- c(
  "Positive" = "black",
  "Negative" = "white",
  "Null" = "black"
)

title_map <- c(
  "Growth" = "Growth",
  "Biomass" = "Biomass",
  "Photosynthesis" = "Photosynthesis",
  "Toxins" = "Toxins",
  "Antioxidant genes" = "Antioxidant genes",
  "CCM" = "CCM",
  "Bicarb transporter genes" = "Bicarb transporter\ngenes",
  "CA" = "CA"
)

top_params <- c(
  "Growth", "Biomass", "Photosynthesis", "Toxins"
)

bottom_params <- c(
  "Antioxidant genes", "CCM","Bicarb transporter genes", "CA"
)

# ----------------------------
# Donut function
# ----------------------------
make_donut <- function(df, parameter_name) {
  plot_data <- df %>%
    filter(Parameter == parameter_name) %>%
    mutate(Taxa = factor(Taxa, levels = taxa_levels))
  
  long_data <- plot_data %>%
    pivot_longer(
      cols = c(Positive, Negative, Null),
      names_to = "Response",
      values_to = "Count"
    ) %>%
    mutate(Response = factor(Response, levels = response_levels)) %>%
    filter(Count > 0) %>%
    arrange(Response, Taxa) %>%
    group_by(Response) %>%
    mutate(
      ymax = cumsum(Count),
      ymin = ymax - Count
    ) %>%
    ungroup()
  
  response_totals <- long_data %>%
    group_by(Response) %>%
    summarise(total = sum(Count), .groups = "drop") %>%
    mutate(offset = lag(cumsum(total), default = 0))
  
  long_data <- long_data %>%
    left_join(response_totals, by = "Response") %>%
    mutate(
      ymin = ymin + offset,
      ymax = ymax + offset
    )
  
  inner_data <- response_totals %>%
    mutate(
      ymin = offset,
      ymax = offset + total,
      label = response_labels[as.character(Response)],
      fill = response_fills[as.character(Response)],
      text_col = response_text_cols[as.character(Response)]
    )
  
  outer_labels <- long_data %>%
    mutate(
      ymid = (ymin + ymax) / 2,
      label = Count
    )
  
  total_count <- sum(response_totals$total)
  single_response <- nrow(inner_data) == 1
  
  p <- ggplot() +
    geom_rect(
      data = long_data,
      aes(
        ymin = ymin,
        ymax = ymax,
        xmin = 1.05,
        xmax = 2.35,
        fill = Taxa
      ),
      colour = "white",
      linewidth = 0.5,
      show.legend = FALSE
    ) +
    scale_fill_manual(
      values = taxa_colors,
      limits = taxa_levels,
      drop = FALSE
    ) +
    ggnewscale::new_scale_fill() +
    geom_rect(
      data = inner_data,
      aes(
        ymin = ymin,
        ymax = ymax,
        xmin = 0,
        xmax = 1.05,
        fill = fill
      ),
      colour = NA, 
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
      size = 4.5
    ) +
    coord_polar(theta = "y", clip = "off") +
    xlim(0, 2.95) +
    scale_y_continuous(expand = expansion(mult = c(0, 0)))+
    theme_void() +
    theme(
      plot.title = element_text(
        hjust = 0.5, 
        size = 16, 
        face = "bold",
        margin = margin(b = -10)
      ),
      plot.margin = margin(t = -15, r = 5, b = 5, l = 5)
    ) +
    labs(title = title_map[parameter_name])
  
  if (parameter_name != "Antioxidant genes") {
    
    p <- p +
      geom_rect(
        data = inner_data,
        aes(
          ymin = ymin,
          ymax = ymax,
          xmin = 0,
          xmax = 1.05
        ),
        fill = NA,
        colour = "black",
        linewidth = 0.7,
        inherit.aes = FALSE
      )
    
  }
  
  if (parameter_name != "Antioxidant genes") {
    
    response_borders <- inner_data %>%
      transmute(
        ymin = ymin,
        ymax = ymax,
        xmin = 0,
        xmax = 2.35
      )
    
    p <- p +
      geom_rect(
        data = response_borders,
        aes(
          ymin = ymin,
          ymax = ymax,
          xmin = xmin,
          xmax = xmax
        ),
        fill = NA,
        colour = "black",
        linewidth = 0.8,
        inherit.aes = FALSE
      )
  }
  
  if (parameter_name == "Antioxidant genes") {
    
    p <- p +
      geom_segment(
        aes(
          x = 1.05,
          xend = 2.35,
          y = inner_data$ymax[1],
          yend = inner_data$ymax[1]
        ),
        colour = "black",
        linewidth = 0.8,
        inherit.aes = FALSE
      )
  }
  
  if (single_response & parameter_name != "Antioxidant genes") {
    
    p <- p +
      geom_rect(
        aes(
          xmin = 0,
          xmax = 2.35,
          ymin = 0,
          ymax = total_count
        ),
        fill = NA,
        colour = "black",
        linewidth = 0.8,
        inherit.aes = FALSE
      )
    
  }
  
  if (parameter_name == "Antioxidant genes") {
    
    p <- p +
      
      geom_rect(
        aes(
          xmin = 1.05,
          xmax = 2.35,
          ymin = 0,
          ymax = total_count
        ),
        fill = NA,
        colour = "black",
        linewidth = 0.8,
        inherit.aes = FALSE
      ) +
      
      geom_segment(
        aes(
          x = 1.05,
          xend = 2.35,
          y = inner_data$ymax[1],
          yend = inner_data$ymax[1]
        ),
        colour = "black",
        linewidth = 0.8,
        inherit.aes = FALSE
      )
  }
  p
}

# ----------------------------
# Build donut plots
# ----------------------------
top_row <- lapply(top_params, function(x) make_donut(data, x))
bottom_row <- lapply(bottom_params, function(x) make_donut(data, x))

# ----------------------------
# Legend
# ----------------------------
legend_df <- data.frame(
  Taxa = factor(taxa_levels, levels = taxa_levels),
  x = seq_along(taxa_levels),
  y = 1
)

legend_plot <- ggplot(legend_df, aes(x = x, y = y, colour = Taxa)) +
  geom_point(alpha = 0, size = 6) +
  scale_colour_manual(
    values = taxa_colors,
    limits = taxa_levels,
    drop = FALSE,
    name = "Taxa"
  ) +
  guides(
    colour = guide_legend(
      nrow = 2,
      byrow = TRUE,
      override.aes = list(alpha = 1, size = 7)
    )
  ) +
  theme_void() +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 16, face = "bold"),
    legend.text = element_text(size = 11),
    legend.key = element_blank(),
    plot.margin = margin(-140, 0, 0, 0),
    legend.spacing.y = unit(0, "mm"),
    legend.box.margin = margin(0, 0, 0, 0),
    legend.margin = margin(0, 0, 0, 0)
  )

# ----------------------------
# Assemble final figure
# ----------------------------
final_plot <-
  (
    wrap_plots(top_row, ncol = 4) /
      wrap_plots(bottom_row, ncol = 4) /
      legend_plot
  ) +
  plot_layout(heights = c(1,1, 0.12))

final_plot

# ----------------------------
# Export
# ----------------------------
ggsave(
  filename = "measured_responses_decreased_pH.pdf",
  plot = final_plot,
  width = 16,
  height = 10,
  units = "in"

)

ggsave(
  filename = "measured_responses_decreased_pH.tiff",
  plot = final_plot,
  width = 16,
  height = 10,
  units = "in",
  dpi = 600,
  compression = "lzw"
)
