# ============================================================
# Figure: Changes in abundance of main phytoplankton groups under increased DIC and reduced pH
# Cantalapiedra Mateo et al. (2026, submitted)
# Author: Victoria Cantalapiedra Mateo      Licence: MIT (see Figure_code/LICENSE_MIT)
#
# Description:
#   Donut plots show increases (+), decreases (-) and no changes in abundance (0) under increased DIC (top row) and reduced pH (bottom row) conditions
#   Matrix table shows the number of filamentous and picoplanktonic forms that make up the positive and negative responses when mentioned.
#   Donuts include all forms.NA = no form-specific information was reported
#   in the studies (not a zero count).
#
# Data:
#   The count data are embedded in this script, so it needs no input files.
#   Counts are observations per driver, algal group and response, 
#   derived from the studies listed in
#   Data/Cantalapiedra_et_al_full_text_screening.csv, and match
#   Data/Cantalapiedra_et_al_Community_composition.csv (which also includes data for community composition and diversity counts for separate plots found in Figure_code/Donut_plot_Community_composition_Diversity.R)  
#
# Output (written to the working directory):
#  abundance_donuts_matrix.pdf
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
library(tidyr)
library(tibble)
library(ggnewscale)
library(patchwork)

# ----------------------------
# Data
# ----------------------------

abundance_data <- tribble(
  ~Treatment, ~Taxa, ~Form, ~Positive, ~Negative, ~Null,
  "DIC", "Cyanobacteria", "General", 5, 8, 2,
  "DIC", "Cyanobacteria", "Filamentous", 2, 1, 0,
  "DIC", "Cyanobacteria", "Picoplanktonic", 0, 0, 0,
  "DIC", "Green algae", "General", 10, 2, 1,
  "DIC", "Green algae", "Filamentous", 1, 0, 0,
  "DIC", "Diatoms", "General", 3, 1, 0,
  "DIC", "Dinoflagellates", "General", 1, 0, 0,
  "pH", "Cyanobacteria", "General", 4, 15, 0,
  "pH", "Cyanobacteria", "Filamentous", 1, 8, 0,
  "pH", "Cyanobacteria", "Picoplanktonic", 0, 3, 0,
  "pH", "Green algae", "General", 6, 4, 1,
  "pH", "Green algae", "Filamentous", 21, 0, 0,
  "pH", "Diatoms", "General", 1, 14, 0,
  "pH", "Dinoflagellates", "General", 13, 0, 0
) %>%
  pivot_longer(
    cols = c(Positive, Negative, Null),
    names_to = "Response",
    values_to = "Count"
  )

abundance_response_levels <- c("Positive", "Negative", "Null")
abundance_taxa_levels <- c("Cyanobacteria", "Green algae", "Diatoms", "Dinoflagellates")

abundance_taxa_colors <- c(
  "Cyanobacteria" = "#4E79A7",
  "Green algae" = "#59A14F",
  "Diatoms" = "#7B6FB6",
  "Dinoflagellates" = "#D9822B"
)

abundance_response_labels <- c("Positive" = "+", "Negative" = "-", "Null" = "0")
abundance_response_fills <- c("Positive" = "white", "Negative" = "black", "Null" = "grey70")
abundance_response_text_cols <- c("Positive" = "black", "Negative" = "white", "Null" = "black")

make_abundance_donut <- function(df, treatment_name, taxa_name) {
  long_data <- df %>%
    filter(Treatment == treatment_name, Taxa == taxa_name) %>%
    mutate(
      Response = factor(Response, levels = abundance_response_levels),
      Taxa = factor(Taxa, levels = abundance_taxa_levels)
    ) %>%
    group_by(Treatment, Taxa, Response) %>%
    summarise(Count = sum(Count), .groups = "drop") %>%
    filter(Count > 0) %>%
    arrange(Response) %>%
    mutate(ymax = cumsum(Count), ymin = ymax - Count)
  
  inner_data <- long_data %>%
    mutate(
      label = abundance_response_labels[as.character(Response)],
      fill = abundance_response_fills[as.character(Response)],
      text_col = abundance_response_text_cols[as.character(Response)]
    )
  
  outer_labels <- long_data %>%
    mutate(
      ymid = (ymin + ymax) / 2,
      label = as.character(Count)
    )
  
  single_response <- nrow(inner_data) == 1
  total_count <- sum(long_data$Count)
  
  p <- ggplot() +
    geom_rect(
      data = long_data,
      aes(ymin = ymin, ymax = ymax, xmin = 1.05, xmax = 2.35, fill = Taxa),
      color = if (single_response) NA else "white",
      linewidth = 0.7,
      show.legend = FALSE
    ) +
    scale_fill_manual(values = abundance_taxa_colors, limits = abundance_taxa_levels, drop = FALSE) +
    ggnewscale::new_scale_fill() +
    geom_rect(
      data = inner_data,
      aes(ymin = ymin, ymax = ymax, xmin = 0, xmax = 1.05, fill = fill),
      color = if (single_response) NA else "black",
      linewidth = 0.7,
      show.legend = FALSE
    ) +
    scale_fill_identity() +
    geom_text(data = outer_labels, aes(x = 2.68, y = ymid, label = label), size = 5) +
    geom_text(
      data = inner_data,
      aes(x = 0.52, y = (ymin + ymax) / 2, label = label),
      color = inner_data$text_col,
      size = 5,
      fontface = "bold"
    ) +
    coord_polar(theta = "y", clip = "off") +
    xlim(0, 2.95) +
    theme_void() +
    theme(
      plot.title = element_text(
        hjust = 0.5,
        size = 14,
        face = "bold",
        margin = margin(b = -8)
      ),
      plot.margin = margin(-8, 2, -10, 2)
    ) +
    labs(title = taxa_name)
  
  if (!single_response) {
    p <- p +
      geom_rect(
        data = inner_data,
        aes(ymin = ymin, ymax = ymax, xmin = 0, xmax = 2.35),
        fill = NA,
        color = "grey20",
        linewidth = 0.8,
        inherit.aes = FALSE
      )
  } else {
    p <- p +
      geom_segment(
        aes(x = 2.35, xend = 2.35, y = 0, yend = total_count),
        inherit.aes = FALSE,
        color = "grey20",
        linewidth = 0.8
      )
  }
  
  p
}

make_abundance_heading <- function(label) {
  ggplot() +
    annotate("text", x = 0.5, y = 0.25, label = label, size = 5, fontface = "bold") +
    xlim(0, 1) +
    ylim(0, 1) +
    theme_void()
}

abundance_group_legend <- ggplot() +
  annotate("text", x = 0.95, y = 0.5, label = "Groups",
           hjust = 0, size = 4, fontface = "bold") +
  annotate("point", x = 1.95, y = 0.5, colour = abundance_taxa_colors["Cyanobacteria"], size = 6) +
  annotate("text", x = 2.15, y = 0.5, label = "Cyanobacteria", hjust = 0, size = 4) +
  annotate("point", x = 3.25, y = 0.5, colour = abundance_taxa_colors["Green algae"], size = 6) +
  annotate("text", x = 3.45, y = 0.5, label = "Green algae", hjust = 0, size = 4) +
  annotate("point", x = 4.35, y = 0.5, colour = abundance_taxa_colors["Diatoms"], size = 6) +
  annotate("text", x = 4.55, y = 0.5, label = "Diatoms", hjust = 0, size = 4) +
  annotate("point", x = 5.20, y = 0.5, colour = abundance_taxa_colors["Dinoflagellates"], size = 6) +
  annotate("text", x = 5.40, y = 0.5, label = "Dinoflagellates", hjust = 0, size = 4) +
  xlim(0.6, 6.9) +
  ylim(0, 1) +
  theme_void() +
  theme(plot.margin = margin(-20, 0, -30, 0))

abundance_form_matrix_data <- abundance_data %>%
  filter(
    Form %in% c("Filamentous", "Picoplanktonic"),
    Response %in% c("Positive", "Negative")
  ) %>%
  mutate(
    Taxa = factor(Taxa, levels = abundance_taxa_levels),
    Form = factor(Form, levels = c("Filamentous", "Picoplanktonic"))
  ) %>%
  group_by(Treatment, Taxa, Form, Response) %>%
  summarise(Count = sum(Count), .groups = "drop") %>%
  complete(
    Treatment,
    Taxa,
    Form,
    Response = c("Positive", "Negative"),
    fill = list(Count = 0)
  ) %>%
  group_by(Treatment, Taxa, Form) %>%
  mutate(has_form_records = any(Count > 0)) %>%
  ungroup() %>%
  group_by(Treatment, Taxa) %>%
  filter(any(has_form_records)) %>%
  ungroup() %>%
  mutate(
    treatment_label = recode(Treatment, DIC = "Increased DIC", pH = "Reduced pH"),
    row_label = paste(treatment_label, Taxa),
    row_order = match(Treatment, c("DIC", "pH")) * 10 + as.integer(Taxa),
    row_label = factor(row_label, levels = unique(row_label[order(row_order)])),
    Form = factor(Form, levels = c("Filamentous", "Picoplanktonic")),
    Response = factor(Response, levels = c("Positive", "Negative")),
    response_label = factor(
      abundance_response_labels[as.character(Response)],
      levels = c("+", "-")
    ),
    column_label = interaction(Form, response_label, sep = "\n", lex.order = TRUE),
    column_label = factor(
      column_label,
      levels = c("Filamentous\n+", "Filamentous\n-", "Picoplanktonic\n+", "Picoplanktonic\n-")
    ),
    column_id = as.integer(column_label),
    label = case_when(
      !has_form_records ~ "NA",
      Count == 0 ~ "NA",
      TRUE ~ as.character(Count)
    )
  )

abundance_form_matrix <- ggplot(
  abundance_form_matrix_data,
  aes(x = column_id, y = row_label)
) +
  geom_tile(fill = "grey96", color = "white", linewidth = 1.1, width = 0.96, height = 0.86) +
  geom_point(
    aes(x = 0.35, y = row_label, colour = Taxa),
    size = 4.2,
    inherit.aes = FALSE
  ) +
  geom_text(aes(label = label), size = 3.4) +
  annotate(
    "text",
    x = 0.02,
    y = length(levels(abundance_form_matrix_data$row_label)) + 0.85,
    label = "Form-specific counts",
    hjust = 0,
    size = 4,
    fontface = "bold"
  ) +
  scale_colour_manual(values = abundance_taxa_colors, guide = "none") +
  scale_x_continuous(
    breaks = 1:4,
    labels = c("Filamentous\n+", "Filamentous\n-", "Picoplanktonic\n+", "Picoplanktonic\n-"),
    position = "top",
    expand = expansion(mult = c(0.02, 0.04))
  ) +
  scale_y_discrete(limits = rev(levels(abundance_form_matrix_data$row_label))) +
  coord_cartesian(xlim = c(0.15, 4.55), clip = "off") +
  labs(x = NULL, y = NULL) +
  theme_void() +
  theme(
    axis.text.x = element_text(size = 9.5, face = "bold", lineheight = 0.9, margin = margin(b = 4)),
    axis.text.y = element_text(size = 9.5, hjust = 1, margin = margin(r = 8)),
    plot.margin = margin(10, 70, 4, 110)
  )

abundance_plot_order <- c("Cyanobacteria", "Green algae", "Diatoms", "Dinoflagellates")

abundance_dic_row <- lapply(abundance_plot_order, function(x) {
  make_abundance_donut(abundance_data, "DIC", x)
})

abundance_ph_row <- lapply(abundance_plot_order, function(x) {
  make_abundance_donut(abundance_data, "pH", x)
})

abundance_final_plot <-
  make_abundance_heading("Increased DIC") /
  wrap_plots(abundance_dic_row, ncol = 4) /
  make_abundance_heading("Reduced pH") /
  wrap_plots(abundance_ph_row, ncol = 4) /
  abundance_group_legend /
  abundance_form_matrix +
  plot_layout(heights = c(0.18, 1.12, 0.18, 1.12, 0.22, 0.58))

abundance_final_plot

ggsave(
  filename = "abundance_donuts_matrix.pdf",
  plot = abundance_final_plot,
  width = 16,
  height = 10.4,
  units = "in"
)
