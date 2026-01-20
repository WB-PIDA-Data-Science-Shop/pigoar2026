# UNPACKING CLUSTERS: REGIONAL INDICATORS HEATMAP

# Static CTF scores: identify which countries have changed the most in their score.


# set-up ------------------------------------------------------------------
library(haven)
library(dplyr)
library(here)
library(tidyverse)
library(ggplot2)
library(janitor)
library(scales)
library(stringr)
library(readr)
library(labelled)
library(cowplot)
library(RColorBrewer)
library(ggrepel)
library(forcats)
library(patchwork)
library(purrr)
library(cliaretl)

devtools::load_all()

theme_set(
  theme_minimal() +
    theme(
      text = element_text(family = "Segoe UI Semibold"),
      axis.text.x = element_text(size = 14, hjust = .5),
      axis.text.y = element_text(size = 14),
      plot.title = element_text(size = 22, face = "bold"),
      plot.subtitle = element_text(size = 16),
      plot.background = element_blank(),
      plot.caption = element_text(hjust = 0, size = 12),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      legend.position = "none"
    )
)

ggsave_db <- partial(
  ggplot2::ggsave,
  bg = "white",
  width = 14,
  height = 9
)

ggsave_long <- partial(
  ggplot2::ggsave,
  bg = "white",
  width = 10,
  height = 18
)

options(ggrepel.max.overlaps = Inf)

set.seed(101010)


# data-load ---------------------------------------------------------------

ctf_static <- cliaretl::closeness_to_frontier_static |>
  filter(country_group == 0)
# filter(region != "North America")  # 2025

dictionary <- cliaretl::db_variables

# data-prepare ------------------------------------------------------------

#Pivot ctf static
ctf_static_long <- ctf_static |>
  pivot_longer(
    cols = (6:last_col()),
    names_to = "indicator",
    values_to = "score"
  )

# Add families to inticators
ctf_static_fam <- ctf_static_long |>
  left_join(
    dictionary |>
      select(variable,
             var_name,
             family_var,
             family_name,
             benchmark_static_family_aggregate_download),
    by = c("indicator" = "variable")
  ) |> # Filter only benchmarked indicators with families
  filter(!is.na(family_var)) |>
  filter(benchmark_static_family_aggregate_download == "Yes") |>
  select(1:5,
         var_name,
         family_var,
         family_name,
         benchmark_static_family_aggregate_download,
         indicator,
         score)


# Drop sectors
center_gov <-
  ctf_static_fam |>
  filter(
    !family_name %in% c(
      "Business Environment", # Drop sector-specific families
      "Energy and Environment Institutions"
    )
  ) |>
  select(-benchmark_static_family_aggregate_download)



#compute the average by region and indicator to facet by cluster
region_indicator_avg <- center_gov |>
  group_by(region, family_name, indicator, var_name) |>
  summarise(
    avg_score = mean(score, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    region = case_when(
      region == "East Asia & Pacific" ~ "EAP",
      region == "Europe & Central Asia" ~ "ECA",
      region == "Latin America & Caribbean" ~ "LAC",
      region == "Middle East, North Africa, Afghanistan & Pakistan" ~ "MENAAP",
      region == "South Asia" ~ "SAR",
      region == "Sub-Saharan Africa" ~ "SSA",
      TRUE ~ region
    )
  ) |>
  mutate(
    cliar_area = case_when(
      family_name %in% c("Social Institutions", "Political Institutions") ~ "pol arena",
      TRUE                                                               ~ "center of gov"
    )
  ) |>
  mutate(
    var_name = str_wrap(var_name, width = 40),
    var_name = fct_reorder(var_name, avg_score),
    strength = case_when(
      avg_score < 0.25 ~ "Weak (0–0.24)",
      avg_score < 0.50 ~ "Emergent (0.25–0.49)",
      TRUE             ~ "Strong (0.50–1.00)"
    )
  )


# lolipop-data-visualize ----------------------------------------------------------


region_indicator_avg |>
  plot_regional_indicators("Justice Institutions")


# region_indicator_avg |>
#   plot_regional_indicators("Social Institutions")
#
#
# region_indicator_avg |>
#   plot_regional_indicators("Political Institutions")


region_indicator_avg |>
  plot_regional_indicators("Public Human Resource Management Institutions")

region_indicator_avg |>
  plot_regional_indicators("Degree of Integrity")


region_indicator_avg |>
  plot_regional_indicators("Transparency and Accountability Institutions")


region_indicator_avg |>
  plot_regional_indicators("Digital and Data Institutions")


# radar-viz ---------------------------------------------------------------

# Long format by cluster
ctf_avgs <- ctf_static |>
  # be a bit more explicit than 1:5 if you can, but this follows your structure
  select(1:5, ends_with("_avg")) |>
  rename(
    `Degree of Integrity`             = vars_anticorruption_avg,
    `Energy and Enviroment Institutions` = vars_climate_avg,
    `Justice Institutions`            = vars_leg_avg,
    `Political Institutions`          = vars_pol_avg,
    `Social Institutions`             = vars_social_avg,
    `Information Systems`            = vars_digital_avg,
    `Transparency Institutions`       = vars_transp_avg,
    `Bussines Enviroment`            = vars_mkt_avg,
    `Public Financial Management`     = vars_pfm_avg,
    `Public Sector Employment`        = vars_hrm_avg
  ) |>
  pivot_longer(
    cols      = 6:last_col(),
    names_to  = "cluster",
    values_to = "value"
  )

# Regional × cluster averages
ctf_avgs_center <- ctf_avgs |>
  filter(
    cluster %in% c(
      "Justice Institutions",
      "Degree of Integrity",
      "Political Institutions",
      "Social Institutions",
      "Transparency Institutions",
      "Information Systems",
      "Public Sector Employment",
      "Public Financial Management"
    )
  ) |>
  group_by(region, cluster) |>
  summarise(
    cluster_mean = mean(value, na.rm = TRUE),
    n_obs        = sum(!is.na(value)),
    .groups      = "drop"
  )



# 1. Regional × cluster averages
ctf_avgs_center <- ctf_avgs |>
  filter(
    cluster %in% c(
      "Degree of Integrity",
      "Transparency Institutions",
      "Information Systems",
      "Public Sector Employment",
      "Public Financial Management"
    )
  ) |>
  group_by(region, cluster) |>
  summarise(
    cluster_mean = mean(value, na.rm = TRUE),
    n_obs        = sum(!is.na(value)),
    .groups      = "drop"
  )

bands <- tibble(
  ymin = c(0.00, 0.25, 0.50),
  ymax = c(0.24, 0.49, 1.00),
  band = factor(
    c(
      "Weak (0–0.24)",
      "Emergent (0.25–0.49)",
      "Strong (0.50–1.00)"
    ),
    levels = c(
      "Weak (0–0.24)",
      "Emergent (0.25–0.49)",
      "Strong (0.50–1.00)"
    )
  )
)

# Plot with shaded polar bands and neutral bars
ctf_avgs_center |>
  mutate(
    strength = case_when(
      cluster_mean < 0.25           ~ "Weak (0–0.24)",
      cluster_mean < 0.50           ~ "Emergent (0.25–0.49)",
      TRUE                          ~ "Strong (0.50–1.00)"
    ),
    strength = factor(
      strength,
      levels = c(
        "Weak (0–0.24)",
        "Emergent (0.25–0.49)",
        "Strong (0.50–1.00)"
      )
    ),
    # optional: order clusters around the circle by their mean
    cluster = str_wrap(cluster, 10)
  ) |>
  ggplot(aes(x = cluster, y = cluster_mean)) +
  # background capacity bands (become rings in polar coordinates)
  geom_rect(
    data        = bands,
    aes(
      ymin  = ymin,
      ymax  = ymax,
      xmin  = -Inf,
      xmax  = Inf,
      fill  = band
    ),
    inherit.aes = FALSE,
    alpha       = 0.5
  ) +
  # neutral bars for the actual regional/cluster averages
  geom_col(aes(fill = strength), alpha = .75, width = 0.8, color = NA) +
  coord_polar(direction = 1) +
  scale_y_continuous(
    limits = c(0, 1),
    breaks = seq(0, 1, by = 0.25)
  ) +
  scale_fill_manual(
    name   = "Capacity level (CTF score)",
    values = c(
      "Weak (0–0.24)"        = "red3",
      "Emergent (0.25–0.49)" = "goldenrod",
      "Strong (0.50–1.00)"   = "darkgreen"
    ),
    limits = c(
      "Weak (0–0.24)",
      "Emergent (0.25–0.49)",
      "Strong (0.50–1.00)"
    ),
    drop = FALSE
  ) +
  labs(
    title    = "Institutional Capacity Overview by Region, 2020–2024",
    subtitle = "Core Government Functions ",
    y        = "Average CTF cluster score",
    x        = NULL
  ) +
  facet_wrap(~ region) +
  theme_minimal() +
  theme(
    axis.text.y        = element_text(size = 6),
    axis.text.x        = element_text(size = 8, hjust = 1),
    panel.grid.minor   = element_blank(),
    panel.grid.major.y = element_line(color = "grey90"),
    panel.grid.major.x = element_blank(),
    strip.text         = element_text(face = "italic"),
    legend.position    = "top"
  )


ggsave_db(
  here("figures","radar-by-region_strengt_color_alpha.png")
)


