# DYNAMIC changes in regional indicators over time (2022-2024)

# set-up ------------------------------------------------------------------
library(haven)
library(dplyr)
library(stringi)
library(here)
library(tidyverse)
library(ggplot2)
library(janitor)
library(reshape2)
library(scales)
library(stringr)
library(readr)
library(readxl)
library(labelled)
library(cowplot)
library(openxlsx)
library(RColorBrewer)
library(countrycode)
library(extrafont)
library(devtools)
library(ggrepel)
library(forcats)
library(patchwork)

devtools::load_all()

theme_set(
  theme_minimal(base_family = "Segoe UI Semilight") +
    theme(
      axis.text.x = element_text(size = 10, hjust = .5),
      axis.text.y = element_text(size = 9),
      plot.title = element_text(size = 16, face = "bold"),
      plot.subtitle = element_text(size = 12),
      plot.background = element_blank(),
      plot.caption = element_text(hjust = 0, size = 10),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      legend.position = "top"
    )
)

## Absence of MENA and SA, in order to maintain colors
custom_colors <- c(
  "East Asia & Pacific" = "#a6cee3",
  "Europe & Central Asia" = "#1f78b4",
  "Latin America & Caribbean" = "#b2df8a",
  "Middle East & North Africa" = "#33a02c",
  "South Asia" = "#fb9a99",
  "Sub-Saharan Africa" = "#e31a1c"
)

ggsave_facet<- partial(
  ggplot2::ggsave,
  bg = "white",
  width = 20,
  height = 16
)

options(ggrepel.max.overlaps = Inf) ### For wrapping country codes


# data-load ---------------------------------------------------------------

# set-up ------------------------------------------------------------------
library(haven)
library(dplyr)
library(stringi)
library(here)
library(tidyverse)
library(ggplot2)
library(janitor)
library(reshape2)
library(scales)
library(stringr)
library(readr)
library(readxl)
library(labelled)
library(cowplot)
library(openxlsx)
library(RColorBrewer)
library(countrycode)
library(extrafont)
library(devtools)
library(ggrepel)
library(forcats)
library(patchwork)

devtools::load_all()

theme_set(
  theme_minimal(base_family = "Segoe UI Semilight") +
    theme(
      axis.text.x = element_text(size = 10, hjust = .5),
      axis.text.y = element_text(size = 9),
      plot.title = element_text(size = 16, face = "bold"),
      plot.subtitle = element_text(size = 12),
      plot.background = element_blank(),
      plot.caption = element_text(hjust = 0, size = 10),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      legend.position = "top"
    )
)

## Absence of MENA and SA, in order to maintain colors
custom_colors <- c(
  "East Asia & Pacific" = "#a6cee3",
  "Europe & Central Asia" = "#1f78b4",
  "Latin America & Caribbean" = "#b2df8a",
  "Middle East & North Africa" = "#33a02c",
  "South Asia" = "#fb9a99",
  "Sub-Saharan Africa" = "#e31a1c"
)

ggsave_facet<- partial(
  ggplot2::ggsave,
  bg = "white",
  width = 20,
  height = 16
)

options(ggrepel.max.overlaps = Inf) ### For wrapping country codes



# data-load ---------------------------------------------------------------
ctf_static <- cliaretl::closeness_to_frontier_static |>
  filter(country_group == 0) |>
  filter(region != "North America")  # Exclude North America

ctf_dyn <- cliaretl::closeness_to_frontier_dynamic |>
  filter(country_group == 0) |>
  filter(region != "North America")  # Exclude North America

db_variables <- cliaretl::db_variables



# 1. data preparation -----------------------------------------------------

# Objective: obtain cluster avgs between 2018 and 2022 by region

dynamic_ctf_clusters <- ctf_dyn |>
  select(1:5, ends_with("_avg")
  ) |>
  rename(
    `Public HRM Institutions` = vars_hrm_avg,
    `Degree of Integrity` = vars_anticorruption_avg,
    `Energy and Enviroment Institutions` = vars_climate_avg,
    `Justice Institutions` = vars_leg_avg, ### Not relevant
    `Political Institutions` = vars_pol_avg,
    `Social Institutions` = vars_social_avg,
    `Transparency Institutions` = vars_transp_avg
  ) |>
  pivot_longer(cols = 6:last_col(),
               names_to = "cluster",
               values_to = "value"
  )

# rename dataframe
year_subset <- dynamic_ctf_clusters |>
  filter(year %in% c(2016, 2018, 2020, 2022, 2024))

# 2. clusters-visualizations -------------------------------------------------












