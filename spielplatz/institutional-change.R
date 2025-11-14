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
library(purrr)

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

options(ggrepel.max.overlaps = Inf)

set.seed(101010)

# data-load ---------------------------------------------------------------

prev_ctf_static <- read_dta( # 2024
  here("data-raw", "input", "static_ctf_032525.dta")) |>
  filter(country_group == 0) |>
  filter(region != "North America")  # Exclude North America


ctf_static <- cliaretl::closeness_to_frontier_static |>
  filter(country_group == 0) |>
  filter(region != "North America")  # 2025

country_name <- cliaretl::wb_country_list


# data-prepare ------------------------------------------------------------

prev_avg <- prev_ctf_static |>
  select(1:5, ends_with("_avg")
  ) |>
  rename(
    `Degree of Integrity` = vars_anticorruption_avg,
    `Energy and Enviroment Institutions` = vars_climate_avg,
    `Justice Institutions` = vars_leg_avg,
    `Political Institutions` = vars_pol_avg,
    `Social Institutions` = vars_social_avg,
    `Digital and Data Use` = vars_digital_avg,
    `Justice Institutions` = vars_leg_avg,
    `Transparency Institutions` = vars_transp_avg,
    `Bussines Enviroment` = vars_mkt_avg,
    `Public Financial Management` = vars_pfm_avg,
    `Public Sector Employment` = vars_hrm_avg
  ) |>
  pivot_longer(cols = 6:last_col(),
               names_to = "cluster",
               values_to = "value"
  )


curr_avg <- ctf_static |>
  select(1:5, ends_with("_avg")
  ) |>
  rename(
    `Degree of Integrity` = vars_anticorruption_avg,
    `Energy and Enviroment Institutions` = vars_climate_avg,
    `Justice Institutions` = vars_leg_avg,
    `Political Institutions` = vars_pol_avg,
    `Social Institutions` = vars_social_avg,
    `Digital and Data Use` = vars_digital_avg,
    `Justice Institutions` = vars_leg_avg,
    `Transparency Institutions` = vars_transp_avg,
    `Bussines Enviroment` = vars_mkt_avg,
    `Public Financial Management` = vars_pfm_avg,
    `Public Sector Employment` = vars_hrm_avg
  ) |>
  pivot_longer(cols = 6:last_col(),
               names_to = "cluster",
               values_to = "value"
  )

# merge datasets
merged_avg <- prev_avg |>
  select(country_code, region, cluster, value) |>
  rename(prev_value = value) |>
  left_join(
    curr_avg |>
      select(country_code, cluster, value) |>
      rename(curr_value = value),
    by = c("country_code", "cluster")
  )

merged_avg_plot <-
  merged_avg|>
  left_join(
    ctf_static |>
      select(country_name, country_code),
    by = c("country_code")
  ) |>
  select(country_name, country_code, region, cluster, everything()) |>
  pivot_longer(
    cols = 5:6,
    names_to = "ctf_time",
    values_to = "value"
  )


# 2. clusters-visualizations -------------------------------------------------

merged_avg_plot |>
  filter(cluster == "Degree of Integrity") |>
  ggplot(aes(x = country_name, y = value, color = as.factor(ctf_time))) +
  geom_segment(aes(xend = country_name, y = 0, yend = value), linewidth = 1) +
  geom_point(size = 2, alpha = 0.6) +
  geom_hline(yintercept = 0,
             linetype = "solid",
             linewidth = .5,
             alpha = 0.75,
             color =  "lightgrey") +
  labs(
    x = "Country by Region",
    y = "Change in CTF Score"
  ) +
  facet_wrap(~region, scales = "free_y", nrow = 2) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 18, hjust = .5),
    axis.text.y = element_text(size = 16),
    axis.title.y = element_text(size = 20, face = "bold"),
    axis.title.x = element_text(size = 20, face = "bold"),
    plot.background = element_blank(),
    plot.caption = element_text(hjust = 0, size = 10),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    # legend.position = "none",
    strip.text = element_text(size = 20)
  ) +
  # scale_color_brewer(palette = "Paired") +
  scale_y_continuous(limits = c(0, 0.5)) +
  coord_flip()


### Change can be better visualized if regional avgs are calculated

regional_avg <- merged_avg_plot |>
  group_by(region, cluster, ctf_time) |>
  summarise(
    reg_value = mean(value, na.rm = TRUE)
  ) |>
  ungroup()


regional_avg |>
  ggplot(aes(x = cluster, y = reg_value, color = as.factor(ctf_time))) +
  geom_segment(aes(xend = cluster, y = 0, yend = reg_value), linewidth = 1) +
  geom_point(size = 5, alpha = 0.6) +
  geom_hline(yintercept = 0,
             linetype = "solid",
             linewidth = .5,
             alpha = 0.75,
             color =  "lightgrey") +
  labs(
    x = "Country by Region",
    y = "Change in CTF Score"
  ) +
  facet_wrap(~region, scales = "free_y", nrow = 2) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 18, hjust = .5),
    axis.text.y = element_text(size = 16),
    axis.title.y = element_text(size = 20, face = "bold"),
    axis.title.x = element_text(size = 20, face = "bold"),
    plot.background = element_blank(),
    plot.caption = element_text(hjust = 0, size = 10),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    # legend.position = "none",
    strip.text = element_text(size = 20)
  ) +
  # scale_color_brewer(palette = "Paired") +
  scale_y_continuous(limits = c(0, 0.5)) +
  coord_flip()













