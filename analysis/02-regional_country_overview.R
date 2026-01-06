# STATIC DUMMBELLS AND

# Static CTF scores: identify which countries have changed the most in their score.


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

# cliaretl instalation:

# install.packages("pak")
# install.packages("remotes")
# install.packages('pointblank')
# remotes::install_github("WB-PIDA-Data-Science-Shop/cliaretl")


# data-load ---------------------------------------------------------------

ctf_static_wide <- cliaretl::closeness_to_frontier_static |>
                      filter(country_group == 0)

ctf_static <- cliaretl::closeness_to_frontier_static |>
  filter(country_group == 0)



# radar overview ----------------------------------------------------------

ctf_avgs <- ctf_static |>
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

plot_df <- ctf_avgs |>
  mutate(cluster_lab = str_wrap(cluster, 12))

plot_df |>
  ggplot(aes(x = reorder(cluster, value), y = value)) +
  geom_col(aes(fill = cluster), alpha = 0.75, show.legend = FALSE) +
  geom_segment(
    aes(y = 0, yend = 1, xend = cluster, color = cluster),
    linetype = "dashed",
    show.legend = FALSE
  ) +
  coord_polar(direction = 1) +
  scale_y_continuous(
    limits = c(0, 1),
    breaks = seq(0, 1, by = 0.25),
    labels = scales::number_format(accuracy = 0.01) # or percent_format()
  ) +
  facet_wrap(~ region) +
  labs(
    title = "Institutional Capacity overview by Region (2020-2024)",
    subtitle = "Regional Average CTF Scores by Institutional Cluster",
    y = "CTF cluster score",
    x = NULL
  ) +
  theme_minimal() +
  theme(
    # show radial scale labels:
    axis.text.y      = element_text(size = 7),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey90")
  )


ggsave_db(
  here("figures","institutional-capacity-radar-by-region_2025.png")
)




# regional-dummbells-visualizations --------------------------------------------


# FIGURE 5. POLITICAL
pol_data <- ctf_static_wide |>
  compute_regional_statistics("vars_pol_avg")

# Plot
pol_data |>
  generate_capacity_levels_plot("Political Institutions") +
  ggtitle(
    "Political Institutions",
    subtitle = "Regional Distributions and Average Trend"
  ) +
  labs(
    x = "Region",
    y = "CTF Average Score",
    shape = "Values",
    color = "Region",
    hjust = 0
  ) +
  coord_cartesian(ylim = c(0, 1)) +
  scale_color_brewer(palette = "Paired")

ggsave_db(
  here("figures","05-poli-arena-regional-dumbbells_color_2025.png")
)


# FIGURE 5. SOCIAL
social_data <- ctf_static_wide |>
  compute_regional_statistics("vars_social_avg")

# Plot
social_data |>
  generate_regional_minmax_plot("Social Institutions") +
  ggtitle(
    "Social Institutions",
    subtitle = "Regional Distributions and Average Trend"
  ) +
  labs(
    x = "Region",
    y = "CTF Average Score",
    shape = "Values",
    color = "Region",
    hjust = 0
  ) +
  coord_cartesian(ylim = c(0, 1)) +
  scale_color_brewer(palette = "Paired")

ggsave_db(
  here("figures","05-social-arena-regional-dumbbells_2025.png")
)



# FIGURE 6. HRM
hrm_data <- ctf_static_wide |>
  compute_regional_statistics("vars_hrm_avg")

# Plot
hrm_data |>
  generate_regional_minmax_plot("Personnel") +
  ggtitle(
    "Public HRM Institutions",
    subtitle = "Regional Distributions and Average Trend"
  ) +
  labs(
    x = "Region",
    y = "CTF Average Score",
    shape = "Values",
    color = "Region",
    hjust = 0
  ) +
  coord_cartesian(ylim = c(0, 1)) +
  scale_color_brewer(palette = "Paired")

ggsave_db(
  here("figures","06-center-of-gov-hrm-regional-dumbbells_2025.png")
)

# FIGURE 6. DIGITAL
digital_data <- ctf_static_wide |>
  compute_regional_statistics("vars_digital_avg")

# Plot
digital_data |>
  generate_regional_minmax_plot("Digital") +
  ggtitle(
    "Digital and Data Institutions",
    subtitle = "Regional Distributions and Average Trend"
  ) +
  labs(
    x = "Region",
    y = "CTF Average Score",
    shape = "Values",
    color = "Region",
    hjust = 0
  ) +
  coord_cartesian(ylim = c(0, 1)) +
  scale_color_brewer(palette = "Paired")

ggsave_db(
  here("figures","06-center-of-gov-digital-regional-dumbbells_2025.png")
)


# FIGURE 7. INTEGRITY
integrity_data <- ctf_static_wide |>
  compute_regional_statistics("vars_anticorruption_avg")

# Plot
integrity_data |>
  generate_regional_minmax_plot("Accountability") +
  ggtitle(
    "Degree of Integrity",
    subtitle = "Regional Distributions and Average Trend"
  ) +
  labs(
    x = "Region",
    y = "CTF Average Score",
    shape = "Values",
    color = "Region",
    hjust = 0
  ) +
  coord_cartesian(ylim = c(0, 1)) +
  scale_color_brewer(palette = "Paired")

ggsave_db(
  here("figures","07-integrity-regional-dumbbells_2025.png")
)


# FIGURE 7. TRANSPARENCY
transp_data <- ctf_static_wide |>
  compute_regional_statistics("vars_transp_avg")

# Plot
transp_data |>
  generate_regional_minmax_plot("Transparency") +
  ggtitle(
    "Transparency & Accountability Institutions",
    subtitle = "Regional Distributions and Average Trend"
  ) +
  labs(
    x = "Region",
    y = "CTF Average Score",
    shape = "Values",
    color = "Region",
    hjust = 0
  ) +
  coord_cartesian(ylim = c(0, 1)) +
  scale_color_brewer(palette = "Paired")

ggsave_db(
  here("figures","07-transp-regional-dumbbells_2025.png")
)


# FIGURE 8. JUSTICE
justice_data <- ctf_static_wide |>
  compute_regional_statistics("vars_leg_avg")

# Plot
justice_data |>
  generate_regional_minmax_plot("Justice") +
  ggtitle(
    "Justice Institutions",
    subtitle = "Regional Distributions and Average Trend"
  ) +
  labs(
    x = "Region",
    y = "CTF Average Score",
    shape = "Values",
    color = "Region",
    hjust = 0
  ) +
  coord_cartesian(ylim = c(0, 1)) +
  scale_color_brewer(palette = "Paired")

ggsave_db(
  here("figures","08-justice-regional-dumbbells_2025.png")
)



# FIGURE 8. ENVIROMENT
climate_data <- ctf_static_wide |>
  compute_regional_statistics("vars_climate_avg")

# Plot
climate_data |>
  generate_regional_minmax_plot("Climate") +
  ggtitle(
    "Energy and Enviroment Institutions",
    subtitle = "Regional Distributions and Average Trend"
  ) +
  labs(
    x = "Region",
    y = "CTF Average Score",
    shape = "Values",
    color = "Region",
    hjust = 0
  ) +
  coord_cartesian(ylim = c(0, 1)) +
  scale_color_brewer(palette = "Paired")

ggsave_db(
  here("figures","08-enviroment-regional-dumbbells_2025.png")
)


### code-end


