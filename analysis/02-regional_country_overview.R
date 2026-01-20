# STATIC RADAR and DUMMBELLS

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
  filter(country_group == 0) |>
  filter(region != "North America") |>
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
  )



ctf_static <- cliaretl::closeness_to_frontier_static |>
  filter(country_group == 0) |>
  filter(region != "North America")




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
  filter(
    cluster %in% c(
      "Degree of Integrity",
      "Transparency Institutions",
      "Digital and Data Use",
      "Public Sector Employment"
      # "Public Financial Management"
    )
  )

plot_df <- ctf_avgs |>
  mutate(cluster_lab = str_wrap(cluster, 12)) |>
  group_by(region, cluster_lab) |>
  summarise(value = mean(value, na.rm = TRUE))

plot_df |>
  ggplot(aes(x = reorder(cluster_lab, value), y = value)) +
  geom_col(aes(fill = cluster_lab), alpha = 0.75, show.legend = FALSE) +
  geom_segment(
    aes(y = 0, yend = 1, xend = cluster_lab, color = cluster_lab),
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
    axis.text.y      = element_text(size = 7),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey90"),
    strip.text       = element_text(face = "bold", size = 12)   # <- facet titles
  )


ggsave_db(
  here("analysis", "figs", "dumbbells","overview-institutional-capacity-radar-by-region.png")
)


plot_df |>
  ggplot(aes(x = value, y = fct_reorder(cluster, value))) +
  geom_segment(aes(x = 0, xend = value, yend = cluster), linewidth = 0.6) +
  geom_point(size = 2.5, alpha = 0.9) +
  facet_wrap(~ region, scales = "free_y") +
  scale_x_continuous(
    limits = c(0, 1),
    breaks = seq(0, 1, 0.25),
    labels = scales::number_format(accuracy = 0.01)
  ) +
  labs(
    title = "Institutional Capacity overview by Region (2020–2024)",
    subtitle = "Regional Average CTF Scores by Institutional Cluster",
    x = "CTF cluster score", y = NULL
  ) +
  theme_minimal() +
  theme(panel.grid.minor = element_blank())



# country-dummbells-visualizations --------------------------------------------



# hrm ---------------------------------------------------------------------


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
  here("analysis", "figs", "dumbbells","hrm-regional-dumbbells.png")
)



# data --------------------------------------------------------------------

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
  here("analysis", "figs", "dumbbells","digital-regional-dumbbells.png")
)



# integrity ---------------------------------------------------------------

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
  here("analysis", "figs", "dumbbells","integrity-regional-dumbbells.png")
)


# transparency ------------------------------------------------------------

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
  here("analysis", "figs", "dumbbells","transp-regional-dumbbells.png")
)


# justice -----------------------------------------------------------------

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
  here("analysis", "figs", "dumbbells","justice-regional-dumbbells.png")
)



### code-end


