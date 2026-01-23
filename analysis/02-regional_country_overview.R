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

ggsave_wide <- partial(
  ggplot2::ggsave,
  bg = "white",
  width = 15,
  height = 5
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

# Radar by region
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
    )
  )


# Radar by region

plot_df_facet <- ctf_avgs |>
  mutate(cluster_lab = str_wrap(cluster, 12)) |>
  group_by(region, cluster_lab) |>
  summarise(value = mean(value, na.rm = TRUE), .groups = "drop") |>
  mutate(value = value * 100)

plot_df_facet |>
  ggplot(aes
    (x = reorder(region, value), y = value)) +
  geom_col(aes(fill = region), alpha = 0.75, show.legend = TRUE) +
  geom_segment(
    aes(y = 0, yend = 100, xend = region, color = region),
    linetype = "dashed",
    show.legend = FALSE
  ) +
  coord_polar(
    theta = "x",
     direction = 1
  ) +
  geom_text(
    aes(y = value + 7, label = round(value, 1), color = region),
     size = 2.5, show.legend = FALSE
    ) +
  scale_y_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, by = 25),
    labels = scales::number_format(accuracy = 1),
    expand = c(0, 0)
  ) +
  scale_fill_brewer(palette = "Paired") +
  scale_color_brewer(palette = "Paired") +
  facet_wrap(~ cluster_lab, nrow = 1) +
  labs(
    # title = "Institutional Capacity Overview (2020-2024)",
    # subtitle = "Regional Average CTF Scores by Institutional Cluster",
    y = "CTF cluster score",
    x = NULL
  ) +
  theme_void() +
  theme(
    axis.text.x      = element_text(hjust = -10, size = 8),
    axis.text.y      = element_text(size = 8, color = "grey40"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey90"),
    plot.margin      = margin(10, 10, 10, 10),
    legend.position  = "top"
  )

ggsave_wide(
  here(
    "analysis",
    "figs",
    "overview_ctf",
    "overview-cluster-radar-by-region.png"
  )
)


# Radar by income level
plot_income_facet <- ctf_avgs |>
  mutate(cluster_lab = str_wrap(cluster, 12)) |>
  group_by(income_group, cluster_lab) |>
  summarise(value = mean(value, na.rm = TRUE), .groups = "drop") |>
  mutate(value = value * 100) |> 
  drop_na(
    income_group
  )

plot_income_facet |>
  ggplot(aes(x = reorder(income_group, value), y = value)) +
  geom_col(aes(fill = income_group), alpha = 0.75, show.legend = TRUE) +
  geom_segment(
    aes(y = 0, yend = 100, xend = income_group, color = income_group),
    linetype = "dashed",
    show.legend = FALSE
  ) +
  coord_polar(
    theta = "x",
     direction = 1
  ) +
  geom_text(
    aes(y = value + 7, label = round(value, 1), color = income_group),
     size = 2.5, show.legend = FALSE
    ) +
  scale_y_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, by = 25),
    labels = scales::number_format(accuracy = 1),
    expand = c(0, 0)
  ) +
  scale_fill_brewer(palette = "Set2") +
  scale_color_brewer(palette = "Set2") +
  facet_wrap(~ cluster_lab, nrow = 1) +
  labs(
    # title = "Institutional Capacity Overview (2020-2024)",
    # subtitle = "Regional Average CTF Scores by Institutional Cluster",
    y = "CTF cluster score",
    x = NULL
  ) +
  theme_void() +
  theme(
    axis.text.x      = element_text(hjust = -10, size = 8),
    axis.text.y      = element_text(size = 8, color = "grey40"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey90"),
    plot.margin      = margin(10, 10, 10, 10),
    legend.position  = "top"
  )

ggsave_wide(
  here(
    "analysis",
    "figs",
    "overview_ctf",
    "overview-cluster-radar-by-income.png"
  )
)


# country-dummbells-Appendix --------------------------------------------


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
  here("analysis", "figs", "overview_ctf","hrm-regional-dumbbells.png")
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
  here("analysis", "figs", "overview_ctf","digital-regional-dumbbells.png")
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
  here("analysis", "figs", "overview_ctf","integrity-regional-dumbbells.png")
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
  here("analysis", "figs", "overview_ctf","transp-regional-dumbbells.png")
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
  here("analysis", "figs", "overview_ctf","justice-regional-dumbbells.png")
)



### code-end


