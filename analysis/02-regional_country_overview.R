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

ggsave_bubble <- partial(
  ggplot2::ggsave,
  bg = "white",
  width = 9,
  height = 8
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


# Also load population data 
population_data <- pigoar2026::unwpp_data

# Add country naming conventions

country_names <- cliaretl::wb_country_list


# population data transformations ----------------------------------------

# Build region abbreviation lookup from ctf_static
region_lookup <- ctf_static |>
  select(country_code, region) |>
  distinct() |>
  mutate(
    region_code = case_when(
      region == "East Asia & Pacific"                                ~ "EAP",
      region == "Europe & Central Asia"                             ~ "ECA",
      region == "Latin America & Caribbean"                         ~ "LAC",
      region == "Middle East, North Africa, Afghanistan & Pakistan" ~ "MENAAP",
      region == "South Asia"                                        ~ "SAR",
      region == "Sub-Saharan Africa"                                ~ "SSA",
      TRUE ~ region
    )
  ) |>
  left_join(country_names |> select(country_code, group_code), by = "country_code")

# Match UN country names in population_data to WB ISO3 codes via countrycode,
# then attach WB region and income_group from ctf_static
population_country <- population_data |>
  dplyr::filter(type == "Country/Area") |>
  dplyr::mutate(
    country_code = suppressWarnings(
      countrycode::countrycode(group_class, origin = "country.name", destination = "wb")
    )
  ) |>
  dplyr::filter(!is.na(country_code)) |>
  dplyr::left_join(
    ctf_static |> dplyr::select(country_code, region, income_group) |> dplyr::distinct(),
    by = "country_code"
  )

# Region-level population growth: aggregate from country-level UN WPP data
avg_pop_region <- population_country |>
  dplyr::filter(!is.na(region), year >= 2027, year <= 2036) |>
  dplyr::mutate(
    region_code = case_when(
      region == "East Asia & Pacific"                                ~ "EAP",
      region == "Europe & Central Asia"                             ~ "ECA",
      region == "Latin America & Caribbean"                         ~ "LAC",
      region == "Middle East, North Africa, Afghanistan & Pakistan" ~ "MENAAP",
      region == "South Asia"                                        ~ "SAR",
      region == "Sub-Saharan Africa"                                ~ "SSA",
      TRUE ~ region
    )
  ) |>
  dplyr::group_by(region_code) |>
  dplyr::summarise(
    pop_growth_avg   = mean(population_growth_rate, na.rm = TRUE),
    total_population = sum(total_population, na.rm = TRUE),
    median_age       = mean(median_age, na.rm = TRUE),
    .groups = "drop"
  )

# Income-group-level population growth: use pre-computed UN WPP aggregates directly
avg_pop_income <- population_data |>
  dplyr::filter(type == "Income Group", year >= 2027, year <= 2036) |>
  dplyr::group_by(group_class) |>
  dplyr::summarise(
    pop_growth_avg   = mean(population_growth_rate, na.rm = TRUE),
    # total_population = mean(total_population, na.rm = TRUE),
    # median_age       = mean(median_age, na.rm = TRUE),
    .groups = "drop"
  )

# Join both to ctf_static for downstream use
regional_pop <- ctf_static |>
  select(country_code, region, income_group) |>
  distinct() |>
  left_join(region_lookup |> select(country_code, region_code, group_code),
            by = "country_code") |>
  left_join(avg_pop_region  |> select(region_code, pop_growth_region = pop_growth_avg),
            by = "region_code") |>
  left_join(avg_pop_income  |> select(group_class, pop_growth_income = pop_growth_avg),
            by = join_by(income_group == group_class))




# gov dimension averages -------------------------------------------------

ctf_avgs <- ctf_static |>
  select(1:5, ends_with("_avg")
  ) |>
  rename(
    `Integrity` = vars_anticorruption_avg,
    `Energy and Enviroment Institutions` = vars_climate_avg,
    `Justice Institutions` = vars_leg_avg,
    `Political Institutions` = vars_pol_avg,
    `Social Institutions` = vars_social_avg,
    `Information Systems` = vars_digital_avg,
    `Justice Institutions` = vars_leg_avg,
    `Transparency and Accountability` = vars_transp_avg,
    `Bussines Enviroment` = vars_mkt_avg,
    `Public Financial Management` = vars_pfm_avg,
    `Public Human Resources Management` = vars_hrm_avg
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
      "Integrity",
      "Transparency and Accountability",
      "Information Systems",
      "Public Human Resources Management"
    )
  )



regional_pop |> count(type)

regional_pop |>  count(income_group)
  
# Bubble plot by income -----------------------------------------------------

# Radar by income level
plot_income_facet <- ctf_avgs |>
  mutate(cluster_lab = str_wrap(cluster, 12)) |>
  group_by(income_group, cluster_lab) |>
  summarise(value = mean(value, na.rm = TRUE), .groups = "drop") |>
  mutate(value = value * 100) |> 
  drop_na(
    income_group
  ) 

merged_income <- avg_pop_income |>
  inner_join(plot_income_facet, 
             by = join_by(group_class == income_group))

cluster_order <- str_wrap(
  c(
    "Public Human Resources Management",
    "Information Systems",
    "Integrity",
    "Transparency and Accountability"
  ),
  width = 12
)

merged_income |>
  mutate(cluster_lab = factor(cluster_lab, levels = cluster_order)) |>
  mutate(
    group_class = factor(group_class, levels = c(
      "Low income",
      "Lower middle income",
      "Upper middle income",
      "High income"
    )),
    value = if_else(cluster_lab == "Institutional\nCapacity", value / 100, value)
  ) |>
  ggplot(aes(x = value, y = group_class, 
             size = pop_growth_avg, color = group_class)) +
  geom_segment(aes(x = 0, xend = value, 
                   y = group_class, yend = group_class),
               linewidth = 0.4, alpha = 0.5) +
  geom_point(alpha = 0.7) +
  facet_wrap(~cluster_lab, scales = "free_x") +
  scale_size_continuous(range = c(1,20)) +
  ggthemes::scale_color_solarized() +
  scale_x_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, by = 25),
    labels = scales::number_format(accuracy = 1),
    expand = c(0, 0)
  ) +
  labs(
    x        = "Benchmarking score (0-100)",
    y        = NULL,
    size     = "Average Annual population growth,\nUN projection (2027–2036)"
  ) +
  guides(
    color = "none",        # drop color legend since y axis already labels groups
    size  = guide_legend()
  ) +
  theme_minimal() +
  theme(panel.grid.minor = element_blank(),
        legend.position = "bottom")


ggsave_bubble(
  here(
    "analysis",
    "figs",
    "overview_ctf",
    "population_vs_income_level_panel.png"
  )
)


# Bubble plot by region ---------------------------------------------------

plot_region_facet <- ctf_avgs |>
  mutate(cluster_lab = str_wrap(cluster, 12)) |>
  group_by(region, cluster_lab) |>
  summarise(value = mean(value, na.rm = TRUE), .groups = "drop") |>
  mutate(value = value * 100)

merged_region <- avg_pop_region |>
  inner_join(plot_region_facet,
             by = join_by(region_code == region))

merged_region |>
  mutate(cluster_lab = factor(cluster_lab, levels = cluster_order)) |>
  ggplot(aes(x = value, y = region_code,
             size = pop_growth_avg, color = region_code)) +
  geom_segment(aes(x = 0, xend = value,
                   y = region_code, yend = region_code),
               linewidth = 0.4, alpha = 0.5) +
  geom_point(alpha = 0.7) +
  facet_wrap(~cluster_lab, scales = "free_x") +
  scale_size_continuous(range = c(1, 15)) +
  ggthemes::scale_color_solarized() +
  scale_x_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, by = 25),
    labels = scales::number_format(accuracy = 1),
    expand = c(0, 0)
  ) +
  labs(
    x        = "Benchmarking score (0-100)",
    y        = NULL,
    size     = "Average Annual population growth,\nUN projection (2027–2036)"
  ) +
  guides(
    color = "none",
    size  = guide_legend()
  ) +
  theme_minimal() +
  theme(panel.grid.minor = element_blank(),
        legend.position = "bottom")

ggsave_bubble(
  here(
    "analysis",
    "figs",
    "overview_ctf",
    "population_vs_region_panel.png"
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


