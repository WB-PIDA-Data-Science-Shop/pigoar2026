# UNPACKING INSTITUTIONAL CAPACITY FAMILIES: INDICATORS SLIDING SCALES PLOT
# Objective: Create sliding scale plots for indicators within selected institutional capacity families,
# showing country codes, income-group means, and overall means.

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
      text = element_text(size = 16, family = "Segoe UI Semibold"),
      axis.text.x = element_text(size = 18, hjust = .5),
      axis.text.y = element_text(size = 18),
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
  # only retain countries
  filter(country_group == 0)

dictionary <- cliaretl::db_variables


# prepare data ------------------------------------------------------------

#Pivot ctf static
ctf_static_long <- ctf_static |>
  pivot_longer(
    cols = (6:last_col()),
    names_to = "indicator",
    values_to = "score"
  )

# Add families to indicators
ctf_static_fam <- ctf_static_long |>
  left_join(
    dictionary |>
      select(
        variable,
        var_name,
        family_var,
        family_name,
        benchmark_static_family_aggregate_download
      ),
    by = c("indicator" = "variable")
  ) |> # Filter only benchmarked indicators with families
  filter(!is.na(family_var)) |>
  filter(benchmark_static_family_aggregate_download == "Yes") |>
  select(
    country_code:country_group,
    var_name,
    family_var,
    family_name,
    benchmark_static_family_aggregate_download,
    indicator,
    score
  )

# Drop sectors
center_gov <-
  ctf_static_fam |>
  filter(
    !family_name %in%
      c(
        "Business Environment", # Drop sector-specific families
        "Energy and Environment Institutions"
      )
  ) |>
  select(-benchmark_static_family_aggregate_download)

# Rename regions and create cliar areas
indicator_wide_scores <- center_gov |>
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
      family_name %in%
        c("Social Institutions", "Political Institutions") ~ "pol arena",
      TRUE ~ "center of gov"
    )
  ) |>
  drop_na(score, income_group) |>
  select(
    country_code,
    income_group,
    region,
    family_name,
    var_name,
    indicator,
    score,
    cliar_area
  )

# hrm ---------------------------------------------------------------------

hrm_data <- prep_benchmark_data(
  data = indicator_wide_scores,
  family_name_value = "Public Human Resource Management Institutions",
  group_var = income_group
)

# Create hrm plot
plot_benchmark(
  data = hrm_data,
  title = "Benchmarking Public Sector Employment Institutions: CTF scores (2020–2024 average)"
)

ggsave_long(here(
  "analysis",
  "figs",
  "indicators_ctf",
  "0_hrm_capacity_final_order.png"
))

# pruned
indicator_wide_scores |> 
  prep_benchmark_data(
    family_name_value = "Public Human Resource Management Institutions",
    select_var_name = c(
      "Criteria for appointment decisions in the state administration",
      "Rigorous and impartial public administration"
    ),
    group_var = income_group
  ) |> 
  plot_benchmark()

ggsave_db(
  here(
    "analysis",
    "figs",
    "indicators_ctf",
    "0_hrm_capacity_pruned.png"
  )
)

# digital -----------------------------------------------------------------

digital_data <- prep_benchmark_data(
  data = indicator_wide_scores,
  family_name_value = "Digital and Data Institutions",
  group_var = income_group
)

# Create digital plot
plot_benchmark(
  data = digital_data,
  title = "Benchmarking Digital and Data Institutions: CTF scores (2020–2024 average)"
)

ggsave_long(here(
  "analysis",
  "figs",
  "indicators_ctf",
  "0_digital_capacity_final_order.png"
))

# pruned
indicator_wide_scores |> 
  prep_benchmark_data(
    family_name_value = "Digital and Data Institutions",
    select_var_name = c(
      "Core government systems index (cgsi)",
      "Public service delivery index (psdi)",
      "Censuses and surveys"
    ),
    group_var = income_group
  ) |> 
  plot_benchmark()

ggsave(
  here(
    "analysis",
    "figs",
    "indicators_ctf",
    "0_digital_capacity_pruned.png"
  ),
  width = 14,
  height = 14,
  bg = "white"
)

# integrity ---------------------------------------------------------------
integrity_data <- prep_benchmark_data(
  data = indicator_wide_scores,
  family_name_value = "Degree of Integrity",
  group_var = income_group
)

# Create integrity plot
plot_benchmark(
  data = integrity_data,
  title = "Benchmarking Integrity Institutions: CTF scores (2020–2024 average)"
)

ggsave_long(here(
  "analysis",
  "figs",
  "indicators_ctf",
  "0_integrity_institutions_final_order.png"
))

# pruned
indicator_wide_scores |> 
  prep_benchmark_data(
    family_name_value = "Degree of Integrity",
    select_var_name = c(
      "Executive corruption",
      "Legislative corruption",
      "Public sector corruption"
    ),
    group_var = income_group
  ) |> 
  plot_benchmark()

ggsave(
  here(
    "analysis",
    "figs",
    "indicators_ctf",
    "0_integrity_capacity_pruned.png"
  ),
  width = 14,
  height = 14,
  bg = "white"
)

# transparency ------------------------------------------------------------

transparency_data <- prep_benchmark_data(
  data = indicator_wide_scores,
  family_name_value = "Transparency and Accountability Institutions",
  group_var = income_group
)

# Create transparency plot
plot_benchmark(
  data = transparency_data,
  title = "Benchmarking Transparency and Accountability Institutions: CTF scores (2020–2024 average)"
)

ggsave_long(here(
  "analysis",
  "figs",
  "indicators_ctf",
  "0_transparency_institutions_final_order.png"
))

# pruned
indicator_wide_scores |> 
  prep_benchmark_data(
    family_name_value = "Transparency and Accountability Institutions",
    select_var_name = c(
      "Publicized laws and government data",
      "Digital citizen engagement index score",
      "Right to information",
      "Open budget index"
    ),
    group_var = income_group
  ) |> 
  plot_benchmark()

ggsave(
  here(
    "analysis",
    "figs",
    "indicators_ctf",
    "0_transparency_capacity_pruned.png"
  ),
  width = 14,
  height = 16,
  bg = "white"
)

# pfm ---------------------------------------------------------------------

pfm_data <- prep_benchmark_data(
  data = indicator_wide_scores,
  family_name_value = "Public Finance Institutions",
  group_var = income_group
)

# Create pfm plot
plot_benchmark(
  data = pfm_data,
  title = "Benchmarking Public Finance Institutions: CTF scores (2020–2024 average)"
)

ggsave_long(here(
  "analysis",
  "figs",
  "indicators_ctf",
  "0_pfm_institutions_finanl_order.png"
))

# pruned
indicator_wide_scores |> 
  prep_benchmark_data(
    family_name_value = "Public Finance Institutions",
    select_var_name = c(
      "In-year budget reports",
      "External audit",
      "Revenue administration",
      "Pfm management information systems"
    ),
    group_var = income_group
  ) |> 
  plot_benchmark()

ggsave(
  here(
    "analysis",
    "figs",
    "indicators_ctf",
    "0_pfm_capacity_pruned.png"
  ),
  width = 14,
  height = 16,
  bg = "white"
)
