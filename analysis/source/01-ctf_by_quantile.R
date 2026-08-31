# set-up ------------------------------------------------------------------
library(haven)
library(dplyr)
library(here)
library(dplyr)
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
  theme_light() +
    theme(
      text = element_text(size = 22, family = "Segoe UI Semibold"),
      axis.text.x = element_text(size = 20, hjust = .5),
      axis.text.y = element_text(size = 18),
      plot.title = element_text(size = 22, face = "bold"),
      plot.subtitle = element_text(size = 16),
      plot.background = element_blank(),
      plot.caption = element_text(hjust = 0, size = 12),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      legend.position = "none",
      strip.text = element_text(size = 20, face = "bold"),
      legend.text = element_text(size = 18),
      legend.title = element_text(size = 16, face = "bold")
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
  # mutate(
  #   region = case_when(
  #     region == "East Asia & Pacific" ~ "EAP",
  #     region == "Europe & Central Asia" ~ "ECA",
  #     region == "Latin America & Caribbean" ~ "LAC",
  #     region == "Middle East, North Africa, Afghanistan & Pakistan" ~ "MENAAP",
  #     region == "South Asia" ~ "SAR",
  #     region == "Sub-Saharan Africa" ~ "SSA",
  #     TRUE ~ region
  #   )
  # ) |>
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
  ) |>
  # ensure that income levels reflect accurate ordering
  mutate(
    income_group = forcats::fct_relevel(
      income_group,
      c(
        "High income",
        "Upper middle income",
        "Lower middle income",
        "Low income"
      )
    )
  )

# hrm ---------------------------------------------------------------------
indicator_wide_scores |>
  filter(
    var_name %in%
      c(
        "Criteria for appointment decisions in the state administration"
      )
  ) |>
  plot_quantile(
    "income_group",
    "score",
    quantile_group = "var_name",
    ylab = "Benchmarked score"
  )

ggsave(
  here(
    "analysis",
    "figs",
    "indicators_ctf",
    "0_hrm_capacity_quantile.png"
  ),
  width = 12,
  height = 10,
  dpi = 300,
  bg = "white"
)

# digital -----------------------------------------------------------------
indicator_wide_scores |>
  filter(
    var_name %in%
      c(
        "Core government systems index (cgsi)"
      )
  ) |>
  plot_quantile(
    "income_group",
    "score",
    quantile_group = "indicator",
    ylab = "Benchmarked score"
  )

ggsave(
  here(
    "analysis",
    "figs",
    "indicators_ctf",
    "0_digital_capacity_quantile.png"
  ),
  width = 12,
  height = 10,
  dpi = 300,
  bg = "white"
)

# integrity ---------------------------------------------------------------
indicator_wide_scores |>
  filter(
    var_name %in%
      c(
        "Public sector corruption"
      )
  ) |>
  plot_quantile(
    "income_group",
    "score",
    quantile_group = "indicator",
    ylab = "Benchmarked score"
  ) 

ggsave(
  here(
    "analysis",
    "figs",
    "indicators_ctf",
    "0_integrity_capacity_quantile.png"
  ),
  width = 12,
  height = 10,
  dpi = 300,
  bg = "white"
)

# transparency ------------------------------------------------------------
indicator_wide_scores |>
  filter(
    var_name %in%
      c(
        "Publicized laws and government data",
        "Right to information",
        "Open budget index"
      )
  ) |>
  plot_quantile(
    "income_group",
    "score",
    quantile_group = "indicator",
    facet_group = "var_name",
    ylab = "Benchmarked score"
  )

ggsave(
  here(
    "analysis",
    "figs",
    "indicators_ctf",
    "0_transparency_capacity_quantile.png"
  ),
  width = 12,
  height = 14,
  dpi = 300,
  bg = "white"
)

# pfm ---------------------------------------------------------------------
indicator_wide_scores |>
  filter(
    var_name %in%
      c(
        "Pfm management information systems"
      )
  ) |>
  plot_quantile(
    "income_group",
    "score",
    quantile_group = "indicator",
    ylab = "Benchmarked score"
  )

ggsave(
  here(
    "analysis",
    "figs",
    "indicators_ctf",
    "0_pfm_capacity_quantile.png"
  ),
  width = 14,
  height = 10,
  dpi = 300,
  bg = "white"
)
