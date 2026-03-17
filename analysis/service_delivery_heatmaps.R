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
library(RColorBrewer)


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
  height = 18
)


set.seed(101010)

# cliaretl 
# remotes::install_github("WB-PIDA-Data-Science-Shop/cliaretl")
library(cliaretl)


# load data --------------------------------------------------------------

# load metadata
metadata <-  cliaretl::db_variables_final

# load CTF scores
static_ctf <- cliaretl::closeness_to_frontier_static

# All Service Delivery indicators (raw) are here
compiled_indicators <-  fs::path_package(
    "extdata", "compiled_indicators.rds", package = "cliaretl"
  ) |> 
  read_rds() |> 
  dplyr::mutate(Year = as.double(year))


# process data -----------------------------------------------------------

# For this section, we will focus on the Service Delivery indicators. 
# We will first filter the compiled_indicators dataset to include only the Service Delivery indicators.
# Then we will generate raw SD indicators avergaged by country for the 2020-2024 , applying a simple average.
# Then we will contrast with the 5 ctf scores related to gov fuctions, which are: hrm, pfm, d&d, integrity and transparency.
# Finally, we will generate heatmaps to visualize the relationship between the SD indicators avg and the ctf scores.

# Compiled indicators - filter for Service Delivery indicators
compiled_long <- compiled_indicators |>
  dplyr::filter(dplyr::between(Year, 2020, 2024)) |>
  tidyr::pivot_longer(
    cols = -(1:5),
    names_to = "indicator",
    values_to = "value"
  )

# Lookup table for Service Delivery indicators
sd_indicators <- metadata |>
  dplyr::filter(family_var == "vars_service_delivery") |>
  dplyr::select(variable, var_name)

# Filter compiled_long and attach readable indicator names
sd_data_avg <- compiled_long |>
  dplyr::inner_join(sd_indicators, by = c("indicator" = "variable")) |>
  dplyr::group_by(country_name, indicator_name = var_name) |>
  dplyr::summarise(
    mean_value = mean(value, na.rm = TRUE)
  )

# Filter CTF scores for relevant indicators
ctf_scores <- static_ctf |>
  dplyr::select(
    (1:5),
    dplyr::all_of(c(
      "vars_anticorruption_avg",
      "vars_digital_avg",
      "vars_hrm_avg",
      "vars_transp_avg",
      "vars_pfm_avg"
    ))
  ) |>
  tidyr::pivot_longer(
    cols = -(1:5),
    names_to = "indicator",
    values_to = "score"
  )

# Merge SD data with CTF scores
# Each country gets all SD indicator averages paired with all 5 CTF scores
# Joined only on country_name to maximize observations
merged_data <- sd_data_avg |>
  dplyr::inner_join(
    ctf_scores |>
      dplyr::select(country_name, ctf_indicator = indicator, ctf_score = score),
    by = "country_name"
  ) |>
  dplyr::filter(
    is.finite(mean_value),   # drop NaN/Inf from SD side
    !is.na(ctf_score)        # drop NA from CTF side
  )


# R2 calculations ---------------------------------------------------------

# R² for each SD indicator ~ CTF indicator pair
r2_results <- merged_data |>
  dplyr::group_by(indicator_name, ctf_indicator) |>
  dplyr::summarise(
    n         = dplyr::n(),
    r2        = ifelse(
      n >= 5,
      cor(mean_value, ctf_score, use = "complete.obs")^2,
      NA_real_
    ),
    .groups = "drop"
  ) |>
  dplyr::filter(!is.na(r2)) |>
  dplyr::arrange(ctf_indicator, desc(r2)) |> 
  filter(n >= 10) # Keep only pairs with n ≥ 10 for reliability'


# heatmap -----------------------------------------------------------------

r2_results |>
  dplyr::mutate(
    ctf_label = dplyr::recode(
      ctf_indicator,
      "vars_anticorruption_avg" = "Integrity",
      "vars_digital_avg"        = "Digital",
      "vars_hrm_avg"            = "HRM",
      "vars_transp_avg"         = "Transparency",
      "vars_pfm_avg"            = "PFM"
    ),
    indicator_name = forcats::fct_reorder(indicator_name, r2, .fun = mean),
    label_face = ifelse(r2 >= 0.5, "bold", "plain")
  ) |>
  ggplot2::ggplot(ggplot2::aes(x = ctf_label, y = indicator_name, fill = r2)) +
  ggplot2::geom_tile(color = "white", linewidth = 0.5) +
  ggplot2::geom_text(
    ggplot2::aes(label = round(r2, 2), fontface = label_face),
    size = 3,
    color = "white"
  ) +
  ggplot2::scale_fill_viridis_c(
    direction = 1,
    limits = c(0, 1),
    name = expression(R^2)
  ) +
  ggplot2::labs(
    title = "R² between Service Delivery outcomes and CTF scores (2020 - 2024 averages)",
    subtitle = "Bivariate correlation; only pairs with n ≥ 10 included",
    x = "Dimensions of Institutional Capacity",
    y = NULL
  ) +
  ggplot2::theme(
    axis.text.y = ggplot2::element_text(size = 11),
    axis.text.x = ggplot2::element_text(size = 11, face = "bold"),
    legend.position = "right",
    panel.grid = ggplot2::element_blank()
  )

ggsave_db(
  here("analysis", "figs", "outcomes", "r2_heatmap_serviced_ctf_2.png")
)


# wide tables & correlation block -----------------------------------------

# Pivot CTF and SD to wide, one row per country
ctf_wide <- merged_data |>
  select(country_name, ctf_indicator, ctf_score) |>
  distinct() |>
  pivot_wider(names_from = ctf_indicator, values_from = ctf_score) |>
  rename(
    Integrity    = vars_anticorruption_avg,
    Digital      = vars_digital_avg,
    HRM          = vars_hrm_avg,
    Transparency = vars_transp_avg,
    PFM          = vars_pfm_avg
  )

sd_wide <- merged_data |>
  select(country_name, indicator_name, mean_value) |>
  distinct() |>
  pivot_wider(names_from = indicator_name, values_from = mean_value)

# Merge and compute correlation block: CTF (rows) x SD (cols)
cor_data <- inner_join(ctf_wide, sd_wide, by = "country_name") |>
  select(-country_name)

ctf_cols <- names(ctf_wide)[-1]   # "Integrity" "Digital" "HRM" "Transparency" "PFM"
sd_cols  <- names(sd_wide)[-1]

cor_block <- cor(
  cor_data[, ctf_cols],
  cor_data[, sd_cols],
  use = "pairwise.complete.obs"
)


# correlation heatmap (geom_tile) -----------------------------------------

cor_long <- cor_block |>
  as.data.frame() |>
  tibble::rownames_to_column("ctf_label") |>
  tidyr::pivot_longer(-ctf_label, names_to = "indicator_name", values_to = "r") |>
  mutate(
    indicator_name = forcats::fct_reorder(indicator_name, r, .fun = mean),
    label_face     = ifelse(abs(r) >= 0.8, "bold", "plain")
  )

cor_long |>
  ggplot2::ggplot(ggplot2::aes(x = ctf_label, y = indicator_name, fill = r)) +
  ggplot2::geom_tile(color = "white", linewidth = 0.5) +
  ggplot2::geom_text(
    ggplot2::aes(label = round(r, 2), fontface = label_face),
    size = 3,
    color = "white"
  ) +
  ggplot2::scale_fill_distiller(
    palette   = "RdYlGn",
    direction = 1,
    limits    = c(-1, 1),
    name      = "r"
  ) +
  ggplot2::labs(
    title    = "Correlation: CTF governance scores vs Service Delivery outcomes",
    subtitle = "Pairwise complete observations | Bold = |r| >= 0.8",
    x        = "CTF governance cluster",
    y        = NULL
  ) +
  ggplot2::theme(
    axis.text.y     = ggplot2::element_text(size = 9),
    axis.text.x     = ggplot2::element_text(size = 11, face = "bold"),
    legend.position = "right",
    panel.grid      = ggplot2::element_blank()
  )

ggsave_db(
  here("analysis", "figs", "outcomes", "corr_heatmap_ctf_sd_2.png")
)


# GGally pairs plot -------------------------------------------------------

library(GGally)

# Custom function to color correlation values by strength
cor_color_fn <- function(data, mapping, ...) {
  x     <- GGally::eval_data_col(data, mapping$x)
  y     <- GGally::eval_data_col(data, mapping$y)
  r     <- cor(x, y, use = "complete.obs")
  color <- dplyr::case_when(
    r >= 0.8  ~ "darkgreen",
    r <= -0.8 ~ "purple",
    TRUE      ~ "grey40"
  )
  GGally::ggally_cor(data, mapping, ...) +
    theme_void() +
    theme(panel.background = element_rect(
      fill = scales::alpha(color, 0.15), color = NA
    ))
}

# Build pairs plot on CTF + SD variables
ggpairs(
  cor_data,
  columns     = seq_along(cor_data),
  upper       = list(continuous = cor_color_fn),
  lower       = list(continuous = wrap("points", alpha = 0.2, size = 0.8)),
  diag        = list(continuous = wrap("densityDiag", fill = "steelblue", alpha = 0.5)),
  columnLabels = names(cor_data)
) +
  labs(
    title    = "Distribution and correlations: CTF scores vs Service Delivery outcomes",
    subtitle = "Green background = r >= 0.8 | Purple background = r <= -0.8"
  ) +
  theme_minimal(base_size = 9) +
  theme(
    strip.text = element_text(size = 7),
    plot.title = element_text(face = "bold", size = 12)
  )

ggsave_db(
  here("analysis", "figs", "outcomes", "ggpairs_ctf_sd_2.png"),
  width = 20, height = 18
)






