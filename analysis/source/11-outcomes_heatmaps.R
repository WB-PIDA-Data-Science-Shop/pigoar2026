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


ggsave_heatmap <- partial(
  ggplot2::ggsave,
  bg = "white",
  width = 12,
  height = 10
)


set.seed(101010)

# cliaretl 
# remotes::install_github("WB-PIDA-Data-Science-Shop/cliaretl")
library(cliaretl)


# load data --------------------------------------------------------------

metadata <-  cliaretl::db_variables_final

static_ctf <- cliaretl::closeness_to_frontier_static

credit_rating_average <- pigoar2026::credit_rating |> 
  filter(
    year %in% 2020:2024
  ) |> 
  group_by(country_code) |> 
  summarise(
    credit_rating_mean = mean(credit_rating, na.rm = TRUE)
  ) 

wdi_outcomes <- cliaretl::wdi_indicators |> 
  filter(
    year %in% 2020:2024
  ) |> 
  group_by(country_code) |> 
  summarise( 
    # gni_per_capita = mean(log(wdi_nygnppcapkd), na.rm = TRUE),
    poverty_gap_215 = mean(wdi_sipovlmicgp, na.rm = TRUE),
    gdp_pc= mean(wdi_nygdppcapppkd, na.rm = TRUE) # Repacing GNI
    # gdp_growth = mean(wdi_nygdpmktpkdzg, na.rm = TRUE),
    # unemployment_rate = mean(wdi_sluemtotlnezs, na.rm = TRUE)
  )

# process data -----------------------------------------------------------

labor_income_average <- pigoar2026::labor_income |> 
  filter(
    year %in% 2020:2024
  ) |> 
  group_by(country_code) |> 
  summarise(
    labor_income = mean(labor_income, na.rm = TRUE)
  )

cliar_correlation <- static_ctf |> 
  left_join(
    credit_rating_average,
    by = c("country_code")
  ) |> 
  left_join(
    wdi_outcomes,
    by = c("country_code")
  ) |> 
  left_join(
    labor_income_average,
    by = c("country_code")
  ) |> 
  mutate(
    income_group = forcats::fct_relevel(
            income_group,
            c(
                "Low income",
                "Lower middle income",
                "Upper middle income",
                "High income"
            )
        )
  )


# Rename institutional clusters for better labels in the heatmap
institutional_clusters <- c(
  "vars_hrm_avg",
  "vars_pfm_avg",
  "vars_digital_avg",
  "vars_anticorruption_avg",
  "vars_transp_avg"
)

names(institutional_clusters) <- c(
  "Public Human Resources Management",
  "Public Financial Management",
  "Information Systems",
  "Integrity",
  "Transparency and Accountability"
)

institutional_clusters <- institutional_clusters |> 
  tibble::enframe(
    name = "x_lab", value = "x_val"
  )

outcomes <- c(
  "Credit Rating" = "credit_rating_mean",
  "GDP per capita (current US$)" = "gdp_pc",
  "Poverty Gap ($2.15 a day)" = "poverty_gap_215",
  # "Annual GDP Growth" = "gdp_growth",
  # "Unemployment rate" = "unemployment_rate",
  "Labor income" = "labor_income"
) |> 
  tibble::enframe(
    name = "y_lab", value = "y_val"
  )

# generate all possible combinations between clusters and outcomes
cartesian_product <- tidyr::crossing(
  institutional_clusters |> select(x_val),
  outcomes |> select(y_val)
) |>
  left_join(institutional_clusters) |> 
  left_join(outcomes)


# correlation heatmap: institutional clusters x outcomes ------------------

# Helper: compute cor_results for a given subset of cliar_correlation
compute_cor_results <- function(data_subset, cartesian_product) {
  cartesian_product |>
    mutate(x_val = as.character(x_val), y_val = as.character(y_val)) |>
    rowwise() |>
    mutate(
      r = cor(
        data_subset[[x_val]],
        data_subset[[y_val]],
        use = "pairwise.complete.obs"
      ),
      n = sum(!is.na(data_subset[[x_val]]) & !is.na(data_subset[[y_val]]))
    ) |>
    ungroup() |>
    mutate(label_face = ifelse(abs(r) >= 0.5, "bold", "plain")) |>
    filter(!is.na(r)) |>
    mutate(
      y_lab = stringr::str_wrap(y_lab, width = 15)
    )
}

# Helper: build the heatmap ggplot
plot_cor_heatmap <- function(cor_results, title_suffix) {
  x_order <- c(
    "Public Human Resources Management",
    "Public Financial Management",
    "Information Systems",
    "Integrity",
    "Transparency and Accountability"
  )

  cor_results |>
    mutate(x_lab = factor(
      stringr::str_wrap(x_lab, width = 15),
      levels = stringr::str_wrap(x_order, width = 15)
    )) |>
    ggplot2::ggplot(ggplot2::aes(x = x_lab, y = y_lab, fill = r)) +
    ggplot2::geom_tile(color = "white", linewidth = 0.8) +
    ggplot2::geom_text(
      ggplot2::aes(label = round(r, 2), fontface = label_face),
      size  = 7,
      color = "white"
    ) +
    ggplot2::scale_fill_distiller(
      palette   = "RdYlGn",
      direction = 1,
      limits    = c(-1, 1),
      name      = "Correlation"
    ) +
    ggplot2::labs(
      title    = "Correlation: Institutional Capacity and Outcomes",
      subtitle = paste0(title_suffix, " | 2020-2024 country averages"),
      x        = NULL,
      y        = NULL
    ) +
    ggplot2::theme(
      axis.text.x     = ggplot2::element_text(size = 14, angle = 20, hjust = 1),
      axis.text.y     = ggplot2::element_text(size = 14),
      legend.position = "top",
      panel.grid      = ggplot2::element_blank()
    )
}

# --- All countries (overall) ---
cor_results_all <- compute_cor_results(cliar_correlation, cartesian_product)
plot_cor_heatmap(cor_results_all, "All countries")
ggsave_heatmap(here("analysis", "figs", "outcomes", "heatmap_clusters_outcomes_global_GDP.png"))

# --- One plot per income group ---
income_groups <- cliar_correlation |>
  dplyr::distinct(income_group) |>
  dplyr::filter(!is.na(income_group)) |>
  dplyr::pull(income_group) |>
  sort()

purrr::walk(income_groups, function(ig) {
  subset_data <- cliar_correlation |> dplyr::filter(income_group == ig)
  cor_res     <- compute_cor_results(subset_data, cartesian_product)
  p           <- plot_cor_heatmap(cor_res, as.character(ig))

  slug <- ig |>
    stringr::str_to_lower() |>
    stringr::str_replace_all("\\s+", "_")

  ggsave_heatmap(
    here("analysis", "figs", "outcomes", paste0("heatmap_clusters_outcomes_GDP", slug, ".png")),
    plot = p
  )
})






