# set-up -----------------------------------------------------------------
library(cliaretl)
library(dplyr)
library(ggplot2)
library(purrr)

devtools::load_all()

# read-in data -----------------------------------------------------------
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
    gni_per_capita = mean(log(wdi_nygnppcapkd), na.rm = TRUE),
    poverty_gap_215 = mean(wdi_sipovlmicgp, na.rm = TRUE),
    gdp_growth = mean(wdi_nygdpmktpkdzg, na.rm = TRUE),
    unemployment_rate = mean(wdi_sluemtotlnezs, na.rm = TRUE)
  )

acled_regional_pop <- pigoar2026::acled_regional |>
    filter(
        between(year, 2020, 2024) &
            event_type %in% c("Protests", "Riots")
    ) |> 
    inner_join(
      pigoar2026::population,
      by = c("country_code", "year")
    )

cliar_correlation <- cliaretl::closeness_to_frontier_static |> 
  left_join(
    credit_rating_average,
    by = c("country_code")
  ) |> 
  left_join(
    wdi_outcomes,
    by = c("country_code")
  )

# analyze ----------------------------------------------------------------
institutional_clusters <- colnames(cliar_correlation) |> 
  stringr::str_subset(".*_avg$")

names(institutional_clusters) <- c(
  "Degree of Integrity",
  "Climate",
  "Digital Institutions",
  "Public HRM Institutions",
  "Justice",
  "Business Environment",
  "Public Financial Management",
  "Political",
  "Social",
  "Transparency"
)

institutional_clusters <- institutional_clusters |> 
  tibble::enframe(
    name = "x_lab", value = "x_val"
  )

outcomes <- c(
  "Credit Rating" = "credit_rating_mean",
  "Logged GNI per capita (Constant International Dollars)" = "gni_per_capita",
  "Poverty Gap ($2.15 a day)" = "poverty_gap_215",
  "Annual GDP Growth" = "gdp_growth",
  "Unemployment rate" = "unemployment_rate"
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

# generate plots
correlation_plots <- purrr::pmap(
  cartesian_product,
  function(x_val, y_val, x_lab, y_lab) {
    ggplot_correlation(
      data = cliar_correlation |> filter(!is.na(region)),
      x = x_val,
      y = y_val
    ) +
      labs(
        x = paste0(x_lab, " (2020-2024)"),
        y = y_lab,
        title = paste("Correlation between:", y_lab),
        subtitle = paste("and", x_lab)
      )
  }
)

# save plots
purrr::walk2(
  correlation_plots,
  seq_len(nrow(cartesian_product)),
  ~ ggplot2::ggsave(
    filename = file.path(
      "analysis/figs/outcomes",
      sprintf(
        "cor_%02d_%s_vs_%s.png", .y,
        gsub("\\s+", "_", cartesian_product$y_val[.y]),
        gsub("\\s+", "_", cartesian_product$x_val[.y])
      )
    ),
    plot = .x,
    width = 12, height = 12, dpi = 300, bg = "white"
  )
)