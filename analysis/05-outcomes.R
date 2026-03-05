# set-up -----------------------------------------------------------------
library(cliaretl)
library(dplyr)
library(ggplot2)
library(purrr)

devtools::load_all()

theme_set(
    ggthemes::theme_few(base_size = 24)
)

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
    unemployment_rate = mean(wdi_sluemtotlnezs, na.rm = TRUE),
    literacy_rate = mean(wdi_seadtlitrzs, na.rm = TRUE),
    mortality_rate = mean(wdi_shdynmort, na.rm = TRUE)
  )

labor_income_average <- pigoar2026::labor_income |> 
  filter(
    year %in% 2020:2024
  ) |> 
  group_by(country_code) |> 
  summarise(
    labor_income = mean(labor_income, na.rm = TRUE)
  )

cliar_correlation <- cliaretl::closeness_to_frontier_static |> 
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
              "High income",
              "Upper middle income",
              "Lower middle income",
              "Low income"
            )
        )
  )

# analyze ----------------------------------------------------------------
institutional_clusters <- c(
  "vars_hrm_avg",
  "vars_pfm_avg",
  "vars_digital_avg",
  "vars_anticorruption_avg",
  "vars_transp_avg"
)

names(institutional_clusters) <- c(
  "Public Human Resource Management",
  "Public Financial Management",
  "Digital and Data",
  "Integrity",
  "Transparency and Accountability"
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
  "Unemployment rate" = "unemployment_rate",
  "Labor income" = "labor_income",
  "Literacy rate (Adult)" = "literacy_rate",
  "Infant mortality rate (logged)" = "mortality_rate"
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
    plot <- ggplot_correlation(
      data = cliar_correlation |> filter(!is.na(income_group)),
      x = x_val,
      y = y_val,
      group = "income_group"
    ) +
      scale_y_continuous(
        labels = function(x) stringr::str_wrap(x, width = 15)
      ) +
      labs(
        x = paste0(x_lab, " (2020-2024)"),
        y = y_lab
      ) +
      guides(
        color = guide_legend(
          "Income Group",
          nrow = 2
        )
      )
    
    if(y_val == "poverty_gap_215" | y_val == "mortality_rate"){
      plot <- plot +
        scale_y_log10()
    }

    plot
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
        "cor_%s_vs_%s.png",
        gsub("\\s+", "_", cartesian_product$y_val[.y]),
        gsub("\\s+", "_", cartesian_product$x_val[.y])
      )
    ),
    plot = .x,
    width = 10, height = 10, dpi = 300, bg = "white"
  )
)
