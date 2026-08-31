# set-up -----------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(purrr)

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

set.seed(101010)

# load data --------------------------------------------------------------
ctf_dynamic <- cliaretl::closeness_to_frontier_dynamic |>
  # only retain countries
  filter(country_group == 0) |> 
  mutate(
    year = as.integer(year),
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

compiled_indicators <- fs::path_package(
    "extdata", "compiled_indicators.rds", package = "cliaretl"
  ) |> 
  readr::read_rds() 

dictionary <- cliaretl::db_variables

wdi_outcomes <- cliaretl::wdi_indicators |> 
  transmute(
    country_code,
    year,
    log_gdp_per_capita = log(wdi_nygdppcapppkd),
    gdp_growth = wdi_nygdpmktpkdzg
  )

cliar_raw <- compiled_indicators |>
  left_join(
    wdi_outcomes,
    by = c("country_code", "year")
  )

cliar_ctf <- ctf_dynamic |> 
  left_join(
    wdi_outcomes,
    by = c("country_code", "year")
  ) |> 
  left_join(
    pigoar2026::budget_execution |> 
      mutate(year = as.integer(year)) |> 
      # budget execution as absolute distance from 100
      mutate(
        budget_execution_rate = abs(budget_execution_rate - 100)
      ),
    by = c("country_code", "year")
  )

# visualize --------------------------------------------------------------
cluster_names <- c(
  "hrm" = "vars_hrm_avg",
  "pub_sector_corruption" = "vdem_core_v2x_pubcorr",
  "open_budget" = "ibp_obs_obi",
  "budget_execution" = "budget_execution_rate"
)

prosperity_plots <- purrr::imap(cluster_names, \(var, label) {
  min_year <- cliar_ctf |> 
    select(
      all_of(
        c(var, "year")
      )
    ) |> 
    na.omit() |> 
    pull(year) |> 
    min()

  quantile_baseline <- cliar_ctf |>
    filter(year == min_year) |>
    classify_quantile(var, "year", threshold = "tercile") |>
    select(country_code, quantile_indicator)

  cliar_ctf |>
    left_join(quantile_baseline, by = "country_code") |>
    filter(
      !is.na(quantile_indicator) &
        # !is.na(income_group) &
        between(year, min_year, 2024)
    ) |>
    group_by(quantile_indicator, year) |>
    summarise(
      log_gdp_per_capita = mean(log_gdp_per_capita, na.rm = TRUE),
      .groups = "drop"
    ) |>
    group_by(quantile_indicator) |>
    mutate(
      log_gdp_per_capita = (log_gdp_per_capita)/
        log_gdp_per_capita[year == min_year] * 100
    ) |>
    ungroup() |>
    ggplot(aes(year, log_gdp_per_capita, color = quantile_indicator)) +
    geom_point(size = 2) +
    geom_line(linewidth = 1.5) +
    geom_hline(
      yintercept = 100,
      linetype = "dashed"
    ) +
    scale_color_manual(
      values = c(
        "Weak" = "red",
        "Emerging" = "goldenrod2",
        "Strong" = "forestgreen"
      ),
      name = "Global Level",
      na.value = "grey60"
    ) +
    scale_x_continuous(breaks = scales::breaks_width(2), labels = scales::label_number(big.mark = "")) +
    theme(legend.position = "bottom") +
    coord_cartesian(
      ylim = c(97.5, 102.5)
    ) +
    labs(x = "Year", y = "Logged GDP per capita (Baseline = 100)")
})

prosperity_plots |> 
  purrr::walk2(
    names(cluster_names),
     ~ ggplot2::ggsave(
      filename = file.path(
        "analysis/figs/prosperity",
        sprintf(
          "gdp_pc_vs_%s_trend.png",
          .y
        )
      ),
      plot = .x,
      width = 10, height = 10, dpi = 300, bg = "white"
    )
  )

# faceted by income group
prosperity_plots_by_income_group <- purrr::imap(cluster_names, \(var, label) {
  min_year <- cliar_ctf |> 
    select(
      all_of(
        c(var, "year")
      )
    ) |> 
    na.omit() |> 
    pull(year) |> 
    min()

  quantile_baseline <- cliar_ctf |>
    filter(year == min_year) |>
    classify_quantile(var, quantile_group = c("income_group", "year"), threshold = "tercile") |>
    select(country_code, quantile_indicator)

  cliar_ctf |>
    left_join(quantile_baseline, by = "country_code") |>
    filter(
      !is.na(quantile_indicator) &
        !is.na(income_group) &
        between(year, min_year, 2024)
    ) |>
    group_by(quantile_indicator, income_group, year) |>
    summarise(
      log_gdp_per_capita = mean(log_gdp_per_capita, na.rm = TRUE),
      .groups = "drop"
    ) |>
    group_by(quantile_indicator, income_group) |>
    mutate(
      log_gdp_per_capita = (log_gdp_per_capita)/
        log_gdp_per_capita[year == min_year] * 100
    ) |>
    ungroup() |>
    ggplot(aes(year, log_gdp_per_capita, color = quantile_indicator)) +
    geom_point(size = 2) +
    geom_line(linewidth = 1.5) +
    geom_hline(
      yintercept = 100,
      linetype = "dashed"
    ) +
    scale_color_manual(
      values = c(
        "Weak" = "red",
        "Emerging" = "goldenrod2",
        "Strong" = "forestgreen"
      ),
      name = "Global Level",
      na.value = "grey60"
    ) +
    scale_x_continuous(breaks = scales::breaks_width(2), labels = scales::label_number(big.mark = "")) +
    theme(legend.position = "bottom") +
    facet_wrap(
      vars(income_group),
      nrow = 2
    ) +
    coord_cartesian(
      ylim = c(95, 105)
    ) +
    labs(x = "Year", y = "Logged GDP per capita (Baseline = 100)")
})

prosperity_plots_by_income_group |> 
  purrr::walk2(
    names(cluster_names),
     ~ ggplot2::ggsave(
      filename = file.path(
        "analysis/figs/prosperity",
        sprintf(
          "gdp_pc_vs_%s_trend_by_income.png",
          .y
        )
      ),
      plot = .x,
      width = 10, height = 10, dpi = 300, bg = "white"
    )
  )

# excluding higher income countries
prosperity_plots_low_income <- purrr::imap(cluster_names, \(var, label) {
  min_year <- cliar_ctf |> 
    filter(income_group == "Low income") |>
    select(
      all_of(
        c(var, "year")
      )
    ) |> 
    na.omit() |> 
    pull(year) |> 
    min()

  quantile_baseline <- cliar_ctf |>
    filter(income_group == "Low income") |>
    filter(year == min_year) |>
    classify_quantile(var, quantile_group = c("year"), threshold = "tercile") |>
    select(country_code, quantile_indicator)

  cliar_ctf |>
    filter(income_group == "Low income") |>
    left_join(quantile_baseline, by = "country_code") |>
    filter(
      !is.na(quantile_indicator) &
        between(year, min_year, 2024)
    ) |>
    group_by(quantile_indicator, year) |>
    summarise(
      log_gdp_per_capita = mean(log_gdp_per_capita, na.rm = TRUE),
      .groups = "drop"
    ) |>
    group_by(quantile_indicator) |>
    mutate(
      log_gdp_per_capita = (log_gdp_per_capita)/
        log_gdp_per_capita[year == min_year] * 100
    ) |>
    ungroup() |>
    ggplot(aes(year, log_gdp_per_capita, color = quantile_indicator)) +
    geom_point(size = 2) +
    geom_line(linewidth = 1.5) +
    geom_hline(
      yintercept = 100,
      linetype = "dashed"
    ) +
    scale_color_manual(
      values = c(
        "Weak" = "red",
        "Emerging" = "goldenrod2",
        "Strong" = "forestgreen"
      ),
      name = "Global Level",
      na.value = "grey60"
    ) +
    scale_x_continuous(breaks = scales::breaks_width(2), labels = scales::label_number(big.mark = "")) +
    theme(legend.position = "bottom") +
    coord_cartesian(
      ylim = c(95, 105)
    ) +
    labs(x = "Year", y = "Logged GDP per capita (Baseline = 100)")
})

prosperity_plots_low_income |> 
  purrr::walk2(
    names(cluster_names),
     ~ ggplot2::ggsave(
      filename = file.path(
        "analysis/figs/prosperity",
        sprintf(
          "gdp_pc_vs_%s_trend_low_income.png",
          .y
        )
      ),
      plot = .x,
      width = 10, height = 10, dpi = 300, bg = "white"
    )
  )
