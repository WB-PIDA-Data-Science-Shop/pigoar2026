# set-up -----------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(ggthemes)
library(forcats)
library(sf)
library(purrr)
library(lubridate)
library(rmapshaper)
library(geojsonio)
library(cliaretl)
library(here)
library(dotwhisker)

devtools::load_all()

theme_set(
    theme_few(base_size = 24)
)

# read-in data -----------------------------------------------------------
acled_events <- pigoar2026::acled |>
    inner_join(
        wdi_indicators,
        by = c("country_code", "year")
    ) |>
    filter(
        between(year, 2020, 2024)
    )

acled_demonstrations_regional <- pigoar2026::acled_regional |>
    filter(
        between(year, 2020, 2024) &
            event_type %in% c("Protests", "Riots")
    ) |>
    mutate(
        month = lubridate::floor_date(week, unit = "months")
    ) |>
    left_join(
        cliaretl::wdi_indicators,
        by = c("country_code", "year")
    ) |>
    left_join(
        pigoar2026::population,
        by = c("country_code", "year")
    ) |> 
    # fix income group
    mutate(
        income_group = factor(
            income_group,
            levels = c(
                "High income",
                "Upper middle income",
                "Lower middle income",
                "Low income"
            )
        )
    )

acled_regional_summary <- pigoar2026::acled_regional |>
    filter(
        year %in% c(2020:2024) & event_type %in% c("Protests", "Riots")
    ) |>
    group_by(
        region,
        income_group,
        country_code,
        year
    ) |>
    summarise(
        events_sum = sum(events, na.rm = TRUE),
        events_mean = mean(events, na.rm = TRUE),
        .groups = "drop"
    ) |>
    left_join(
        pigoar2026::population |>
            filter(year == 2020) |>
            select(country_code, total_population),
        by = c("country_code")
    ) |>
    # fix income group
    mutate(
        income_group = factor(
            income_group,
            levels = c(
                "High income",
                "Upper middle income",
                "Lower middle income",
                "Low income"
            )
        )
    )

acled_estimation <- acled_regional_summary |>
    left_join(
        cliaretl::closeness_to_frontier_dynamic |>
            select(
                country_code,
                year,
                vdem_core_v2stcritrecadm,
                wb_spi_std_and_methods,
                wjp_rol_2,
                wjp_rol_3_1,
                log_gdp
            ),
        by = c("country_code", "year")
    ) |>
    mutate(
        across(
            c(events_sum, vdem_core_v2stcritrecadm:wjp_rol_3_1, log_gdp),
            \(x) as.vector(scale(x))
        )
    ) |>
    mutate(
        income_group = factor(
            income_group,
            levels = c(
                "High income",
                "Upper middle income",
                "Lower middle income",
                "Low income"
            )
        )
    )

# heterogeneity ----------------------------------------------------------
# global
acled_demonstrations_regional |>
    mutate(
        quarter = lubridate::quarter(week, type = "date_first")
    ) |>
    compute_summary(
        cols = "events",
        fns = "sum",
        groups = "quarter"
    ) |>
    mutate(
        events_index = value / value[quarter == min(quarter)] * 100
    ) |>
    ggplot(aes(x = quarter, y = events_index)) +
    geom_line() +
    scale_y_continuous(
        limits = c(0, 200)
    ) +
    geom_hline(
        yintercept = 100,
        linetype = "dashed"
    ) +
    labs(
        x = "Time",
        y = "Protests (Baseline = 100)"
    )

ggsave(
    here("analysis", "figs", "final", "fig_5a_global_demonstration_trends.png"),
    width = 10,
    height = 10,
    dpi = 300,
    bg = "white"
)

# income group
acled_demonstrations_regional |>
    plot_events_index(
        "income_group",
        "Income Group"
    ) +
    guides(
        color = guide_legend(
            "Income Group",
            nrow = 2
        )
    )

ggsave(
    here("analysis", "figs", "final", "fig_5b_global_demonstration_trends_income.png"),
    width = 10,
    height = 10,
    dpi = 300,
    bg = "white"
)

# correlation with institutional capacity --------------------------------
# linear regression
income_groups <- acled_estimation |>
    filter(
        !is.na(income_group)
    ) |>
    distinct(income_group) |>
    pull()

lm_protests_pooled <- lm(
    events_sum ~ vdem_core_v2stcritrecadm +
        wb_spi_std_and_methods +
        wjp_rol_2 +
        wjp_rol_3_1 +
        total_population +
        log_gdp +
        as.factor(income_group) +
        as.factor(region) +
        as.factor(year),
    data = acled_estimation
)

lm_protests_income_group <- income_groups |>
    map(
        \(group) {
            lm(
                events_sum ~ vdem_core_v2stcritrecadm +
                    wb_spi_std_and_methods +
                    wjp_rol_2 +
                    wjp_rol_3_1 +
                    log_gdp +
                    total_population +
                    as.factor(region) +
                    as.factor(year),
                data = acled_estimation |> filter(income_group == group)
            )
        }
    ) |>
    set_names(income_groups)

list(
    "Low income" = lm_protests_income_group[["Low income"]],
    "Lower middle income" = lm_protests_income_group[["Lower middle income"]],
    "Upper middle income" = lm_protests_income_group[["Upper middle income"]],
    "High income" = lm_protests_income_group[["High income"]]
) |>
    purrr::map_dfr(broom::tidy, .id = "model") |>
    filter(
        !grepl("Intercept|as.factor|total_population|log_gdp", term)
    ) |>
    mutate(
        model = factor(
            model,
            levels = c("Low income", "Lower middle income", "Upper middle income", "High income")
        )
    ) |> 
    dwplot(
        dot_args = list(
            aes(colour = model),
            size = 5
        ),
        whisker_args = list(size = 1.5)
    ) |>
    relabel_predictors(
        c(
            vdem_core_v2stcritrecadm = "Meritocratic criteria for appointment",
            wb_spi_std_and_methods = "Standards and methods for data",
            wjp_rol_2 = "Degree of integrity",
            wjp_rol_3_1 = "Publicized laws and government data"
        )
    ) +
    xlab("Coefficient") +
    ylab("") +
    geom_vline(
        xintercept = 0,
        colour = "grey60",
        linetype = 2
    ) +
    labs(x = "Coefficient Estimate with 95% CIs", y = "") +
    theme(
        legend.position = "bottom"
    ) +
    scale_shape_discrete(
        name = "Income group",
        breaks = c(0, 1)
    ) +
    scale_color_manual(
        values = rev(ggthemes::solarized_pal()(4)),
        name = "Income group"
    ) +
    guides(color = guide_legend(nrow = 2))

ggsave(
    here("analysis", "figs", "final", "fig_6_regression_income.png"),
    dpi = 300,
    width = 14,
    height = 8,
    bg = "white"
)
