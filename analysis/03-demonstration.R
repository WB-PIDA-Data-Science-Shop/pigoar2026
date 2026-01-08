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
    theme_few(base_size = 20)
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
    )

acled_regional_summary <- pigoar2026::acled_regional |>
    filter(
        year %in% c(2020:2024) &
            event_type %in% c("Protests", "Riots")
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
            ) |> 
          mutate(
            across(
                vdem_core_v2stcritrecadm:wjp_rol_3_1,
                \(x) as.vector(scale(x))
            )
          ),
        by = c("country_code", "year")
    )

# global trends ----------------------------------------------------------
# pooled
acled_events |>
    filter(!is.na(income_group)) |>
    group_by(year) |>
    summarize(
        events = sum(events, na.rm = TRUE)
    ) |>
    ggplot(aes(x = year, y = events)) +
    geom_line() +
    geom_point() +
    coord_cartesian(
        ylim = c(0, 2.5e5)
    ) +
    labs(
        title = "Global Demonstrations Over Time",
        x = "Year",
        y = "Total"
    )

ggsave(
    here("analysis", "figs", "acled", "global_demonstration_trends.png")
)

# by income group
acled_events |>
    filter(!is.na(income_group)) |>
    group_by(income_group, year) |>
    summarize(
        events = sum(events, na.rm = TRUE)
    ) |>
    ggplot(aes(x = year, y = events, color = income_group)) +
    geom_line(
        linewidth = 1.2
    ) +
    geom_point(
        size = 6
    ) +
    scale_color_brewer(
        palette = "Set1",
    ) +
    scale_y_continuous(
        limits = c(0, NA)
    ) +
    scale_x_continuous(expand = expansion(mult = c(0.05, 0.15))) +
    geom_text(
        data = \(x) x |> group_by(income_group) |> slice_max(year, n = 1),
        aes(label = income_group),
        hjust = 0,
        nudge_x = 0.2,
        size = 4,
        fontface = "bold"
    ) +
    theme(
        legend.position = "none"
    ) +
    labs(
        title = "Global Demonstration Events Over Time, by Income Group",
        x = "Year",
        y = "Number of Events"
    )

ggsave(
    here("analysis", "figs", "acled", "global_demonstration_trends_income.png")
)

# by region
acled_events |>
    left_join(
        pigoar2026::population,
        by = c("country_code", "year")
    ) |> 
    filter(!is.na(region)) |>
    group_by(region, year) |>
    summarize(
        events_sum = sum(events, na.rm = TRUE),
        total_population = sum(total_population, na.rm = TRUE),
        events_per_capita = events_sum/total_population,
        .groups = "drop"
    ) |>
    ggplot(aes(x = year, y = events_per_capita, color = region)) +
    geom_line(linewidth = 1.2) +
    geom_point(size = 6) +
    geom_text(
        data = \(x) x |> group_by(region) |> slice_max(year, n = 1),
        aes(label = region),
        hjust = 0,
        nudge_x = 0.2,
        size = 4,
        fontface = "bold"
    ) +
    scale_color_brewer(palette = "Set1") +
    scale_x_continuous(expand = expansion(mult = c(0.05, 0.15))) +
    labs(
        title = "Global Demonstration Events Over Time, by Region",
        x = "Year",
        y = "Number of Events"
    ) +
    theme(legend.position = "none")

ggsave(
    here("analysis", "figs", "acled", "global_demonstration_trends_region.png")
)

# regional analysis ------------------------------------------------------
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
    events_index = value/value[quarter == min(quarter)] * 100
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
    here("analysis", "figs", "acled", "global_demonstration_trends.png"),
    width = 12,
    height = 9,
    bg = "white"
)

# income group
acled_demonstrations_regional |>
    plot_events_index(
        "income_group",
        "Income Group"
   )

ggsave(
    here("analysis", "figs", "acled", "global_demonstration_trends_income.png"),
    width = 12,
    height = 9,
    bg = "white"
)

# by region
acled_demonstrations_regional |>
   plot_events_index(
        "region",
        "Region",
        facet_group = TRUE
   )

ggsave(
    here("analysis", "figs", "acled", "global_demonstration_trends_region.png"),
    dpi = 300,
    height = 9,
    width = 12,
    bg = "white"
)

# spatial analysis -------------------------------------------------------
acled_regional_sf <- acled_regional |>
    filter(
        year == 2024 &
            event_type %in% c("Protests", "Riots")
    ) |>
    group_by(
        region,
        centroid_longitude,
        centroid_latitude
    ) |>
    summarise(
        total_events = sum(events),
        .groups = "drop"
    ) |>
    st_as_sf(
        coords = c("centroid_longitude", "centroid_latitude"),
        crs = 4326 # WGS84 coordinate system (standard for lat/lon)
    )

wb_regions <- wb_income_and_region |>
    filter(!is.na(region) & region != "North America") |>
    distinct(region) |>
    pull()

wb_acled_maps <- wb_regions |>
    map(
        \(region) {
            plot_regional_map(region, acled_regional_sf) +
                scale_size_continuous(
                    range = c(1, 10),
                    limits = c(
                        min(acled_regional_sf$total_events),
                        max(acled_regional_sf$total_events)
                    ),
                    name = "Total Number of Demonstrations"
                ) +
                labs(
                    title = sprintf("Demonstrations in %s (2024)", region),
                    x = "Longitude",
                    y = "Latitude"
                )
        }
    )

map_names <- sprintf(
    here("analysis", "figs", "acled", "map", "map_acled_%s.png"),
    janitor::make_clean_names(wb_regions)
)

walk2(
    wb_acled_maps,
    map_names,
    ~ ggsave(
        .x,
        filename = .y,
        dpi = 300,
        width = 14,
        height = 12,
        bg = "white"
    )
)

# correlation with institutional capacity --------------------------------
institutional_clusters <- colnames(
    cliaretl::closeness_to_frontier_static
) |>
    str_subset(
        "_avg$"
    )

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

plot_correlation <- map2(
    institutional_clusters,
    names(institutional_clusters),
    \(cluster, cluster_name) {
        acled_regional_summary |>
            left_join(
                cliaretl::closeness_to_frontier_static |>
                    select(country_code, ends_with("_avg")),
                by = c("country_code")
            ) |>
            filter(
                !is.na(income_group)
            ) |>
            ggplot(
                aes(
                    .data[[cluster]],
                    events_sum / total_population * 1e6,
                    color = income_group
                )
            ) +
            geom_point() +
            geom_smooth(
                method = "lm"
            ) +
            scale_y_log10(
                breaks = scales::breaks_log(n = 6),
                labels = scales::label_number(big.mark = ",")
            ) +
            facet_wrap(
                vars(income_group),
                nrow = 2,
                scales = "free_y"
            ) +
            labs(
                x = cluster_name,
                y = "Number of events per million (logged)"
            ) +
            theme(
                legend.position = "none"
            ) +
            scale_color_solarized()
    }
)

plot_correlation_names <- sprintf(
    here("analysis", "figs", "acled", "cor_acled_%s.png"),
    janitor::make_clean_names(institutional_clusters)
)

walk2(
    plot_correlation,
    plot_correlation_names,
    ~ ggsave(
        .x,
        filename = .y,
        dpi = 300,
        width = 14,
        height = 12,
        bg = "white"
    )
)

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
    "Low Income" = lm_protests_income_group[["Low income"]],
    "Lower middle income" = lm_protests_income_group[["Lower middle income"]],
    "Upper middle income" = lm_protests_income_group[["Upper middle income"]],
    "High income" = lm_protests_income_group[["High income"]]
) |>
    purrr::map_dfr(broom::tidy, .id = "model") |>
    filter(
        !grepl("Intercept|as.factor|total_population|log_gdp", term)
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
        vdem_core_v2stcritrecadm = "Personnel\n (Meritocratic criteria for appointment)",
        wb_spi_std_and_methods = "Information Systems\n (Standards and methods for data)",
        wjp_rol_2 = "Integrity\n (Absence of corruption)",
        wjp_rol_3_1 = "Transparency\n (Publicized laws and government data)"
      )
    ) +
    xlab("Coefficient") +
    ylab("") +
    geom_vline(
        xintercept = 0,
        colour = "grey60",
        linetype = 2
    ) +
    theme_bw() +
    labs(x = "Coefficient Estimate with 95% CIs", y = "") +
    theme(
        legend.position = "bottom"
    ) +
    scale_shape_discrete(
        name = "Models",
        breaks = c(0, 1)
    ) +
    scale_color_solarized(
        name = "Models"
    )

ggsave(
    here("analysis", "figs", "acled", "regression_income.png"),
    dpi = 300,
    width = 10,
    height = 6,
    bg = "white"
)
