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

devtools::load_all()

theme_set(
    theme_few()
)

# global trends ----------------------------------------------------------
acled_events <- acled |>
    inner_join(
        wdi_indicators,
        by = c("country_code", "year")
    ) |> 
    filter(
        year >= 2019
    )

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
    here("analysis", "figs", "global_demonstration_trends.png")
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
        linewidth = 2
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
    here("analysis", "figs", "global_demonstration_trends_income.png")
)

# by region
acled_events |>
    filter(!is.na(region)) |>
    group_by(region, year) |>
    summarize(
        events = sum(events, na.rm = TRUE),
        .groups = "drop"
    ) |>
    ggplot(aes(x = year, y = events, color = region)) +
    geom_line(linewidth = 2) +
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
    here("analysis", "figs", "global_demonstration_trends_region.png")
)

# regional analysis ------------------------------------------------------
acled_demonstrations_regional <- acled_regional |>
    filter(
        between(year, 2019, 2024) &
            event_type %in% c("Protests", "Riots")
    ) |> 
    mutate(
        month = lubridate::floor_date(week, unit = "months"),
        year = lubridate::floor_date(week, unit = "years") |> as.numeric()
    ) |> 
    left_join(
        wdi_indicators,
        by = c("country_code", "year")
    )

acled_demonstrations_regional |> 
    compute_summary(
        cols = "events",
        fns = "sum",
        groups = "month"
    ) |> 
    ggplot(aes(x = month, y = value)) +
    geom_line() +
    scale_y_continuous(
        limits = c(0, NA)
    ) +
    labs(
        title = "Global Protests and Riots Over Time",
        x = "Time",
        y = "Total"
    )

ggsave(
    here("analysis", "figs", "global_demonstration_trends.png")
)

acled_demonstrations_regional |> 
    compute_summary(
        cols = "events",
        fns = "sum",
        groups = c("month", "income_group")
    ) |> 
    filter(
        !is.na(income_group)
    ) |> 
    ggplot(
        aes(x = month, y = value, color = income_group)
    ) +
    geom_line(
        linewidth = 2
    ) +
    scale_y_continuous(
        limits = c(0, NA)
    ) +
    scale_color_brewer(
        palette = "Set1"
    ) +
    facet_wrap(
        vars(income_group)
    ) +
    theme(
        legend.position = "none"
    ) +
    labs(
        title = "Global Protests and Riots Over Time",
        x = "Time",
        y = "Total"
    )

ggsave(
    here("analysis", "figs", "global_demonstration_trends_income.png")
)

# by region
acled_demonstrations_regional |> 
    compute_summary(
        cols = "events",
        fns = "sum",
        groups = c("month", "region")
    ) |> 
    filter(
        !is.na(.data[["region"]])
    ) |> 
    ggplot(
        aes(x = month, y = value, color = .data[["region"]])
    ) +
    geom_line(
        linewidth = 2
    ) +
    scale_y_continuous(
        limits = c(0, NA)
    ) +
    scale_color_brewer(
        palette = "Set1"
    ) +
    facet_wrap(
        vars(.data[["region"]])
    ) +
    theme(
        legend.position = "none"
    ) +
    labs(
        title = "Global Protests and Riots Over Time, by Region",
        x = "Time",
        y = "Total"
    )

ggsave(
    here("analysis", "figs", "global_demonstration_trends_region.png")
)

# spatial analysis -------------------------------------------------------
acled_regional_sf <- acled_regional |> 
    filter(
        year == 2024 &
            event_type %in% c("Protests", "Riots")
    ) |> 
    group_by(
        region, centroid_longitude, centroid_latitude
    ) |> 
    summarise(
        total_events = sum(events),
        .groups = "drop"
    ) |> 
    st_as_sf(
        coords = c("centroid_longitude", "centroid_latitude"), 
        crs = 4326  # WGS84 coordinate system (standard for lat/lon)
    )

wb_regions <- wb_income_and_region |> 
  filter(!is.na(region) & region != "North America") |> 
  distinct(region) |> 
  pull()

wb_acled_maps <- wb_regions |> 
  map(
    \(region) plot_regional_map(region, acled_regional_sf)
  )

map_names <- sprintf(
    here("analysis", "figs", "map_acled_%s.png"), janitor::make_clean_names(wb_regions)
)

walk2(
    wb_acled_maps,
    map_names,
    ~ ggsave(.x, filename = .y, dpi = 300, width = 14, height = 12, bg = "white")
)
