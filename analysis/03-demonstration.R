# set-up -----------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(ggthemes)
library(forcats)
library(sf)
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
    left_join(
        wb_income_and_region,
        by = c("country_code")
    ) |> 
    # country coverage improves in 2019
    filter(
        year >= 2019
    ) |> 
    mutate(
        income_group = fct_relevel(
            income_group,
            c(
                "Low income",
                "Lower middle income",
                "Upper middle income",
                "High income"
            )
        )
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

# spatial analysis -------------------------------------------------------
acled_asia_aggregate <- acled_asia |>
    filter(
        year(week) == 2024 &
        region %in% c("East Asia", "Southeast Asia", "Oceania") &
        event_type %in% c("Protests", "Riots")
    ) |> 
    group_by(centroid_latitude, centroid_longitude) |> 
    summarise(
        total_events = n(),
        .groups = "drop"
    )

acled_asia_sf <- acled_asia_aggregate |> 
    st_as_sf(
    coords = c("centroid_longitude", "centroid_latitude"), 
    crs = 4326  # WGS84 coordinate system (standard for lat/lon)
    )

wb_map |> 
    left_join(
        wb_income_and_region,
        by = c("country_code")
    ) |> 
    filter(
        region == "East Asia & Pacific"
    ) |> 
    ggplot() +
    geom_sf(
        fill = "orange2", color = "white"
    ) +
    geom_sf(
        aes(
            size = total_events
        ),
        data = acled_asia_sf, color = "steelblue3", alpha = 0.6
    ) +
    coord_sf(crs = st_crs("+proj=longlat +lon_0=120")) +
    labs(
        title = "Demonstrations in East Asia and the Pacific (2019-2024)",
        x = "Longitude",
        y = "Latitude"
    ) +
    theme_void()

ggsave(
    here("analysis", "figs", "map_global_demonstration_asia.png"),
    width = 14,
    height = 8,
    bg = "white"
)
