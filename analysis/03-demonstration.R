# ---------------------------------------------------------------------------- #
#                                    set-up                                    #
# ---------------------------------------------------------------------------- #
library(dplyr)
library(ggplot2)
library(ggthemes)
library(sf)
library(rmapshaper)
library(geojsonio)
library(here)

devtools::load_all()
theme_set(
    theme_few()
)

# ---------------------------------------------------------------------------- #
#                                 global trends                                #
# ---------------------------------------------------------------------------- #
acled_events <- acled |>
    inner_join(
        wdi,
        by = c("country_code", "year")
    ) |>
    left_join(
        countryclass,
        by = c("country_code")
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
    labs(
        title = "Global Demonstration Events Over Time",
        x = "Year",
        y = "Number of Events"
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
    geom_line() +
    geom_point() +
    scale_color_brewer(
        palette = "Set1",
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
        events = sum(events, na.rm = TRUE)
    ) |>
    ggplot(aes(x = year, y = events, color = region)) +
    geom_line() +
    geom_point() +
    scale_color_brewer(
        palette = "Set1",
    ) +
    labs(
        title = "Global Demonstration Events Over Time, by Region",
        x = "Year",
        y = "Number of Events"
    )

ggsave(
    here("analysis", "figs", "global_demonstration_trends_region.png")
)

