
# ---------------------------------------------------------------------------- #
#                                    set-up                                    #
# ---------------------------------------------------------------------------- #
library(dplyr)
library(ggplot2)
library(ggthemes)
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
        by = c("country", "year")
    ) |>
    filter(year >= 2010) 
    
acled_events |>
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
