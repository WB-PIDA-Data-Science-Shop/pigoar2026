# set-up -----------------------------------------------------------------
library(dplyr)
library(ggplot2)

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

# prepare data -----------------------------------------------------------
open_budget <- pigoar2026::open_budget |>
  # improved coverage starting in 2012
  filter(
    year >= 2008
  ) |> 
  left_join(
    pigoar2026::countryclass,
    by = c("country_code")
  ) |> 
  mutate(
    income_group = factor(
      income_group,
      levels = c("High income", "Upper middle income", "Lower middle income", "Low income")
    )
  )

# visualize --------------------------------------------------------------
open_budget |> 
  rename(
    `Income group` = income_group
  ) |> 
  plot_point_line(
    "year",
    "budget_transparency_score",
    group = "Income group"
  ) +
  labs(
    x = "Year",
    y = "Budget transparency"
  )

ggsave(
  here("analysis", "figs", "custom", "open_budget.png"),
  width = 14,
  height = 10,
  dpi = 300,
  bg = "white"
)
