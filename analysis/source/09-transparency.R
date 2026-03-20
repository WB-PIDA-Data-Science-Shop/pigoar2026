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
      legend.position = "none",
      strip.text = element_text(size = 20, face = "bold"),
      legend.text = element_text(size = 18),
      legend.title = element_text(size = 16, face = "bold")
    )
)

# prepare data -----------------------------------------------------------
open_budget <- pigoar2026::open_budget |> 
  left_join(
    pigoar2026::countryclass,
    by = c("country_code")
  )

# visualize --------------------------------------------------------------
open_budget |>
  ggplot(
    aes(
      year,
      budget_transparency_score
    )
  ) +
  geom_smooth(
    aes(color = income_group)
  )

open_budget |> 
  plot_point_line(
    "year",
    "budget_transparency_score"
  )