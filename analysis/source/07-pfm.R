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
budget_execution <- pigoar2026::budget_execution |> 
  left_join(
    countryclass,
    by = "country_code"
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

# visualize --------------------------------------------------------------
budget_execution |> 
  mutate(
    year = as.integer(year)
  ) |> 
  # data coverage issues before 2007 (less countries)
  # in 2024, low-income countries coverage drops from 10 to 2
  filter(
    between(year, 2007, 2023)
  ) |> 
  group_by(income_group, year) |> 
  summarise(
    mean_budget_execution_rate = mean(budget_execution_rate, na.rm = TRUE),
    n = n()
  ) |> 
  filter(!is.na(income_group)) |> 
  ggplot(
    aes(year, mean_budget_execution_rate, color = income_group)
  ) +
  geom_point(
    size = 5
  ) +
  geom_line(
    linewidth = 2
  ) +
  geom_hline(
    aes(yintercept = 100),
    linetype = "dashed"
  ) +
  scale_color_solarized(
    name = "Income Group"
  ) +
  theme(
    legend.position = "bottom"
  ) +
  coord_cartesian(
    ylim = c(75, 125)
  ) +
  labs(
    x = "Year",
    y = "Budget Execution Rate (Percentage)"
  ) +
  guides(
    color = guide_legend(nrow = 2)
  )

ggsave(
  here("analysis", "figs", "custom", "budget_execution.png"),
  width = 12,
  height = 10,
  dpi = 300,
  bg = "white"
)
