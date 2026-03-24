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

open_budget <- pigoar2026::open_budget |> 
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

# open budget over time
open_budget |> 
  mutate(
    year = as.integer(year)
  ) |> 
  group_by(income_group, year) |> 
  summarise(
    mean_budget_transparency_score = mean(budget_transparency_score, na.rm = TRUE),
    n = n()
  ) |> 
  filter(!is.na(income_group)) |> 
  ggplot(
    aes(year, mean_budget_transparency_score, color = income_group)
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
  labs(
    x = "Year",
    y = "Budget transparency score"
  ) +
  guides(
    color = guide_legend(nrow = 2)
  )

ggsave(
  here("analysis", "figs", "custom", "open_budget.png"),
  width = 12,
  height = 10,
  dpi = 300,
  bg = "white"
)

# correlation with FMIS
budget_execution_fmis <- budget_execution |> 
  mutate(
    year = as.integer(year)
  ) |> 
  filter(
    between(year, 2019, 2024)
  ) |> 
  group_by(
    country_code, income_group, region
  ) |> 
  summarise(
    budget_execution_rate = mean(budget_execution_rate, na.rm = TRUE)
  ) |> 
  left_join(
    cliaretl::closeness_to_frontier_static |> 
      select(country_code, wb_gtmi_pfm_mis),
    by = c("country_code")
  )

lm(
  budget_execution_rate ~ wb_gtmi_pfm_mis +
    as.factor(income_group) + as.factor(region),
  data = budget_execution_fmis
) |> 
  dwplot(
        dot_args = list(
            aes(colour = model),
            size = 5
        ),
        whisker_args = list(size = 1.5)
  )
  
budget_execution_fmis |> 
  ggplot(
    aes(wb_gtmi_pfm_mis, budget_execution_rate)
  ) +
  geom_point(
    aes(
      color = income_group
    ),
    shape = 1,
    size = 4,
    stroke = 1,
    alpha = 0.8
  ) +
  geom_smooth(
    method = "lm"
  ) +
  scale_color_solarized()
