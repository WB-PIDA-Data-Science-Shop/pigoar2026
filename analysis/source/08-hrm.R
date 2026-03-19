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

# visualize --------------------------------------------------------------
gsps_institutional <- gsps |>
  filter(
    respondent_group == "Institution"
  )

gsps_national_merit <- gsps |>
  filter(
    respondent_group == "All"
  ) |>
  filter(
    indicator_group == "Recruitment standard: written examination" &
      country_code != "ROU"
  ) |>
  mutate(
    economy_fct = fct_reorder(economy, mean)
  )

gsps_institutional |>
  filter(
    indicator_group == "Recruitment standard: written examination" &
      country_code != "ROU"
  ) |>
  rename(
    score = mean
  ) |> 
  plot_quantile(
    "economy",
    "score",
    quantile_group = "indicator_group",
    reorder = TRUE
  ) +
  scale_x_discrete(
    limits = rev
  ) +
  scale_y_continuous(
    labels = scales::percent_format()
  ) +
  labs(
    x = "",
    y = "Share recruited with written exams"
  )

ggsave(
  here("analysis", "figs", "custom", "gsps_written_exam.png"),
  width = 14,
  height = 10,
  dpi = 300,
  bg = "white"
)
