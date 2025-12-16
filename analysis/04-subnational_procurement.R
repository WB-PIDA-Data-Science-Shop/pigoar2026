# set-up -----------------------------------------------------------------
library(dplyr)
library(broom)
library(dotwhisker)
library(ggthemes)
library(ggplot2)

devtools::load_all()

theme_set(
  theme_minimal()
)

# clean ------------------------------------------------------------------
subnational_hr <- pigoar2026::rais_mun |> 
  rename(
    municipality_code = id_municipio,
    year = ano
  ) |> 
  # weigh local governments by headcount, changing weights each year
  group_by(year) |> 
  mutate(
    weighted_share_dismissed = (total_headcount/sum(total_headcount)) * share_dismissed
  )

subnational_gov <- pigoar2026::mides |> 
  filter(
    between(year, 2008, 2021)
  ) |> 
  mutate(
    municipality_code = as.character(municipality_code),
    gdp_per_capita_quartile = ntile(gdp_per_capita, 4) |> 
      as.factor()
  ) |> 
  left_join(
    subnational_hr,
    by = c("municipality_code", "year")
  ) |> 
  mutate(
    year = as.character(year),
    across(
      is.numeric, 
      \(col) scale(col)
    )
  )

# visualize --------------------------------------------------------------
subnational_gov |> 
  filter(
    !is.na(gdp_per_capita_quartile)
  ) |> 
  ggplot(
    aes(
      x = weighted_share_dismissed,
      y = weighted_average_delay
      # color = gdp_per_capita_quartile
    )
  ) +
  # geom_point() +
  # plot binned averages
  stat_summary_bin(
    fun = "mean",
    bins = 30,         # choose the number of bins
    geom = "point",
    size = 3
  ) +
  geom_smooth(method = "lm") +
  scale_x_log10() +
  scale_y_log10()

ggsave(
  here("analysis", "figs", "correlation", "subnat_procur_hrm.png"),
  bg = "white"
)

# regression -------------------------------------------------------------
baseline_lm <- lm(
  weighted_average_delay ~ weighted_share_dismissed,
  data = subnational_gov
)

controls_lm <- update(baseline_lm, . ~ . + gdp_per_capita + idhm + population)
fe_lm <- update(controls_lm, . ~ . + as.factor(state) + as.factor(year))

list(
  "Baseline" = baseline_lm,
  "Controls" = controls_lm,
  "Fixed-effects" = fe_lm
) |> 
  purrr::map_dfr(tidy, .id = "model") |> 
  filter(
    !grepl("Intercept|as.factor", term)
  ) |> 
  dwplot(
    dot_args = list(
      aes(colour = model, shape = model),
      size = 5
    ),
    whisker_args = list(size = 1.5)
  )  |> 
  relabel_predictors(
    c(
      weighted_share_dismissed = "Dismissal Rate", 
      weighted_average_delay = "Weighted Procurement Delay",
      gdp_per_capita = "GDP per capita",
      idhm = "Human Development Index",
      population = "Population"
    )
  ) +
  xlab("Coefficient") + 
  ylab("") +
  geom_vline(
    xintercept = 0, colour = "grey60", linetype = 2
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
  scale_colour_grey(
    start = .3, end = .7, name = "Models"
  ) 

  ggsave(
    here("analysis", "figs", "procurement", "subnat_procur_hrm_fit.png"),
    height = 6,
    width = 9,
    bg = "white"
  )
