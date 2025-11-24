# set-up -----------------------------------------------------------------
library(dplyr)
library(dotwshiker)
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
  )

subnational_gov <- pigoar2026::mides |> 
  mutate(
    municipality_code = as.character(municipality_code),
    gdp_per_capita_tercile = ntile(gdp_per_capita, 3)
  ) |> 
  left_join(
    subnational_hr,
    by = c("municipality_code", "year")
  )

# visualize --------------------------------------------------------------
lm(
  weighted_average_delay ~ share_dismissed + log(gdp_per_capita) +
    idhm + log(population) +
    as.factor(state) + as.factor(year),
  data = subnational_gov
) |> 
  tidy() |> 
  filter(
    !grepl("Intercept|as.factor", term)
  ) |> 
  dwplot() %>%
  relabel_predictors(
    c(share_dismissed = "Dismissal Rate", weighted_average_delay = "Weighted Procurement Delay")
  )+
  theme_bw() + xlab("Coefficient") + ylab("") +
  geom_vline(
    xintercept = 0, colour = "grey60", linetype = 2
  )
  
subnational_gov |> 
  filter(
    !is.na(gdp_per_capita_tercile)
  ) |> 
  ggplot(
    aes(
      share_dismissed,
      weighted_average_delay,
      color = gdp_per_capita_tercile
    )
  ) +
  geom_point(
    alpha = 0.5
  ) +
  geom_smooth(
    method = "lm"
  ) +
  scale_x_log10() +
  scale_y_log10()
