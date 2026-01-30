# IDENTIFY AND RANK COUNTRIES BASED ON INDICATORS VOLATILITY

###
# remember:
# include North America
# income level class
# Graph: labels colred, no points

# set-up ------------------------------------------------------------------
library(haven)
library(dplyr)
library(here)
library(tidyverse)
library(ggplot2)
library(janitor)
library(stringr)
library(readr)


devtools::load_all()


set.seed(101010)




# load data ---------------------------------------------------------------

cliar_indicators <- read_rds(
  here("data-raw", "input", "cliar", "compiled_indicators.rds")
) |>
  filter(region != "North America") |>
  filter(year >= 2015)

dictionary <- cliaretl::db_variables_final


# identify most volatile indicators ---------------------------------------

#Pivot longer to have indicators in one column
cliar_indicators_long <- cliar_indicators |>
select(-ends_with(c("_avg","_nygnpmktpkd"))) |>
  pivot_longer(
    cols = 6:last_col(),
    names_to = "indicator",
    values_to = "value"
  )


### Integrate panel with family_names and var_names
cliar_indicators_renamed <- cliar_indicators_long |>
  left_join(
    dictionary |>
      select(
        variable,
        var_name,
        family_var,
        family_name,
        benchmark_static_family_aggregate_download,
      ) |>
      distinct(),
    by = c("indicator" = "variable")
  ) |>
  filter(!is.na(family_var)) |>
  filter(benchmark_static_family_aggregate_download == "Yes")


#Calculate standard deviation for each country and indicator
indicator_volatility <- cliar_indicators_renamed |>
  group_by(country_name, indicator, family_name) |>
  summarise(
    sd_value = sd(value, na.rm = TRUE),
    mean_value = mean(value, na.rm = TRUE)
  ) |>
  ungroup() |>
  mutate(
    cv_value = sd_value / mean_value
  )


# Rank indicators by coefficient of variation for each cluster
indicator_rankings <- indicator_volatility |>
  group_by(family_name) |>
  arrange(desc(cv_value)) |>
  mutate(
    rank = row_number()
  ) |>
  ungroup()

