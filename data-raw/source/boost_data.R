## code to prepare `budget_outturn` dataset goes here
# available in: MEGA databricks platform
# access date: 04/02/2026

library(dplyr)
library(purrr)
library(tidyr)
library(stringr)
library(here)
library(readr)

#data-load-------------------------------------------------------------

budget_raw <- read_csv(
  here::here("data-raw/input/BOOST_country_2026_03_24_14_40_53.csv")
) |>
  mutate(
    approved = as.numeric(approved),
    revised  = as.numeric(revised),
    executed = as.numeric(executed)
  )

country_class <- cliaretl::wb_country_list


# clean data -----------------------------------------------------------

# Join to get country_code from country_class via country_name,
# drop func_sub, then aggregate by country_code and year
budget_clean <- budget_raw |>
  left_join(country_class, by = "country_name") |>
  select(-func_sub) |>
  filter(!is.na(country_code)) |>
  group_by(country_code, year) |>
  summarise(
    total_executed = sum(executed, na.rm = TRUE),
    total_approved = sum(approved, na.rm = TRUE),
    .groups = "drop"
  )

# Calculate outturn using PEFA formula: Aggregate expenditure outturn = Executed Budget/Approved Budget*100
budget_outturn <- budget_clean |>
  mutate(
    budget_outturn = if_else(total_approved > 0, (total_executed / total_approved) * 100, NA_real_)
  ) 

# save data --------------------------------------------------------------
usethis::use_data(budget_outturn, overwrite = TRUE)

