## code to prepare `mides` dataset goes here
# available in: https://reproducibility.worldbank.org/index.php/catalog/72
# access date: 11/24/2025
library(readr)
library(dplyr)
library(sf)
library(janitor)
library(here)

mides_input <- read_csv(
  here("data-raw", "input", "mides", "full_budget_execution_index.csv")
)

mides <- mides_input |> 
  select(
    state,
    year,
    municipality_code = municipality,
    weighted_average_delay = wavg_delay,
    population,
    gdp,
    gdp_per_capita,
    total_students,
    formal_market_workers,
    idhm
  )

usethis::use_data(mides, overwrite = TRUE)