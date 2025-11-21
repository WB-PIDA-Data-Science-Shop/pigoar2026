## code to prepare `acled` dataset goes here
library(readxl)
library(dplyr)
library(janitor)
library(here)
library(httr)

# Read ACLED demonstrations data
# available at https://acleddata.com/aggregated/number-demonstration-events-country-year
# accessed 11/18/2025
acled_raw <- read_excel(
  here(
    "data-raw",
    "input",
    "acled",
    "number_of_demonstration_events_by_country-year_as-of-07Nov2025.xlsx"
  )
)

# granular data for each region
# available at: https://acleddata.com/aggregated/aggregated-data-(your-region)
# accessed 11/21/2025
acled_regional_raw <- fs::dir_ls(
  here("data-raw", "input", "acled"),
  pattern = "*2025-11-01.xlsx$"
) |>
  purrr::map_dfr(read_excel)

# Clean and process the data
acled <- acled_raw |>
  clean_names() |>
  mutate(
    country_code = countrycode::countrycode(
      country,
      origin = "country.name",
      destination = "wb"
    )
  ) |>
  left_join(
    cliaretl::wb_income_and_region,
    by = c("country_code")
  )

acled_regional <- acled_regional_raw |>
  clean_names() |>
  mutate(
    country_code = countrycode::countrycode(
      country,
      origin = "country.name",
      destination = "wb"
    ),
    year = lubridate::year(week)
  ) |>
  select(-region) |>
  left_join(
    cliaretl::wb_income_and_region,
    by = c("country_code")
  )

usethis::use_data(acled, overwrite = TRUE)
usethis::use_data(acled_regional, overwrite = TRUE)
