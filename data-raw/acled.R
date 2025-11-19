## code to prepare `acled` dataset goes here
library(readxl)
library(dplyr)
library(janitor)
library(here)

# Read ACLED demonstrations data
# available at https://acleddata.com/aggregated/number-demonstration-events-country-year
# Downloaded 11/18/2025
acled_raw <- read_excel(
    here("data-raw", "input", "acled", "number_of_demonstration_events_by_country-year_as-of-07Nov2025.xlsx")
)

acled_asia_raw <- read_excel(
    here("data-raw", "input", "acled", "Asia-Pacific_aggregated_data_up_to-2025-11-01.xlsx")
)

# Clean and process the data
acled <- acled_raw |>
    clean_names()

acled_asia <- acled_asia_raw |>
    clean_names()

usethis::use_data(acled, overwrite = TRUE)
usethis::use_data(acled_asia, overwrite = TRUE)
