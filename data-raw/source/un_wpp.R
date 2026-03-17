## code to prepare `unwpp2024` dataset goes here
# available in: https://population.un.org/wpp/downloads?folder=Standard%20Projections&group=Most%20used
# access date: 03/06/2026
library(readxl)
library(dplyr)
library(janitor)
library(here)


# load data --------------------------------------------------------------

urldata <- "https://population.un.org/wpp/assets/Excel%20Files/1_Indicator%20(Standard)/EXCEL_FILES/1_General/WPP2024_GEN_F01_DEMOGRAPHIC_INDICATORS_COMPACT.xlsx"

# ext_urldata <- "https://population.un.org/wpp/assets/Excel%20Files/1_Indicator%20(Standard)/EXCEL_FILES/1_General/WPP2024_GEN_F01_DEMOGRAPHIC_INDICATORS_FULL.xlsx"

# Download to a temp file first
temp_file <- tempfile(fileext = ".xlsx")
download.file(urldata, destfile = temp_file, mode = "wb")

unwpp2024_raw <- read_excel(temp_file, sheet = "Medium variant", skip = 16) |>
  clean_names()



# clean data -----------------------------------------------------------
# The dataset contains demographic indicators for various countries.
# We need to focus on regional and income groups estimates of population anual growth for the ucoming 10 years (2025-2035).
# The indicator for population growth is "population_growth_rate_percentage" to number format.

unwpp2024_clean <- unwpp2024_raw |>
  mutate(
    population_growth_rate_percentage = as.numeric(
      population_growth_rate_percentage
    ),
    total_population_as_of_1_january_thousands = as.numeric(
      total_population_as_of_1_january_thousands
    ),
    median_age_as_of_1_july_years = as.numeric(median_age_as_of_1_july_years)
  ) |>
  filter(
    # type %in% c("Region", "Income Group"),  # keep only regional/income aggregates
    variant == "Medium", # keep only medium variant projections
    year >= 2026 & year <= 2036
  ) |>
  select(
    iso3_alpha_code,
    region_subregion_country_or_area,
    type,
    year,
    population_growth_rate_percentage,
    total_population_as_of_1_january_thousands,
    median_age_as_of_1_july_years
  )


# Clean up the group_class names by removing "countries" and replacing dashes with spaces, then filter for relevant groups
unwpp_data <- unwpp2024_clean |>
  rename(
    country_code = iso3_alpha_code,
    group_class = region_subregion_country_or_area,
    population_growth_rate = population_growth_rate_percentage,
    total_population = total_population_as_of_1_january_thousands,
    median_age = median_age_as_of_1_july_years
  ) |>
  mutate(
    group_class = group_class |>
      stringr::str_replace_all("[-–]", " ") |>
      stringr::str_remove("countries") |>
      stringr::str_squish() |>
      stringr::str_replace("Low and middle income", "Lower middle income")
  ) 


# save data --------------------------------------------------------------
usethis::use_data(unwpp_data, overwrite = TRUE)
