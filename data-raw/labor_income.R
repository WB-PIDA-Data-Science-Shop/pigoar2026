## code to prepare `labor_income` dataset goes here
# source: https://ilostat.ilo.org/topics/labour-income/
library(dplyr)
library(readr)
library(countrycode)

labor_income_raw <- read_csv(
  "https://rplumber.ilo.org/data/indicator/?id=SDG_1041_NOC_RT_A&lang=en&type=label&format=.csv&channel=ilostat&title=sdg-indicator-1041-labour-income-share-as-a-percent-of-gdp-annual"
)

labor_income <- labor_income_raw |> 
  transmute(
    country_code = countrycode(ref_area.label, origin = "country.name", dest = "wb"),
    year = time,
    labor_income = obs_value,
    status_label = obs_status.label
  )

usethis::use_data(labor_income, overwrite = TRUE)
