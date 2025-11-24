## code to prepare `mides` dataset goes here
# available in: https://reproducibility.worldbank.org/index.php/catalog/72
# access date: 7/3/2025
library(readr)
library(dplyr)
library(sf)
library(janitor)
library(here)

mides_input <- read_csv(
  here("data-raw", "input", "mides", "full_budget_execution_index.csv")
)

# download shape files
brazil_mun_shp <- get_shapefile_from_zip_url(
  "https://geoftp.ibge.gov.br/organizacao_do_territorio/malhas_territoriais/malhas_municipais/municipio_2024/Brasil/BR_Municipios_2024.zip"
  ) |>
  st_read() |>
  st_simplify(dTolerance = 100)

brazil_mun_shp <- brazil_mun_shp %>%
  clean_names() |>
  rename(
    municipality_code = cd_mun
  )

mides <- mides_input %>%
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
usethis::use_data(brazil_mun_shp, overwrite = TRUE)