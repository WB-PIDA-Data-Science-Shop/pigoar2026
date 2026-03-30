## code to prepare `open_budget` dataset goes here
library(readr)
library(tidyr)
library(here)

# last accessed: 3/20/2026
open_budget_input <- read_csv(
  "https://data360files.worldbank.org/data360-data/data/IBP_OBS/IBP_OBS.csv"
)

open_budget <- open_budget_input |>
  pivot_wider(
    id_cols = c(REF_AREA, TIME_PERIOD),
    values_from = OBS_VALUE,
    names_from = INDICATOR              
  ) |>
  select(
    country_code = REF_AREA,
    year = TIME_PERIOD,
    budget_transparency_score = IBP_OBS_OBI,
    supreme_audit_oversight_score = IBP_OBS_SAI_OBI,
    oversight_score = IBP_OBS_OVERSIGHT_OBI,
    legislative_oversight_score = IBP_OBS_LEG_OBI,
    public_participation_score = IBP_OBS_PUB_ENG
  )

usethis::use_data(open_budget, overwrite = TRUE)