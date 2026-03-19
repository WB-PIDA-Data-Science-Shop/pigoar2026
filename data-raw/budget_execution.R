## code to prepare `budget_execution` dataset goes here
# source: https://data360.worldbank.org/en/indicator/WB_WDI_GF_XPD_BUDG_ZS
# accessed in: 3/19/2026
library(readr)
library(tidyr)

budget_execution_input <- get_data360_api(
  "WB_WDI", "WB_WDI_GF_XPD_BUDG_ZS"
)

budget_execution <- budget_execution_input |> 
  select(
    country_code,
    year,
    budget_execution_rate = wb_wdi_gf_xpd_budg_zs
  )

usethis::use_data(budget_execution, overwrite = TRUE)
