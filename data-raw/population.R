## code to prepare `population` dataset goes here
population_raw <- get_data360_api(
  "WB_WDI",
  "WB_WDI_SP_POP_TOTL"
)

population <- population_raw |> 
  transmute(
    country_code,
    year = as.numeric(year),
    total_population = as.numeric(wb_wdi_sp_pop_totl)
  )

usethis::use_data(population, overwrite = TRUE)
