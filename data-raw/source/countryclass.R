## code to prepare `countryclass` dataset goes here
# data of access: 3/19/2026
library(openxlsx)

countryclass_input <- read.xlsx(
  "https://ddh-openapi.worldbank.org/resources/DR0095333/download/"
)

countryclass <- countryclass_input |>
  select(
    economy = Economy,
    country_code = Code,
    region = Region,
    income_group = `Income.group`
  ) |>
  # fix country name
  mutate(
    economy = case_when(
      economy == "Vietnam" ~ "Viet Nam",
      TRUE ~ economy
    )
  )

usethis::use_data(countryclass, overwrite = TRUE)
