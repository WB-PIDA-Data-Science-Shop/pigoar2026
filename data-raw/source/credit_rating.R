## code to prepare `credit_rating` dataset goes here
credit_rating_raw <- get_data360_api(
  "WEF_TTDI",
  "WEF_TTDI_INDCCREDITRATE",
  pivot = FALSE
)

credit_rating <- credit_rating_raw |> 
  filter(
    COMP_BREAKDOWN_1 == "WEF_TTDI_VAL"
  ) |> 
  pivot_data360() |> 
  rename(
    credit_rating = wef_ttdi_indccreditrate
  ) |> 
  mutate(
    credit_rating = as.numeric(credit_rating)
  )

usethis::use_data(credit_rating, overwrite = TRUE)
