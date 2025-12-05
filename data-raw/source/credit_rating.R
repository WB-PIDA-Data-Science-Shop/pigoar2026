## code to prepare `credit_rating` dataset goes here
credit_rating_raw <- get_data360_api(
  "WEF_TTDI",
  "WEF_TTDI_INDCCREDITRATE",
  pivot = FALSE
)

credit_rating_raw |> 
  count(COMP_BREAKDOWN_1)

usethis::use_data(credit_rating, overwrite = TRUE)
