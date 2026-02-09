# set-up -----------------------------------------------------------------
library(cliaretl)
library(dplyr)
library(ggplot2)
library(plotly)
library(lubridate)
library(purrr)

devtools::load_all()

theme_set(
    ggthemes::theme_few(base_size = 24)
)

# visualize --------------------------------------------------------------
gov_documents <- pigoar2026::wb_documents |> 
  filter(
    owner_label == "gov" & doc_type == "Report"
  ) |> 
  mutate(
    doc_quarter = lubridate::round_date(
      lubridate::ymd_hms(doc_date), 
      unit = "quarter"
    )
  )

gov_documents |>
  count(doc_quarter, name = "Total") |> 
  ggplot(
    aes(doc_quarter, Total)
  ) +
  geom_point() +
  geom_line()

gov_documents |> 
  count(doc_quarter, owner_code, name = "Total") |> 
  ggplot(
    aes(doc_quarter, Total)
  ) +
  geom_point() +
  geom_line() +
  facet_wrap(
    vars(owner_code),
    nrow = 7
  )
