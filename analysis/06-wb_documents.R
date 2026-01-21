# set-up -----------------------------------------------------------------
library(cliaretl)
library(dplyr)
library(ggplot2)
library(plotly)
library(purrr)

devtools::load_all()

theme_set(
    ggthemes::theme_few(base_size = 24)
)

# visualize --------------------------------------------------------------
gov_documents <- pigoar2026::wb_documents

gov_documents |> 
  count(doc_date) |> 
  ggplot() +
  geom_point() +
  geom_line()