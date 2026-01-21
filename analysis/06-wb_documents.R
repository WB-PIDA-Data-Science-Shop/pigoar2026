# set-up -----------------------------------------------------------------
library(cliaretl)
library(dplyr)
library(ggplot2)
library(purrr)

devtools::load_all()

theme_set(
    ggthemes::theme_few(base_size = 24)
)

# visualize --------------------------------------------------------------
wb_documents
