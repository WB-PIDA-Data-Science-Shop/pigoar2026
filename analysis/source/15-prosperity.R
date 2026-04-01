# set-up -----------------------------------------------------------------
library(dplyr)
library(ggplot2)

set.seed(101010)

# data-load ---------------------------------------------------------------
ctf_static <- cliaretl::closeness_to_frontier_static |>
  # only retain countries
  filter(country_group == 0)

dictionary <- cliaretl::db_variables