# CTF 2020 to 2024 coverage by country

# Load necessary libraries
library(haven)
library(here)
library(readxl)
library(readr)
library(dplyr)
library(purrr)
library(stringr)
library(tidyr)
library(janitor)
library(ggthemes)
library(ggplot2)  
library(readr)

devtools::load_all()

theme_set(
    theme_few(base_size = 14)
)

ggsave <- partial(
  ggplot2::ggsave,
  bg     = "white",
  width  = 12,
  height = 8
)


# load-data --------------------------------------------------------------
clusters_data <- readRDS(here("data-raw", "output", "cluster_coverage_data.rds"))

# data-transf ------------------------------------------------------------
glimpse(clusters_data)
