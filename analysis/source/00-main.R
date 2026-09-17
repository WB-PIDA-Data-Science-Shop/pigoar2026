# Main R Script for running the analysis and generating the results for the project

# Set a seed for reproducibility
set.seed(1902)


# library-load -----------------------------------------------------------

### run the following line on code only the first time you are running the project 
### to install the packages used in the project
# renv::restore()
library(here)

# Defining all the folders that contain the scripts to be sourced
source_files <- fs::dir_ls(
  here("analysis", "source")
) |>
  stringr::str_subset(
    "00-main.R",
    negate = TRUE
  )

source_files |>
  purrr::map(source)
