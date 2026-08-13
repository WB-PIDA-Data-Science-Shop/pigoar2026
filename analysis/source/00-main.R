# Main R Script for running the analysis and generating the results for the project

# Set a seed for reproducibility
set.seed(1010)


# library-load -----------------------------------------------------------

### run the following line on code only the first time you are running the project 
### to install the packages used in the project
# renv::restore()

library(here)

# Defining all the folders that contain the scripts to be sourced
dirs <- c(
  here("data-raw", "source"), # Data Cleaning Scripts (optional)
  here("analysis", "source") # Data Analysis scripts
)


# source-scripts ---------------------------------------------------------

# helper fun to source every .R/.r file in a directory
# this will run the scripts in the right order lovated at both
# data-raw/source and analysis/source directories

source_all <- function(dir) {
  scripts <- list.files(
    path       = dir,
    pattern    = "\\.[Rr]$",
    full.names = TRUE
  )
  invisible(lapply(scripts, source))
}

# Source all the scripts listed
invisible(lapply(dirs, source_all))