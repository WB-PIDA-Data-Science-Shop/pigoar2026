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
safe_source_all <- function(dir, exclude = "00-main.R") { ## Prevent sourcing this file (00-main.R) which would cause infinite recursion
  scripts <- list.files(
    path       = dir,
    pattern    = "\\.[Rr]$",
    full.names = TRUE
  )
  scripts <- scripts[basename(scripts) != exclude]
  invisible(lapply(scripts, source))
}

invisible(lapply(dirs, safe_source_all))