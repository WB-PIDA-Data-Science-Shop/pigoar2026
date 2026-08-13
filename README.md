<!-- badges: start -->
[![R-CMD-check](https://github.com/WB-PIDA-Data-Science-Shop/pigoar2026/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/WB-PIDA-Data-Science-Shop/pigoar2026/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

## Overview
The `pigoar2026` package is designed to streamline the analysis and 
visualization of the Public Institutions Global Observatory (PIGO): Annual Review 2026.
It provides the following key features:

**Data Transformations:** Scripts to clean and reshape raw inputs, 
located in the `data-raw/source` directory.

**Data Analysis:** Functions in the `R` directory and scripts in the 
`analysis` directory for generating visualizations and insights.

This `README` guides you through setting up your environment, restoring 
dependencies, and running the full analysis pipeline.



**Project Structure:**

The repository will be organized as follows:

```         
├── data-raw/           # Raw data and initial processing scripts.
│   ├── source          # Scripts for cleanign raw data
│   ├── input           # Raw data files (provided by the WKGPI Team)
│   └── output          # Intermediate outputs for analysis
├── analysis/           # Report ready figures and scripts.
│   ├── figs            # Directory that hosts the final generated plots.
│   ├── source          # Scripts for analysis and plotting
├── R/                  # Custom R functions.
├── documentation/      # Reproducibility documentation.
├── data/               # Processed data for analysis.
├── man/                # R package manual pages.
├── renv/               # R package dependency management.
├── README.Rmd          # Overview to run workflow.
├── pigographs.Rproj    # RStudio project file.
├── .gitignore          # Files ignored by Git.
├── .Rbuildignore       # Files ignored when building.
├── .Rprofile           # Project-specific R startup settings.
├── DESCRIPTION         # Project metadata.
├── LICENSE             # Licensing information.
├── LICENSE.md          # License in Markdown format.
├── NAMESPACE           # Package namespace definition.
├── main.R              # Main file to reproduce the whole workflow
└── spielplatz/         # Sandbox/experimentation area.


## Requirements

### Software
- **R**: Version 4.3 or higher
- **Dependency Management**: `renv` for managing package dependencies
- **Development Tools**: `devtools` for package loading

### Hardware
- **Memory**: No minimum required, but 16 GB is recommended.
- **Run Time**: ~3 minutes on recommended hardware.

## Installations for Replication

**1. Restore Package Dependencies:**

a. Clone this repository and open the `pigoar2026.Rproj` file in RStudio.  
b. Execute `renv::restore()` in the RStudio console to install the exact 
   package versions in the `renv.lock` file.


**2. Execute the Full Workflow:**

Open `main.R` and run it. This executes the following in order:

a. **Data Transformation:** Confirm the `input/` folder provided by the 
   WKGPI team is in the project root. Source the scripts in `data-raw/source/` 
   sequentially.

b. **Data Analysis & Visualization:** Source all scripts in `analysis/`. 
   Verify that each plot in `analysis/figs/` matches the corresponding figure in 
   the PIGO Annual Review 2026 exactly.
