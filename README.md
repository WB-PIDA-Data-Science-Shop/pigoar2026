## Overview
This repository reproduces the analysis presented in the Public Institutions Global Observatory (PIGO): Annual Review 2026.

- Status: Work in Progress.
- Target Completion Date: Spring, 2026

**Project Structure:**

The repository will be organized as follows:

```         
├── data-raw/           # Raw data and initial processing scripts.
│   ├── source          # Scripts for cleanign raw data
│   ├── input           # Raw data files (provided by the EGVPI Team)
│   └── output          # Intermediate outputs for analysis
├── analysis/           # Scripts for analysis and plotting.
├── R/                  # Custom R functions.
├── documentation/      # Reproducibility documentation.
├── data/               # Processed data for analysis.
├── figures/            # Final generated plots.
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


