## code to prepare `trust_gov` dataset goes here
# available in: https://dataverse.harvard.edu/file.xhtml?fileId=13283107&version=1.0&toolType=PREVIEW
# Obtained from Harvard Dataverse
# Sorurce: Besley, Timothy; Dann, Christopher; Dray, Sacha, 2025, 
# "Replication Data for: 'Growth Experiences and Trust in Government'", https://doi.org/10.7910/DVN/EYWTOW, 
# Harvard Dataverse, V1; country_year_cohort_panel.tab [fileName], UNF:6:VLfd+r9U0Bhx+9tERrSMrA== [fileUNF]
# access date: 03/20/2026


library(readxl)
library(dplyr)
library(janitor)
library(here)
library(haven)
library(ggplot2)


# load-data --------------------------------------------------------------

trust_gov_panel <- read_dta(
  here::here("data-raw/input/country_year_cohort_panel.dta")
) |> 
  mutate(year = as.numeric(survey_year)) |>
  filter(year >= 2010) |> 
  rename(
    country_code = ccode
  )


# transform-data ---------------------------------------------------------

# Calculate average trustat country-year level
trust_gov_avg <- trust_gov_panel |> 
  group_by(country_code, year) |> 
  summarise(trust_gov_level = mean(trust_govt, na.rm = TRUE)) |> 
  ungroup()


