## code to prepare `wb_documents` dataset goes here
library(dplyr)
library(purrr)
library(tidyr)
library(stringr)
library(here)
library(readr)

devtools::load_all()

# read-in data -----------------------------------------------------------
theme_2017 <- read_csv(
  here("data-raw", "input", "wb", "theme_taxonomy_2017.csv")
) |> 
  janitor::clean_names()

theme_2025 <- read_csv(
  here("data-raw", "input", "wb", "theme_taxonomy_2025.csv")
) |> 
  janitor::clean_names()

theme_consolidated <- list(
    `2017` = theme_2017,
    `2025` = theme_2025
  ) |> 
  bind_rows(
    .id = "year"
  ) |> 
  mutate(
    across(starts_with("theme"), stringr::str_to_lower)
  ) |>
  pivot_wider(
    id_cols = c(year, code),
    names_from = level,
    values_from = theme_name,
    names_prefix = "level_"
  ) |>
  # fill down within each taxonomy
  group_by(year) |>
  fill(level_1, .direction = "down") |>
  fill(level_2, .direction = "down") |>
  ungroup() |>
  # keep only level 3 rows (most granular)
  filter(!is.na(level_3)) |>
  rename(
    theme_level_1 = level_1,
    theme_level_2 = level_2,
    theme_level_3 = level_3
  ) 

# fetch documents --------------------------------------------------------
total <- fetch_wb_documents_json(
  doc_type = c(
        "Report",
        "Implementation Completion and Results Report",
        "Implementation Completion Report Review"
      )
) |> 
  pluck("total")

skip_rows <- 0
n_rows <- 1000
documents_tbl <- tibble()

while (skip_rows < total) {
  resp_json <- fetch_wb_documents_json(
    os = skip_rows,
    rows = n_rows,
    doc_type = c(
        "Report",
        "Implementation Completion and Results Report",
        "Implementation Completion Report Review"
    )
  )

  # flatten authors to avoid extra rows
  docs_tbl <- extract_wb_documents(resp_json)

  documents_tbl <- documents_tbl |>
    dplyr::bind_rows(
      docs_tbl
    )

  skip_rows <- skip_rows + n_rows

  message("Fetched ", nrow(documents_tbl), " / ", total)
  Sys.sleep(2) # pause
}

# process documents ------------------------------------------------------
wb_documents <- documents_tbl |>
  rename(
    document_id = id,
    doc_type = docty,
    doc_date = docdt,
    orig_unit = origu,
    abstract = abstracts
  ) |>
  # fix abstracts
  mutate(
    abstract = purrr::map(
      abstract,
      ~ if (is.null(.x)) NA_character_ else .x
    ) |>
      unlist()
  )

gov_unit <- wb_documents |>
  mutate(
    stringr::str_squish(owner)
  ) |> 
  distinct(owner) |>
  separate_rows(
    owner,
    sep = ";"
  ) |>
  filter(
    grepl("GOV|Inst", owner) &
      grepl("^efi|^prosperity", owner, ignore.case = TRUE)
  ) |>
  mutate(
    owner_code = str_extract(owner, "(?<=\\().*?(?=\\))")
  ) |>
  distinct(owner, owner_code) |>
  select(
    owner,
    owner_code
  ) |> 
  mutate(
    owner_label = "gov"
  )

# tag reports and ICRs produced by GOV units
wb_documents <- wb_documents |>
  left_join(
    gov_unit,
    by = "owner"
  ) |> 
  mutate(
    owner_label = if_else(
      is.na(owner_label),
      "other",
      owner_label
    )
  )

# classify themes --------------------------------------------------------
wb_documents <- wb_documents |> 
  mutate(
    theme = str_squish(theme) |> 
      stringr::str_to_lower()
  ) |>
  # classify thematic category based on World Bank taxonomy from FY2017 and FY2025
  # available at https://worldbankgroup.sharepoint.com/sites/OPCS/SitePages/PublishingPages/Sector%20and%20Theme%20Tax-1765299121360.aspx
  rowwise() |>
  mutate(
    theme_category = {
      if (is.na(theme)) {
        NA_character_
      } else {
        categories <- c()
        
        # Personnel
        if (str_detect(theme, "administrative and civil service reform|public administration, compensation, and management")) {
          categories <- c(categories, "Personnel")
        }
        
        # Public Financial Management
        if (str_detect(theme, "public expenditure management|domestic revenue administration|debt management|public assets and investment management|government financial reporting and balance sheets|procurement|budget and treasury management")) {
          categories <- c(categories, "Public Financial Management")
        }
        
        # Integrity
        if (str_detect(theme, "anticorruption transparency and political economy")) {
          categories <- c(categories, "Integrity")
        }
        
        # Transparency and Accountability
        if (str_detect(theme, "transparency, accountability and good governance|oversight, accountability, and supreme audit institutions|government financial reporting and balance sheets|citizen engagement")) {
          categories <- c(categories, "Transparency and Accountability")
        }
        
        # Information Systems
        if (str_detect(theme, "e-government, incl. e-services|civil registration and identification|data production, accessibility and use|institutional strengthening and capacity building|core government systems|public service delivery|govtech enabling environment|data")) {
          categories <- c(categories, "Information Systems")
        }
        
        # Subnational
        if (str_detect(theme, "subnational fiscal policies|municipal institution building|intergovernmental and subnational institution building")) {
          categories <- c(categories, "Subnational Governance")
        }
        
        # Return concatenated or "Other"
        if (length(categories) > 0) {
          paste(categories, collapse = " | ")
        } else {
          "Other"
        }
      }
    }
  ) |>
  ungroup() |> 
  # simplify dates
  mutate(
    doc_month = lubridate::round_date(
      lubridate::ymd_hms(doc_date), 
      unit = "month"
    )
  )

usethis::use_data(wb_documents, overwrite = TRUE)
usethis::use_data(gov_unit, overwrite = TRUE)
