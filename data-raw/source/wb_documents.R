## code to prepare `wb_documents` dataset goes here
library(dplyr)
library(purrr)
library(tidyr)
library(stringr)

devtools::load_all()

skip_rows <- 0
n_rows <- 5000
documents_tbl <- tibble()

total <- fetch_wb_documents_json() |> 
  pluck("total")

while (skip_rows < total) {
  resp_json <- fetch_wb_documents_json(
    os = skip_rows,
    rows = n_rows
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
  select(
    owner,
    owner_code
  ) |> 
  mutate(
    owner_label = "gov"
  )

# subset to reports and ICRs produced by GOV units
wb_documents <- wb_documents |>
  filter(
    doc_type %in%
      c(
        "Report",
        "Implementation Completion and Results Report",
        "Implementation Completion Report Review"
      )
  ) |>
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

# create thematic tags and round dates
wb_documents <- mutate(
    theme_category = case_when(
      str_detect(theme, "Public Finance Management|Public Expenditure Management|Domestic Revenue Administration|Debt Management") ~ "Public Finance Management",
      str_detect(theme, "Public Administration|Administrative and Civil Service Reform|Institutional Strengthening and Capacity Building") ~ "Personnel",
      str_detect(theme, "E-Government, inc. E-services|Data Production, Accessibility, and Use") ~ "Digital and Data",
      str_detect(theme, "Transparency, Accountability and Good Governance") ~ "Transparency and Accountability",
      T ~ "Other"
    )
  ) |> 
  # simplify dates
  mutate(
    doc_month = lubridate::round_date(
      lubridate::ymd_hms(doc_date), 
      unit = "month"
    )
  ) 

wb_documents_themes <- wb_documents |>
  select(theme) |> 
  # remove separate rows
  separate_rows(
    theme,
    sep = "(?<=\\S),(?=\\S)"  # comma preceded and followed by non-space
  ) |>
  mutate(theme = str_squish(theme)) |> 
  distinct(theme) |> 
  # a tag on subnational level governance
  mutate(
    theme_category = case_when(
      str_detect(theme, "Public Finance Management|Public Expenditure Management|Domestic Revenue Administration|Debt Management") ~ "Public Finance Management",
      str_detect(theme, "Public Administration|Administrative and Civil Service Reform|Institutional Strengthening and Capacity Building") ~ "Personnel",
      str_detect(theme, "E-Government, inc. E-services|Data Production, Accessibility, and Use") ~ "Digital and Data",
      str_detect(theme, "Transparency, Accountability and Good Governance") ~ "Transparency and Accountability",
      str_detect(theme, "Corruption|Integrity|Audit|Fraud|Asset Declarion|Conflict ofInterest") ~ "Degree of Integrity",
      str_detect(theme, "Decentralization|Subnational|Local Government|Municipal|Intergovernmental") ~ "Subnational Level Governance",
      T ~ "Other"
    )
  )

usethis::use_data(wb_documents, overwrite = TRUE)
usethis::use_data(gov_unit, overwrite = TRUE)
