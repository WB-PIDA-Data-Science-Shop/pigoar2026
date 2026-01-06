## code to prepare `wb_documents` dataset goes here
library(dplyr)
library(purrr)

skip_rows <- 0
n_rows <- 500
nrow_docs <- 500
documents_tbl <- tibble()

while (nrow_docs == n_rows) {
  resp <- httr2::request("https://search.worldbank.org/api/v3/wds") |>
    httr2::req_url_query(
      format = "json",
      fl = "id,count,abstracts,authr,docdt,origu,owner,projectid,theme,topic,docty",
      strdate = "2024-01-01",
      enddate = "2025-12-31",
      os = skip_rows,
      rows = n_rows
    ) |>
    httr2::req_perform()

  resp_json <- httr2::resp_body_json(resp)

  # flatten authors to avoid extra rows
  docs <- resp_json |>
    pluck("documents") |>
    map(
      \(doc) {
        author <- doc |>
          purrr::pluck("authors", .default = NULL)

        doc$authors <- author |>
          purrr::map_chr(
            \(au) {
              au |>
                pluck("author", .default = as.character(au))
            }
          ) |>
          paste(collapse = ";")

        doc
      }
    )

  docs_tbl <- docs |>
    dplyr::bind_rows() |>
    # remove extra row
    filter(
      !is.na(id)
    )

  documents_tbl <- documents_tbl |>
    dplyr::bind_rows(
      docs_tbl
    )

  nrow_docs <- nrow(docs_tbl)
  skip_rows <- skip_rows + n_rows
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
  inner_join(
    gov_unit,
    by = "owner"
  )

wb_documents_themes <- wb_documents |>
  select(theme) |> 
  separate_rows(
    theme,
    sep = "(?<=\\S),(?=\\S)"  # comma preceded and followed by non-space
  ) |>
  mutate(theme = str_squish(theme)) |> 
  distinct(theme) |> 
  mutate(
    theme_category = case_when(
      str_detect(theme, "Public Finance Management|Public Expenditure Management|Domestic Revenue Administration|Debt Management") ~ "Public Finance Management",
      str_detect(theme, "Public Administration|Administrative and Civil Service Reform|Institutional Strengthening and Capacity Building") ~ "Public Human Resource Management",
      str_detect(theme, "E-Government, inc. E-services|Data Production, Accessibility, and Use") ~ "GovTech",
      str_detect(theme, "Transparency, Accountability and Good Governance") ~ "Transparency and Accountability",
      T ~ "Other"
    )
  )

wb_documents |> 
  write_csv("wb_documents.csv")

usethis::use_data(wb_documents, overwrite = TRUE)
usethis::use_data(gov_unit, overwrite = TRUE)
