## code to prepare `wb_documents` dataset goes here
library(dplyr)

skip_rows <- 0
n_rows <- 1000
nrow_docs <- 1000
documents_tbl <- tibble()

while(nrow_docs == n_rows){
  resp <- httr2::request("https://search.worldbank.org/api/v3/wds") |>
    httr2::req_url_query(
      format = "json",
      fl = "id,count,abstracts,authr,docdt,origu,owner,projectid,theme,topic,docty",
      strdate = "2025-01-01",
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
      \(doc){
        author <- doc |> 
          purrr::pluck("authors", .default = NULL)

        doc$authors <- author |> 
          purrr::map_chr(
            \(au) au |> 
              pluck("author", .default = as.character(au))
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
  filter(
    grepl("GOV|FM|Proc|Inst", owner) &
      grepl("^efi|^prosperity", owner, ignore.case = TRUE)
  ) |> 
  mutate(
    gov_unit = 1
  )

wb_documents <- wb_documents |> 
  left_join(
    gov_unit,
    by = "owner"
  ) |> 
  mutate(
    gov_unit = if_else(
      is.na(gov_unit), 0, gov_unit
    )
  )

usethis::use_data(wb_documents)