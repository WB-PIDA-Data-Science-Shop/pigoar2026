#' Fetch a single page from World Bank Documents API
#'
#' @param base_url Character. API endpoint URL.
#' @param fl Character. Comma-separated list of fields to return.
#' @param strdate Character. Start date in "YYYY-MM-DD" format.
#' @param enddate Character. End date in "YYYY-MM-DD" format.
#' @param os Integer. Offset (starting row).
#' @param doc_type Character. A vector of document types, to be matched exactly.
#' @param rows Integer. Number of rows to return.
#'
#' @return A list from the parsed JSON response.
#'
#' @importFrom httr2 request req_url_query req_perform resp_body_json
fetch_wb_documents_json <- function(
    base_url = "https://search.worldbank.org/api/v3/wds",
    fl = "id,count,abstracts,authr,docdt,origu,owner,projectid,theme,topic,docty",
    strdate = "2021-01-01",
    enddate = "2025-12-31",
    doc_type = NULL,
    os = 0,
    rows = 200
) {
  if(is.null(doc_type)){
    httr2::request(base_url) |>
      httr2::req_url_query(
        format = "json",
        fl = fl,
        strdate = strdate,
        enddate = enddate,
        os = os,
        rows = rows
      ) |>
      httr2::req_perform() |>
      httr2::resp_body_json()
  }else{
    httr2::request(base_url) |>
      httr2::req_url_query(
        format = "json",
        fl = fl,
        strdate = strdate,
        enddate = enddate,
        os = os,
        rows = rows,
        doc_ty_exact = paste(doc_type, collapse = "\\%E") |> 
          stringr::str_replace_all("\\s", "\\%20") 
      ) |>
      httr2::req_perform() |>
      httr2::resp_body_json()
  }
  
}

#' Extract World Bank API documents from JSON
#'
#' Extracts documents from a World Bank API JSON response, collapses the nested
#' `authors` field to a semicolon-separated string, and returns a tibble with
#' one row per document.
#'
#' @param json A list from `httr2::resp_body_json()`. See \code{\link{fetch_wb_documents_json}}.
#'
#' @return A tibble with one row per document. The `authors` column is a single
#'   character string with names separated by semicolons. Rows with missing `id`
#'   are removed.
#'
#' @importFrom purrr pluck map map_chr
#' @importFrom dplyr bind_rows filter
#'
#' @examples
#' \dontrun{
#' resp_json <- fetch_wb_documents_json(os = 0, rows = 100)
#' docs_tbl <- pluck_wb_documents(resp_json)
#' }
extract_wb_documents <- function(json) {
  documents <- json |>
    purrr::pluck("documents") |>
    purrr::map(\(doc) {
      author <- purrr::pluck(doc, "authors", .default = NULL)
      doc$authors <- if (is.null(author)) {
        NA_character_
      } else {
        purrr::map_chr(author, ~ purrr::pluck(.x, "author", .default = as.character(.x))) |>
          paste(collapse = ";")
      }
      doc
    })

  documents |>
    dplyr::bind_rows() |>
    dplyr::filter(!is.na(id))
}