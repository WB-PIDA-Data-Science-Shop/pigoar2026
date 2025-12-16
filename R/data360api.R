#' This function retrieves data from the Data360 API.
#'
#' @return A tibble containing data, including country codes, country names,
#'         years, and values.
#' @examples
#' \dontrun{
#'   cpi_data <- get_data360_api("WB_WDI", "WB_WDI_FP_CPI_TOTL")
#'   head(cpi_data)
#' }
#'
#' @param dataset_id Unique identifier for the database.
#' @param indicator_id Indicator ID. If a vector of indicators is provided, the query retrieves all indicators.
#' @param pivot Whether or not to pivot the extracted data. Default is true.
#'
#' @import httr
#' @import dplyr
#' @importFrom janitor clean_names
#' @importFrom jsonlite fromJSON
#' @importFrom tibble as_tibble
#' @export
get_data360_api <- function(dataset_id, indicator_id, pivot = TRUE) {
  base_url <- "https://data360api.worldbank.org/data360/data"

  indicator_id <- paste0(
    indicator_id,
    collapse = ","
  )

  modified_url <- httr::modify_url(
    base_url,
    query = list(
      DATABASE_ID = dataset_id,
      INDICATOR = indicator_id,
      skip = 0
    )
  )

  res <- httr::GET(modified_url)
  httr::stop_for_status(res)  # error if request fails

  data_json <- httr::content(res, as = "text", encoding = "UTF-8")
  data_list <- jsonlite::fromJSON(data_json)
  count <- data_list$count

  skip_row <- 0
  nrow_data_360 <- 0
  data_360 <- tibble()

  while(nrow_data_360 < count){
    data_url <- httr::modify_url(
      url = modified_url,
      query = list(skip = skip_row)
    )

    data_res <- httr::GET(data_url)
    httr::stop_for_status(res)  # error if request fails

    data_json <- httr::content(data_res, as = "text", encoding = "UTF-8")
    data_list <- jsonlite::fromJSON(data_json)

    data_360 <- data_360 |>
      bind_rows(
        data_list$value
      )

    nrow_data_360 <- nrow(data_360)
    skip_row <- skip_row + 1000
  }

  if(pivot){
    data_360 <- data_360 |>
      pivot_data360()
  }

  return(data_360)
}

#' Pivot Data360 dataset to wide format
#'
#' This function reshapes a Data360-style dataset from long to wide format.
#' It spreads indicator values (`OBS_VALUE`) across multiple columns based on
#' the indicator variable (`INDICATOR`), while keeping the reference area and
#' time period identifiers. The resulting dataset is renamed and cleaned to
#' have snake_case variable names.
#'
#' @param data A data frame or tibble containing the variables:
#'   - `REF_AREA`: Country or region code
#'   - `TIME_PERIOD`: Year or time reference
#'   - `INDICATOR`: Indicator code or name
#'   - `OBS_VALUE`: Observation value for the indicator
#'
#' @return A tibble in wide format with columns:
#'   - `country_code`: The country or region code (from `REF_AREA`)
#'   - `year`: The year or time period (from `TIME_PERIOD`)
#'   - One column per unique `INDICATOR`, containing corresponding values from `OBS_VALUE`
#'
#'
#' @examples
#' library(dplyr)
#' library(tidyr)
#'
#' data_long <- tibble(
#'   REF_AREA = c("USA", "USA", "CAN", "CAN"),
#'   TIME_PERIOD = c(2020, 2020, 2020, 2020),
#'   INDICATOR = c("GDP", "POP", "GDP", "POP"),
#'   OBS_VALUE = c(21000, 330, 1800, 38)
#' )
#'
#' data_wide <- pivot_data360(data_long)
#' print(data_wide)
#'
#' @importFrom tidyr pivot_wider
#' @importFrom dplyr select everything
#' @importFrom janitor clean_names
#'
#' @export
pivot_data360 <- function(data){
  data_pivot <- data |>
    pivot_wider(
      id_cols = c(.data[["REF_AREA"]], .data[["TIME_PERIOD"]]),
      values_from = .data[["OBS_VALUE"]],
      names_from = .data[["INDICATOR"]]
    ) |>
    select(
      country_code = .data[["REF_AREA"]],
      year = .data[["TIME_PERIOD"]],
      everything()
    ) |>
    janitor::clean_names()

  return(data_pivot)
}