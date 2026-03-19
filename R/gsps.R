#' Left join with national-level data and compute confidence intervals
#'
#' This function takes a dataset and joins it with a national-level dataset
#' on `country_code`. It also brings in the national mean, then computes
#' the upper and lower bounds (max and min of `mean`) within each country group.
#'
#' @param data A data frame containing at least a `country_code` column
#'   and a `mean` column.
#' @param national_data A data frame containing `country_code`, `economy_fct`,
#'   and `mean` columns. The `mean` column is renamed to `country_mean` in
#'   the output.
#'
#' @return A tibble with the original `data`, joined with national-level
#'   information (`economy_fct`, `country_mean`), and two additional columns:
#'   \describe{
#'     \item{upper_ci}{The maximum value of `mean` for each `country_code`}
#'     \item{lower_ci}{The minimum value of `mean` for each `country_code`}
#'   }
#'
#' @importFrom dplyr left_join select group_by mutate
#' @export
left_join_national <- function(data, national_data){
  data |>
    left_join(
      {{national_data}} |>
        select(country_code, economy_fct, country_mean = mean),
      by = c("country_code")
    ) |>
    group_by(
      country_code
    ) |>
    mutate(
      upper_ci = max(mean),
      lower_ci = min(mean)
    )
}