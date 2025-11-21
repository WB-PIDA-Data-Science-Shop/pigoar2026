
#' Define Default Summary Functions
#'
#' @description
#' Creates and returns a named list of default summary functions used
#' throughout the analytics framework (e.g., by [compute_summary()]).
#' Each function is defined as a purrr-style formula (`~`) that operates
#' on a vector `.x` and returns a scalar summary statistic. The returned
#' list can be supplied directly to a summarization pipeline or extended
#' by users with custom functions.
#'
#' @details
#' The returned list contains commonly used descriptive statistics for
#' numeric vectors, including measures of central tendency, dispersion,
#' distribution, and data quality (e.g., share of missing or zero values).
#' Users can extend or override the defaults by appending their own
#' named formulas before passing to [compute_summary()].
#'
#' @return
#' A named list of formula functions suitable for use with
#' `dplyr::across()`, where each element name is the function label
#' and the value is a one-sided formula that computes the summary.
#'
#' @format
#' The list includes the following summary functions:
#' \describe{
#'   \item{sum}{Sum of values, ignoring `NA`s.}
#'   \item{mean}{Arithmetic mean.}
#'   \item{median}{Median value.}
#'   \item{cv}{Coefficient of variation (requires a `cv()` helper).}
#'   \item{cp_ratio}{Custom "cp ratio" statistic (requires a `cp_ratio()` helper).}
#'   \item{var}{Sample variance.}
#'   \item{iqr}{Interquartile range, computed as `diff(range(.x))`.}
#'   \item{min}{Minimum value.}
#'   \item{max}{Maximum value.}
#'   \item{count}{Number of observations.}
#'   \item{count_unique}{Number of distinct (unique) values.}
#'   \item{prop_na}{Proportion of missing (`NA`) values.}
#'   \item{prop_zero}{Proportion of zero values among non-missing data.}
#'   \item{p25}{25th percentile (first quartile).}
#'   \item{p75}{75th percentile (third quartile).}
#'   \item{p90}{90th percentile.}
#'   \item{sd}{Standard deviation.}
#' }
#'
#' @examples
#' # Load the default function set
#' fns <- define_fns()
#'
#' # Inspect available summaries
#' names(fns)
#'
#' # Example usage with compute_summary()
#' compute_summary(
#'   data = tibble::tibble(
#'      country_code = c(rep("A", 100), rep("B", 100)),
#'      gross_salary_lcu = c(
#'       rnorm(100, mean = 1000, sd = 100),
#'       rnorm(100,  mean = 2000, sd = 100)
#'       ),
#'      net_salary_lcu = c(
#'       rnorm(100, mean = 0.7 * 1000, sd = 100),
#'       rnorm(100,  mean = 0.7 * 2000, sd = 100)
#'      )
#'   ),
#'   cols = c("gross_salary_lcu", "net_salary_lcu"),
#'   groups = c("country_code"),
#'   fns = c("mean", "sd", "cv")
#' )
#'
#' @seealso [compute_summary()], [compute_share()]
#' @keywords internal utilities summarization
#' @export

define_fns <- function(){

  # --- 1. Define default summary functions ---
  default_fns <- list(
    sum    = ~sum(.x, na.rm = TRUE),
    mean   = ~mean(.x, na.rm = TRUE),
    median = ~median(.x, na.rm = TRUE),
    cv     = ~cv(.x),
    cp_ratio = ~cp_ratio(.x),
    var = ~var(.x, na.rm = TRUE),
    iqr = ~diff(range(.x, na.rm = TRUE)),
    min = ~min(.x, na.rm = TRUE),
    max = ~max(.x, na.rm = TRUE),
    count = ~length(.x),
    prop = ~prop(.x),
    dtprop = ~.N / sum(.N),
    count_unique = ~count_unique(.x),
    prop_na = ~mean(is.na(.x)),
    prop_zero = ~mean(.x == 0, na.rm = TRUE),
    p25 = ~quantile(.x, 0.25, na.rm = TRUE),
    p75 = ~quantile(.x, 0.75, na.rm = TRUE),
    p90 = ~quantile(.x, 0.9, na.rm = TRUE),
    sd = ~sd(.x, na.rm = TRUE)
  )

  return(default_fns)

}