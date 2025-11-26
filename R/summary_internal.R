#' Compute the Coefficient of Variation (CV)
#'
#' Calculates the coefficient of variation for a numeric vector, defined as the
#' ratio of the standard deviation to the mean. This provides a unitless measure
#' of relative dispersion, allowing comparison of variability across variables
#' or groups with different scales.
#'
#' @param x A numeric vector of values.
#' @param na.rm Logical; if `TRUE`, missing values (`NA`) are removed before
#' computation. Defaults to `TRUE`.
#'
#' @details
#' The function safely handles missing values and cases where the mean is zero
#' (to avoid division by zero). If the input vector is empty or the mean equals
#' zero, the function returns `NA_real_`.
#'
#' @return A numeric value representing the coefficient of variation (CV).
#' Returns `NA_real_` if the computation is not possible (e.g., all values are
#' missing or the mean is zero).
#'
#' @examples
#' x <- c(10, 12, 8, 15, NA)
#' cv(x)
#'
#' @export
cv <- function(x, na.rm = TRUE) {

  ## handling missing values
  if (na.rm) {
    x <- x[!is.na(x)]
  }

  ## compute the vector mean
  m <- mean(x)

  ## numerator-denominator handling to avoid dividing by 0
  if (length(x) == 0 || is.na(m) || m == 0) {

    return(NA_real_)

  }

  y <- sd(x) / m

  return(y)

}

#' Compute a Compression Ratio Between Two Percentiles
#'
#' Calculates a wage (or value) compression ratio by dividing one quantile by
#' another, typically the 90th percentile divided by the 10th percentile.
#' This provides a measure of wage inequality or spread within a group.
#'
#' @param x A numeric vector of values (e.g., wages or salaries).
#' @param upper Numeric; the upper percentile to compute (default is `0.9` for
#' the 90th percentile).
#' @param lower Numeric; the lower percentile to compute (default is `0.1` for
#' the 10th percentile).
#' @param na.rm Logical; if `TRUE`, missing values (`NA`) are removed before
#' computation. Defaults to `TRUE`.
#'
#' @details
#' The function computes the specified upper and lower quantiles and returns
#' their ratio (`upper / lower`). If either quantile is `NA` or the lower
#' quantile is zero, the function returns `NA_real_`.
#'
#' @return A numeric value representing the ratio of the specified upper to
#' lower percentile values. Returns `NA_real_` if computation is not possible
#' (e.g., due to missing data or zero denominator).
#'
#' @examples
#' wages <- c(1000, 1200, 900, 3000, 5000, NA)
#' cp_ratio(wages)             # default 90/10 ratio
#' cp_ratio(wages, 0.75, 0.25) # 75/25 ratio
#'
#' @export
cp_ratio <- function(x, upper = 0.9, lower = 0.1, na.rm = TRUE){

  if (na.rm) {
    x <- x[!is.na(x)]
  }

  q <- quantile(x, probs = c(lower, upper), na.rm = na.rm)

  if (any(is.na(q)) || q[1] == 0) return(NA_real_)

  ratio <- q[2] / q[1]

  return(ratio)

}



prop <- function(x) {
  # Count nonmissing values of x within each group
  tbl <- data.table::data.table(val = x)[, .N, by = val]
  tbl[, prop := N / sum(N)]

  # Map back to the original vector
  tbl$prop[match(x, tbl$val)]
}


#' Count Unique Non-Missing Values
#'
#' Returns the number of unique values in a vector, excluding missing values (NA).
#'
#' @param x A vector of any type (numeric, character, factor, etc.)
#'
#' @return An integer representing the count of unique non-missing values in `x`.
#'
#' @examples
#' # Basic usage
#' count_unique(c(1, 2, 2, 3, 3, 3))
#'
#' # With missing values
#' count_unique(c(1, 2, NA, 2, 3, NA))
#'
#' # With character vector
#' count_unique(c("a", "b", "a", "c"))
#'
#' # Empty vector
#' count_unique(c())
#'
#' # All NA values
#' count_unique(c(NA, NA, NA))
#'
#' @export
count_unique <- function(x){
  x <- x[!is.na(x)]
  y <- length(unique(x))
  return(y)
}

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
#' @seealso [compute_summary()]
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

#' Apply multiple functions to multiple variables (data.table compatible)
#'
#' @description
#' A lightweight helper function that applies a list of functions to a set of variables,
#' mimicking the behavior of `dplyr::summarise_at()`, but designed for use within
#' `data.table` syntax. This is useful when you want to compute several summary
#' statistics (e.g., mean, sd) over a subset of columns defined by `.SD` and
#' `.SDcols`.
#'
#' @param var A vector, list, or data frame-like object (e.g., `.SD` inside a
#'   `data.table` call) containing the variables to which the functions should
#'   be applied.
#' @param funs A character vector of function names (e.g., `"mean"`, `"sd"`, `"sum"`)
#'   or a list of function objects to be applied to each element of `var`.
#' @param ... Additional arguments passed to each function call (e.g., `na.rm = TRUE`).
#'
#' @return A named list where each element corresponds to a combination of variable
#'   and function (e.g., `Sepal.Length_mean`, `Sepal.Length_sd`), suitable for
#'   use inside `data.table::lapply()`.
#'
#' @examples
#' library(data.table)
#' iris_dt <- as.data.table(iris)
#'
#' # Compute mean and sd for Sepal variables by Species
#' iris_dt[, lapply_at(.SD, c("mean", "sd"), na.rm = TRUE),
#'         .SDcols = patterns("^Sepal"),
#'         by = Species]
#'
#' @seealso [dplyr::summarise_at()]
#'
#' @export
lapply_at <- function(var, funs, ...) {
  results <- sapply(var, function(var) {
    lapply(funs, do.call, list(var, ...))
  })
  names(results) <- vapply(
    names(var),
    paste,
    funs,
    sep = "_",
    FUN.VALUE = character(length(funs)),
    USE.NAMES = FALSE
  )
  results
}