#' Compute grouped summary statistics for multiple variables
#'
#' @description
#' A flexible wrapper around `dplyr::summarise()` and `dplyr::across()` that
#' computes one or more summary statistics for a set of numeric variables,
#' optionally grouped by one or more categorical variables. Users can choose
#' from built-in summary functions defined in [define_fns()] or supply their own.
#'
#' @param data A data frame or tibble containing the variables to summarize.
#'
#' @param cols A character vector of variable names to summarize, or a list
#'   with an element `vars` (e.g., `list(vars = c("var1", "var2"))`).
#'
#' @param fns Optional. A specification of the summary functions to apply:
#'   - `NULL` (default): applies all default functions returned by [define_fns()].
#'   - A **character vector** of function names (e.g., `c("mean", "sd")`)
#'     referring to defaults in [define_fns()].
#'   - A **list** of formulas (e.g., `list(mean = ~mean(.x, na.rm = TRUE))`)
#'     or a mix of character names and formulas.
#'
#' @param groups A character vector of grouping variables used for aggregation.
#'
#' @param output Character, either `"long"` or `"wide"`. If `"long"`, reshapes
#'   the results into a tidy format with columns `indicator` and `value`.
#'   Defaults to `"long"`.
#'
#' @return
#' A tibble containing grouped summary statistics.
#' - If `output = "wide"`, each statistic appears as a separate column.
#' - If `output = "long"`, the data are in tidy (long) format with one row per
#'   group-variable-statistic combination.
#'
#' @details
#' This function simplifies grouped summaries by handling:
#' - Flexible function definitions (`fns`) using formula notation `~expr(.x)`.
#' - Integration with user-defined functions via [define_fns()].
#' - Optional long/wide reshaping for tidy analysis workflows.
#'
#' @section Default Functions:
#' The default functions are defined in [define_fns()] and typically include:
#' `sum`, `mean`, `median`, `cv`, and `cp_ratio`. These can be customized or
#' extended by modifying `define_fns()`.
#'
#' @examples
#' df <- tibble::tibble(
#'   country = rep(c("A", "B"), each = 5),
#'   year = rep(2020:2021, times = 5),
#'   income = runif(10, 1000, 2000),
#'   expenditure = runif(10, 500, 1500)
#' )
#'
#'  # Default summaries by country and year
#' compute_summary(data = df,
#'                 cols = c("income", "expenditure"),
#'                 groups = c("country", "year"),
#'                 fns = "mean")
#'
#'  # Select specific functions
#'  compute_summary(data = df,
#'                 cols = c("income"),
#'                 groups = c("country"),
#'                 fns = c("mean", "sd"),
#'                 output = "wide")
#'
#'  # Custom function
#'  compute_summary(df,
#'                 cols = "income",
#'                 groups = "country",
#'                 fns = list("cv",
#'                            q90 = ~quantile(.x, 0.9, na.rm = TRUE)))
#'
#' @seealso [define_fns()], [dplyr::summarise()], [tidyr::pivot_longer()]
#' @importFrom dplyr group_by summarise across all_of n_distinct
#' @importFrom tidyr pivot_longer
#' @importFrom glue glue
#' @importFrom tidyselect matches
#' @importFrom rlang is_formula
#' @export

compute_summary <- function(data,
                            cols,
                            fns = NULL,
                            groups = NULL,
                            output = c("long", "wide")) {

  # --- 0. Match output argument ---
  output <- match.arg(output)

  # --- 1. Define default summary functions ---
  default_fns <- define_fns()

  # --- 2. Resolve which functions to use ---
  if (is.null(fns)) {
    # No functions specified → use all defaults
    selected_fns <- default_fns

  } else if (is.character(fns)) {
    # Character vector → use named defaults
    unknown <- setdiff(fns, names(default_fns))
    if (length(unknown) > 0) {
      stop(glue::glue("Unknown function name(s): {toString(unknown)}"))
    }
    selected_fns <- default_fns[fns]

  } else if (is.list(fns)) {
    # List of formulas or a mix (character names + formulas)
    char_fns <- fns[sapply(fns, is.character)]
    formula_fns <- fns[sapply(fns, rlang::is_formula)]

    # Add default functions where names are characters
    selected_fns <- c(
      default_fns[intersect(unlist(char_fns), names(default_fns))],
      formula_fns
    )
  } else {
    stop("`fns` must be NULL, a character vector, or a list of formulas.")
  }

  # --- 3. Handle cols input (list or character vector) ---
  if (is.list(cols) && !is.null(cols$vars)) {
    summary_vars <- cols$vars
  } else if (is.character(cols)) {
    summary_vars <- cols
  } else {
    stop("`cols` must be a character vector or a list with element `vars`.")
  }

  # --- 4. Identify grouping variables ---
  group_vars <- groups

  # --- 5. Compute summaries ---
  stats_df <-
    data |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) |>
    dplyr::summarise(
      dplyr::across(
        dplyr::all_of(summary_vars),
        selected_fns,
        .names = "{.col}_{.fn}"
      ),
      .groups = "drop"
    )

  # --- 6. Reshape output if requested ---
  if (output == "long") {
    stats_df <- stats_df |>
      tidyr::pivot_longer(
        cols = tidyselect::matches(paste(summary_vars,
                                         collapse = "|")),
        names_to = "indicator",
        values_to = "value"
      )
  }

  return(stats_df)
}