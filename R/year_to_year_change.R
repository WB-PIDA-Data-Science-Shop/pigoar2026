#' Compute CTF Score Difference Between Two Years
#'
#' Filters a long-format CTF dynamics data frame to two specified years,
#' maps raw family names to display labels via \code{mapping}, averages
#' indicator values to the country x income group x family level, pivots
#' to wide format, and returns the year-over-year difference. Countries
#' missing data for either year are silently dropped.
#'
#' @param data A long-format data frame containing at least the columns:
#'   \code{family_name}, \code{country_group}, \code{year}, \code{value},
#'   \code{country_code}, and \code{income_group}.
#' @param family A single character string giving the **display label** of the
#'   family to return (i.e. a value in \code{mapping}, not a key).
#' @param from_year Integer. The baseline year.
#' @param to_year Integer. The comparison year.
#' @param mapping A named character vector mapping raw \code{family_name}
#'   values (names) to display labels (values). Defaults to
#'   \code{cluster_mapping}.
#'
#' @return A tibble with one row per country, containing columns
#'   \code{country_code}, \code{income_group}, \code{family_name},
#'   \code{value_<from_year>}, \code{value_<to_year>}, \code{from_year},
#'   \code{to_year}, and \code{difference}. Rows where \code{difference}
#'   is \code{NA} are excluded.
#'
#' @importFrom dplyr mutate filter group_by summarise recode
#' @importFrom tidyr pivot_wider
#' @importFrom stringr str_trim
#' @importFrom rlang has_name .data
#' @export
compute_ctf_diff <- function(data, family, from_year, to_year, mapping = cluster_mapping) {
  # Step 1: trim whitespace, keep only mapped families and target years
  filtered <- data |>
    dplyr::mutate(family_name = stringr::str_trim(family_name)) |>
    dplyr::filter(
      family_name %in% names(mapping),
      country_group == 0,
      year          %in% c(from_year, to_year),
      !is.na(value)
    ) |>
    dplyr::mutate(family_name = dplyr::recode(family_name, !!!mapping)) |>
    dplyr::filter(family_name == family)

  # Step 2: average to country x income_group x family x year
  summarised <- filtered |>
    dplyr::group_by(country_code, income_group, family_name, year) |>
    dplyr::summarise(ctf_score = mean(value, na.rm = TRUE), .groups = "drop")

  # Step 3: pivot wide — one column per year
  wide <- tidyr::pivot_wider(
    summarised,
    id_cols      = c(country_code, income_group, family_name),
    names_from   = year,
    values_from  = ctf_score,
    names_prefix = "value_",
    values_fill  = NA_real_
  )

  # Step 4: guarantee both year columns exist even when a year has no data at all
  from_col <- paste0("value_", from_year)
  to_col   <- paste0("value_", to_year)
  if (!rlang::has_name(wide, from_col)) wide[[from_col]] <- NA_real_
  if (!rlang::has_name(wide, to_col))   wide[[to_col]]   <- NA_real_

  # Step 5: compute difference and drop incomplete rows
  wide |>
    dplyr::mutate(
      from_year  = from_year,
      to_year    = to_year,
      difference = .data[[to_col]] - .data[[from_col]]
    ) |>
    dplyr::filter(!is.na(difference))
}


#' Plot Year-over-Year CTF Score Change by Country and Income Group
#'
#' Creates a lollipop chart of CTF score changes between two years for all
#' countries in \code{data}, faceted by income group. Countries within each
#' panel are ordered by the magnitude of change (descending). Both facet
#' panels and the legend follow the order supplied in \code{income_order}.
#' Returns \code{NULL} invisibly if \code{data} contains no rows.
#'
#' @param data A tibble returned by \code{\link{compute_ctf_diff}}, containing
#'   at least \code{country_code}, \code{income_group}, \code{difference},
#'   \code{from_year}, \code{to_year}, and \code{family_name}.
#' @param title Character. Optional plot title. Defaults to an auto-generated
#'   string combining \code{from_year}, \code{to_year}, and \code{family_name}.
#' @param income_order Character vector giving the desired order of income
#'   group levels for both facets and the colour legend. Defaults to
#'   \code{c("High income", "Upper middle income", "Lower middle income",
#'   "Low income")}.
#'
#' @return A \code{ggplot} object, or \code{NULL} invisibly when \code{data}
#'   is empty.
#'
#' @importFrom dplyr filter group_by mutate ungroup
#' @importFrom forcats fct_reorder
#' @importFrom ggplot2 ggplot aes geom_segment geom_point geom_hline
#'   facet_wrap scale_y_continuous labs theme element_text
#' @importFrom ggthemes scale_color_solarized
#' @export
generate_ctf_diff_plot <- function(
    data,
    title        = NULL,
    income_order = c("High income", "Upper middle income", "Lower middle income", "Low income")
) {
  if (nrow(data) == 0) {
    warning("No data available for family: ", unique(data$family_name), " — skipping plot.")
    return(invisible(NULL))
  }

  auto_title <- title %||% paste(
    "Change in CTF Score:",
    unique(data$from_year), "\u2192", unique(data$to_year),
    "|", unique(data$family_name)
  )

  data |>
    dplyr::filter(!is.na(income_group)) |>
    dplyr::mutate(
      income_group = factor(income_group, levels = income_order)
    ) |>
    dplyr::group_by(income_group) |>
    dplyr::mutate(
      country_code = forcats::fct_reorder(country_code, difference, .desc = TRUE)
    ) |>
    dplyr::ungroup() |>
    ggplot(aes(x = country_code, y = difference, color = income_group)) +
    geom_segment(
      aes(xend = country_code, y = 0, yend = difference),
      linewidth = 0.7
    ) +
    geom_point(size = 2, alpha = 0.75) +
    geom_hline(
      yintercept = 0,
      linetype   = "solid",
      linewidth  = 0.5,
      alpha      = 0.6,
      color      = "grey60"
    ) +
    # facet_wrap(~income_group, scales = "free_y") +
    ggthemes::scale_color_solarized(name = "Income Group") +
    scale_y_continuous(
      limits = c(-0.25, 0.25),
      breaks = seq(-0.25, 0.25, by = 0.05)
    ) +
    labs(
      title    = auto_title,
      subtitle = "Shifts in Institutional Benchmarking Scores by Income Group, 2020–2024",
      x        = "Countries (grouped by income level)",
      y        = "Change in Benchmarking Score (0-1)"
    ) +
    theme(
      strip.text      = element_text(size = 12),
      legend.position = "bottom",
      axis.text.x     = element_blank()
    )
}