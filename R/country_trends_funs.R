#' A function to Compute Regional Min/Average/Max Statistics for a Given Cluster
#'
#' @param data A data frame containing the input data.
#' @param dimension The column name (as a string or symbol) representing the dimension to compute statistics for.
#' @return A data frame with computed regional statistics.
#'
#' @import dplyr rlang tidyr countrycode
#' @importFrom rlang ensym
#' @export

compute_regional_statistics <- function(data, dimension) {
  dimension <- rlang::ensym(dimension)

  # Define custom mappings for unmatched country names
  custom_mappings <- c(
    "Channel Islands" = "CHI",
    "Kosovo" = "XKX"
  )

  data |>
    select(country_name, !!dimension, region) |>
    group_by(region) |>
    mutate(
      region_av = ifelse(all(is.na(!!dimension)), NA, mean(!!dimension, na.rm = TRUE)),
      max_av = ifelse(all(is.na(!!dimension)), NA, max(!!dimension, na.rm = TRUE)),
      min_av = ifelse(all(is.na(!!dimension)), NA, min(!!dimension, na.rm = TRUE))
    ) |>
    ungroup() |>
    pivot_longer(
      cols = c(min_av, region_av, max_av),
      names_to = "type",
      values_to = "value"
    ) |>
    mutate(
      type = recode(type,
                    min_av = "Min",
                    region_av = "Average",
                    max_av = "Max")
    ) |>
    left_join(
      data |>
        group_by(region) |>
        summarise(
          min_value = ifelse(all(is.na(!!dimension)), NA, min(!!dimension, na.rm = TRUE)),
          max_value = ifelse(all(is.na(!!dimension)), NA, max(!!dimension, na.rm = TRUE)),
          .groups = "drop"
        ),
      by = "region"
    ) |>
    mutate(
      country_code = countrycode(
        country_name,
        "country.name",
        "iso3c",
        custom_match = custom_mappings
      )
    ) |>
    rename(
      country_dimension_av = !!dimension,
      region_long = region
    )
}


#' A function to wrap graph's text labels
#'
#' @param x object or variable that need to be wrapped
#' @param width number of characters
#'
#' @import stringr
#' @export
wrap_facet_titles <- function(x, width) {
  str_wrap(x, width = width)
}


#' Generate a regional min/average/max plot by dimension
#'
#' Creates a dot-and-segment plot showing the min, average, and max values
#' by region for a given institutional dimension, with country code labels
#' at the average position.
#'
#' @param data Data frame returned by \code{\link{compute_regional_statistics}},
#'   with columns: \code{region_long}, \code{type} ("Min", "Average", "Max"),
#'   \code{value}, \code{min_value}, \code{max_value}, \code{country_dimension_av},
#'   and \code{country_code}.
#'
#' @return A ggplot object with points, segments, and country code labels faceted
#'   by region.
#'
#' @importFrom dplyr filter mutate
#' @importFrom ggplot2 ggplot aes geom_point geom_segment scale_shape_manual theme element_text
#' @importFrom ggrepel geom_text_repel
#' @export
generate_regional_minmax_plot <- function(data) {
  # Prepare data for plotting
  plot_data <- data |>
    mutate(region_w = wrap_facet_titles(region_long, width = 15))

  # Generate the plot
  ggplot(plot_data, aes(x = region_w,
                            y = value,
                            shape = type,
                            color = region_w)) +
    geom_point(size = 8) +

    # Vertical segment from min to max values
    geom_segment(aes(x = region_w,
                     xend = region_w,
                     y = min_value,
                     yend = max_value),
                 linetype = "solid",
                 linewidth = 1.5) +

    # Add country code labels
    geom_text_repel(aes(x = region_w,
                        y = country_dimension_av,
                        label = country_code),
                    data = plot_data |> filter(type == "Average"),
                    size = 4,
                    nudge_x = -0.3,
                    segment.color = NA,
                    box.padding = 0.15,
    ) +

    # Custom shapes for points
    scale_shape_manual(values = c("Min" = 19, "Average" = 24, "Max" = 19)) +

    # Adjust x-axis text to be shifted to the left
    theme(axis.text.x = element_text(hjust = 1))
}


#' A function to Compute Change in Cluster Values Between Two Years for Each Country
#'
#' Computes the difference in values for a specific cluster between two years.
#'
#' @param data A data frame containing the input data.
#' @param cluster_name The name of the cluster to filter.
#' @param from_year The starting year for comparison.
#' @param to_year The ending year for comparison.
#' @return A data frame with the computed differences.
#' @export
compute_cluster_diff <- function(data, cluster_name, from_year, to_year) {
  data |>
    filter(cluster == cluster_name,
           year %in% c(from_year, to_year),
           !is.na(value)) |>
    pivot_wider(
      id_cols = c(country_name, country_code, income_group, lending_category, region),
      names_from = year,
      values_from = value,
      names_prefix = "value_"
    ) |>
    mutate(ctf_distance = .data[[paste0("value_", to_year)]] - .data[[paste0("value_", from_year)]]) |>
    arrange(desc(ctf_distance))
}



#' A function to Plot Regional CTF Score Change by Country
#'
#' Creates a lollipop-style chart of CTF score changes for each country,
#' facetted by region with free y-axis scales. Each country's change (
#' \code{ctf_distance}) is shown as a line from 0 plus a point, with
#' a reference horizontal line at 0. The chart is flipped for readability
#' of country names.
#'
#' @param data A data frame containing the input data.
#' @return A ggplot object showing the regional changes.
#' @export
generate_diff_plot <- function(data){
  data |>
    ggplot(aes(x = country_name, y = ctf_distance, color = as.factor(region))) +
    geom_segment(aes(xend = country_name, y = 0, yend = ctf_distance), linewidth = 1) +
    geom_point(size = 2, alpha = 0.6) +
    geom_hline(yintercept = 0,
               linetype = "solid",
               linewidth = .5,
               alpha = 0.75,
               color =  "lightgrey") +
    labs(
      x = "Country by Region",
      y = "Change in CTF Score"
    ) +
    facet_wrap(~region, scales = "free_y", nrow = 2) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(size = 18, hjust = .5),
      axis.text.y = element_text(size = 16),
      axis.title.y = element_text(size = 20, face = "bold"),
      axis.title.x = element_text(size = 20, face = "bold"),
      plot.background = element_blank(),
      plot.caption = element_text(hjust = 0, size = 10),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      legend.position = "none",
      strip.text = element_text(size = 20)
    ) +
    scale_color_brewer(palette = "Paired") +
    scale_y_continuous(limits = c(-0.4, 0.4), breaks = c(-0.4, 0, 0.4)) +
    coord_flip()
}



#' Compute Year-to-Year Indicator Differences
#'
#' This function takes a long-format data frame of indicator values by country and year,
#' filters it to two specified years, reshapes it to wide format, and then computes the
#' difference in the indicator value between those years for each country. Results are
#' returned sorted by the computed difference in descending order.
#'
#' @param data A data frame containing the input data.
#' @param from_year An integer or numeric scalar specifying the first (baseline) year.
#' @param to_year An integer or numeric scalar specifying the comparison year.
#'
#' @return A tibble in wide format.
#'
#' @export
compute_indicator_diff <- function(data, from_year, to_year) {
  data |>
    filter(year %in% c(from_year, to_year)) |>
    pivot_wider(
      id_cols     = c(country_name, country_code, income_group,
                      lending_category, region, variable, var_name, family_name),
      names_from  = year,
      values_from = value,
      names_prefix= "value_",
      values_fill = NA_real_
    ) |>
    mutate(
      ctf_distance =
        .data[[paste0("value_", to_year)]] -
        .data[[paste0("value_", from_year)]]
    ) |>
    arrange(desc(ctf_distance))
}


#' Plot Budget Execution Ratios by Region
#'
#' @description
#' Filters a BOOST budget execution data frame by year, sector, and budget_line,
#' then returns a flipped execution‐ratio plot with reference bands.
#'
#' @param data
#'   A data.frame or tibble containing at least:
#'   `year` (numeric), `sector`, `budget_line`,
#'   `region`, `execution_ratio`, `country_code`.
#' @param exyear
#'   Year to filter (numeric or a string representing an integer).
#' @param govsector
#'   One value from the `sector` column (e.g. `"Health"`).
#' @param budgetline
#'   One value from the `budget_line` column
#'   (e.g. `"Spending: Capital expenditures in health"`).
#'
#' @return
#'   A ggplot2 object.
#' @importFrom dplyr filter
#' @importFrom ggplot2 ggplot aes geom_rect geom_hline geom_point labs theme_minimal coord_flip theme scale_color_brewer ylim
#' @importFrom ggrepel geom_text_repel
#' @export


plot_budget_execution <- function(data,
                                  exyear,
                                  govsector,
                                  budgetline) {
  # coerce exyear to numeric if needed
  if (is.character(exyear)) {
    exyear2 <- suppressWarnings(as.numeric(exyear))
    if (is.na(exyear2)) {
      stop("`exyear` must be numeric or a string coercible to numeric.")
    }
  } else {
    exyear2 <- exyear
  }

  df <- data %>%
    filter(
      year        == exyear2,
      sector      == govsector,
      budget_line == budgetline
    )

  if (nrow(df) == 0) {
    warning("No data for year=", exyear2,
            ", sector='", govsector,
            "', budget_line='", budgetline, "'.")
  }

  ggplot(df, aes(x = region, y = execution_ratio, color = region)) +
    geom_rect(
      aes(xmin = -Inf, xmax = Inf, ymin = 80, ymax = 120),
      fill = "#d0ece7", alpha = 0.5, inherit.aes = FALSE
    ) +
    geom_hline(
      yintercept = c(50, 150),
      linetype   = "dotted",
      linewidth  = 1.5,
      alpha      = 0.25,
      color      = "darkred"
    ) +
    geom_hline(
      yintercept = 100,
      linetype   = "solid",
      linewidth  = 2,
      alpha      = 0.75,
      color      = "#117a65"
    ) +
    geom_point(size = 2) +
    geom_text_repel(
      aes(label = country_code),
      segment.size = 0,
      size         = 4,
      hjust        = 1
    ) +
    labs(
      x     = "Region",
      y     = "Budget Execution Rate (%)"
    )
}


#' Plot Cluster Appendix Figures (optional)
#'
#' Creates and saves a horizontal bar‐segment plot of `ctf_distance` for each indicator
#' (`var_name`) in the specified cluster (`family_name`), faceted by region. Outputs
#' PNG files into `figures/appendix/<cluster>/`.
#'
#' @param data A data frame or tibble containing at least these columns:
#'   - `family_name`
#'   - `var_name`
#'   - `ctf_distance`
#'   - `region`
#'   - `country_name`
#' @param cluster_ctf A string matching the `family_name` of the cluster to plot.
#' @param year_label A character label (e.g. `"2010 vs 2020"`) to include in the plot title.
#'
#' @return Invisibly returns `NULL`. Side effect: writes one PNG per indicator to disk.
#' @export
plot_cluster_appendix <- function(data, cluster_ctf, year_label) {
  # make a file-system-safe folder name:
  safe_cluster <- gsub("[^A-Za-z0-9]+", "_", cluster_ctf)
  out_dir     <- here("figures", "appendix", safe_cluster)

  # create the directory if needed:
  if (!dir.exists(out_dir)) {
    dir.create(out_dir, recursive = TRUE)
  }

  # subset to that cluster
  cluster_data <- data %>%
    filter(family_name == cluster_ctf)

  # one plot per var_name
  indicators <- unique(cluster_data$var_name)

  for (indicator in indicators) {
    # subset and reorder within each region
    plot_data <- cluster_data %>%
      filter(var_name == indicator) %>%
      group_by(region) %>%
      mutate(
        country_name = fct_reorder(country_name, ctf_distance, .desc = FALSE)
      ) %>%
      ungroup()

    # build the plot
    p <- ggplot(plot_data, aes(
      x     = country_name,
      y     = ctf_distance,
      color = as.factor(region)
    )) +
      geom_segment(aes(xend = country_name, y = 0, yend = ctf_distance),
                   linewidth = 1) +
      geom_point(size = 2, alpha = 0.6) +
      geom_hline(yintercept = 0,
                 linetype = "solid",
                 linewidth = 0.5,
                 alpha = 0.75) +
      labs(
        x        = "Country",
        y        = "Change in CTF Score",
        title    = paste0("Change between ", year_label, " for: ", indicator),
        subtitle = paste("Cluster:", cluster_ctf),
        caption = paste(
          "Source: World Bank CLIAR Dashboard. Analysis by PIGO authors",
          "Includes only IBRD/IDA & Blend countries. Observations shown only where data are available",
          sep = "\n"
        )
      ) +
      facet_wrap(~region, scales = "free_y", nrow = 2) +
      theme_minimal() +
      theme(
        axis.text.x     = element_text(size = 18, angle = 45, hjust = 0.5),
        axis.text.y     = element_text(size = 16, hjust = 1),
        axis.title      = element_text(size = 20, face = "bold"),
        plot.title      = element_text(size = 22, face = "bold", hjust = 0.5),
        plot.subtitle   = element_text(size = 16, face = "bold", hjust = 0.5),
        strip.text      = element_text(size = 18),
        legend.position = "none",
        panel.grid      = element_blank()
      ) +
      scale_color_brewer(palette = "Paired") +
      scale_y_continuous(limits = c(-1, 1), breaks = seq(-1, 1, by = 0.2)) +
      coord_flip()

    # Save under figures/<safe_cluster>/<safe_indicator>.png
    safe_ind <- gsub("[^A-Za-z0-9]+", "_", indicator)
    ggsave(
      filename = file.path(out_dir, paste0(safe_ind, "_indicator.png")),
      plot     = p,
      bg       = "white",
      width    = 20,
      height   = 16,
      dpi      = 300
    )
  }
}

#' Plot CTF dynamic scores over time by income group
#'
#' Creates a line plot of average CTF dynamic scores over time, grouped and colored by income group.
#' Accepts either a single family name or a vector of families.
#'
#' @param data A data frame with columns: `income_group`, `family_name`, `year`, `ctf_score`.
#' @param family Character vector or single string. The indicator family/families to plot
#'   (must match value(s) in `family_name`). If a vector, returns a named list of plots.
#' @param title Character. Plot title. Defaults to the family name. Ignored if `family` is a vector.
#' @param subtitle Character. Plot subtitle. Defaults to "Average CTF dynamic score by income group".
#' @param y_limits Numeric vector of length 2. Y-axis limits. Defaults to c(0, 1).
#'
#' @return A ggplot object if `family` is length 1; otherwise a named list of ggplot objects
#'   with file-safe names.
#' @examples
#' # Single plot
#' plot_ctf_time_trends(plot_dyn, family = "Degree of Integrity")
#'
#' # Multiple plots as named list
#' plots <- plot_ctf_time_trends(plot_dyn, family = c("Degree of Integrity", "HRM"))
#' purrr::iwalk(plots, ~ggsave(paste0(.y, ".png"), .x))
plot_ctf_time_trends <- function(
  data,
  family,
  title = NULL,
  subtitle = "Average CTF dynamic score by income group",
  y_limits = c(0, 1)
) {
  # Helper function to create a single plot
  .make_plot <- function(fam, ttl = NULL) {
    if (is.null(ttl)) ttl <- fam

    income_levels <- c(
      "High income",
      "Upper middle income",
      "Lower middle income",
      "Low income"
    )

    plot_data <- data |>
      filter(family_name == fam) |>
      mutate(
        income_group = factor(income_group, levels = income_levels)
      )

    ggplot(plot_data, aes(year, ctf_score, color = income_group)) +
      geom_line(linewidth = 1.5) +
      geom_point(size = 3) +
      ggthemes::scale_color_solarized(name = "Income Group") +
      scale_x_continuous(breaks = scales::breaks_pretty()) +
      scale_y_continuous(limits = y_limits) +
      labs(
        title    = ttl,
        subtitle = subtitle,
        x        = "Year",
        y        = "CTF Score"
      ) +
      theme(legend.position = "bottom") +
      guides(color = guide_legend(nrow = 1))
  }

  # If family is a vector, return named list of plots
  if (length(family) > 1) {
    plots <- purrr::map(family, .make_plot)
    safe_names <- tolower(family)
    safe_names <- gsub("[^a-z0-9]+", "_", safe_names)
    safe_names <- gsub("_+",         "_", safe_names)
    safe_names <- gsub("^_|_$",      "",  safe_names)
    names(plots) <- safe_names
    return(plots)
  }

  # Single family: return single plot
  .make_plot(family, ttl = title %||% family)
}


#' Plot CTF dynamic scores over time by income group, faceted by indicator
#'
#' Like \code{plot_ctf_time_trends} but shows one panel per indicator (`var_name`)
#' within a given family.
#'
#' @param data A data frame with columns: `income_group`, `family_name`,
#'   `var_name`, `year`, `ctf_score`.
#' @param family Character. Single family name to plot (must match `family_name`).
#' @param title Character. Plot title. Defaults to the family name.
#' @param subtitle Character. Plot subtitle.
#' @param y_limits Numeric vector of length 2. Y-axis limits. Defaults to c(0, 1).
#' @param ncol Integer. Number of columns in the facet grid. Defaults to 3.
#'
#' @return A ggplot object with panels faceted by `var_name`.
#' @export
plot_ctf_time_trends_facet <- function(
  data,
  family,
  title    = family,
  subtitle = "Average CTF dynamic score by income group and indicator",
  y_limits = c(0, 1),
  ncol     = 3
) {
  income_levels <- c(
    "High income",
    "Upper middle income",
    "Lower middle income",
    "Low income"
  )

  plot_data <- data |>
    dplyr::filter(family_name == family) |>
    dplyr::mutate(
      income_group = factor(income_group, levels = income_levels),
      var_name     = stringr::str_wrap(var_name, width = 30)
    )

  ggplot(plot_data, aes(year, ctf_score, color = income_group)) +
    geom_line(linewidth = 1) +
    geom_point(size = 2) +
    facet_wrap(~var_name, ncol = ncol, scales = "free_y") +
    ggthemes::scale_color_solarized(name = "Income Group") +
    scale_x_continuous(breaks = scales::breaks_pretty()) +
    scale_y_continuous(limits = y_limits) +
    labs(
      title    = title,
      subtitle = subtitle,
      x        = "Year",
      y        = "CTF Score"
    ) +
    theme(
      legend.position = "bottom",
      strip.text      = element_text(size = 9)
    ) +
    guides(color = guide_legend(nrow = 1))
}