#' Prepare benchmark indicator data for plotting
#'
#' Filters institutional capacity indicators by family, scales scores to 0-100,
#' computes group and overall means, and orders data by performance (descending).
#'
#' @param data Data frame with columns: \code{family_name}, \code{score} (0-1 scale),
#'   \code{var_name}, \code{country_code}, and a grouping variable.
#' @param family_name_value Character. Institutional family to filter (e.g.,
#'   "Public Human Resource Management Institutions").
#' @param select_var_name Optional character vector. Indicator names to include.
#'   If NULL (default), all indicators in the family are used.
#' @param group_var Unquoted column name for grouping (e.g., \code{income_group},
#'   \code{region}).
#'
#' @return Named list with six elements:
#'   \describe{
#'     \item{\code{plot_data}}{Main data with scores (0-100), ordered factors, and y-positions}
#'     \item{\code{group_means}}{Group means by indicator (\code{xbar})}
#'     \item{\code{indicator_means}}{Overall means by indicator (\code{xbar_ind})}
#'     \item{\code{labels}}{Country labels (excludes NAs)}
#'     \item{\code{y_levels}}{Ordered indicator names (high to low performance)}
#'     \item{\code{group_var}}{Grouping variable name (character)}
#'   }
#'
#' @examples
#' \dontrun{
#' hrm_data <- prep_benchmark_data(
#'   data = indicator_wide_scores,
#'   family_name_value = "Public Human Resource Management Institutions",
#'   group_var = income_group
#' )
#'
#' plot_benchmark(hrm_data)
#' }
#'
#' @export
prep_benchmark_data <- function(
  data,
  family_name_value,
  select_var_name = NULL,
  group_var
) {
  # Filter and scale scores
  df <- data |>
    filter(family_name == family_name_value) |>
    mutate(score_100 = score * 100)

  # Add a filter for selecting indicators
  if (!is.null(select_var_name)) {
    df <- df |>
      filter(
        var_name %in% select_var_name
      )
  }

  # Create initial ordered factor and numeric y position (temporary ordering)
  df_plot <- df |>
    mutate(
      var_name = fct_reorder(var_name, score_100, .fun = median, na.rm = TRUE),
      y = as.numeric(var_name)
    )

  # Group means (by income_group or other grouping variable)
  group_var_sym <- rlang::ensym(group_var)

  df_group_mean <- df_plot |>
    group_by(var_name, !!group_var_sym) |>
    summarise(
      xbar = ifelse(
        all(is.na(score_100)),
        NA_real_,
        mean(score_100, na.rm = TRUE)
      ),
      y = first(y),
      .groups = "drop"
    )

  # Overall indicator means
  df_indicator_mean <- df_plot |>
    group_by(var_name) |>
    summarise(
      xbar_ind = ifelse(
        all(is.na(score_100)),
        NA_real_,
        mean(score_100, na.rm = TRUE)
      ),
      y = first(y),
      .groups = "drop"
    )

  # Order the grouping variable factor levels by mean score (descending)
  group_order <- df_group_mean |>
    group_by(!!group_var_sym) |>
    summarise(overall_mean = mean(xbar, na.rm = TRUE), .groups = "drop") |>
    arrange(desc(overall_mean)) |>
    pull(!!group_var_sym)

  # Reorder var_name by overall mean xbar across all groups (descending)
  var_name_order <- df_group_mean |>
    group_by(var_name) |>
    summarise(indicator_mean = mean(xbar, na.rm = TRUE), .groups = "drop") |>
    arrange(desc(indicator_mean)) |>
    pull(var_name)

  # Apply ordering to all data frames
  df_plot <- df_plot |>
    mutate(
      !!group_var_sym := factor(!!group_var_sym, levels = group_order),
      var_name = factor(var_name, levels = var_name_order),
      y = as.numeric(var_name)
    )

  df_group_mean <- df_group_mean |>
    mutate(
      !!group_var_sym := factor(!!group_var_sym, levels = group_order),
      var_name = factor(var_name, levels = var_name_order),
      y = as.numeric(var_name)
    )

  df_indicator_mean <- df_indicator_mean |>
    mutate(
      var_name = factor(var_name, levels = var_name_order),
      y = as.numeric(var_name)
    )

  # Labels data
  df_labels <- df_plot |>
    filter(
      is.finite(score_100),
      !is.na(y),
      !is.na(country_code),
      !is.na(!!group_var_sym)
    )

  # Return list with all components
  list(
    plot_data = df_plot,
    group_means = df_group_mean,
    indicator_means = df_indicator_mean,
    labels = df_labels,
    y_levels = var_name_order,
    group_var = rlang::as_string(group_var_sym)
  )
}

#' Plot benchmark indicator scores with group means (faceted)
#'
#' Creates a faceted horizontal plot showing country scores (grey segments),
#' group means (colored dots), and overall means (grey triangles) for each 
#' institutional capacity indicator.
#'
#' @param data List returned by \code{\link{prep_benchmark_data}} with components:
#'   \code{plot_data}, \code{group_means}, \code{indicator_means}, \code{labels},
#'   \code{y_levels}, \code{group_var}.
#' @param legend_title Character. Legend title. Default: "Income Group".
#' @param geom_type Character. Plot type: "label" for country labels, "point" for
#'   jittered points. Default: "point".
#' @param country_label_size Numeric. Size of country code labels. Default: 3.5.
#' @param facet_ncol Integer. Number of columns for faceting. Default: 1.
#'
#' @return A ggplot object with facets for each indicator.
#'
#' @examples
#' \dontrun{
#' hrm_data <- prep_benchmark_data(
#'   data = indicator_wide_scores,
#'   family_name_value = "Public Human Resource Management Institutions",
#'   group_var = income_group
#' )
#'
#' plot_benchmark(hrm_data)
#' plot_benchmark(hrm_data, geom_type = "label")
#' plot_benchmark(hrm_data, geom_type = "point")
#' }
#'
#' @seealso \code{\link{prep_benchmark_data}}
#'
#' @importFrom ggthemes scale_color_solarized
#' @import ggplot2
#'
#' @export
plot_benchmark <- function(
  data,
  legend_title = "Income Group",
  geom_type = "point",
  country_label_size = 3.5,
  facet_ncol = 1
) {
  # Validate input
  required_names <- c(
    "plot_data",
    "group_means",
    "indicator_means",
    "labels",
    "y_levels",
    "group_var"
  )
  if (!all(required_names %in% names(data))) {
    stop("Input data must contain: ", paste(required_names, collapse = ", "))
  }

  # Create plot with facets
  p <- ggplot(data$plot_data, aes(x = score_100, y = 1)) +

    # Country segments (0 -> score)
    geom_segment(
      aes(x = 0, xend = score_100, y = 1, yend = 1),
      color = "grey90",
      linewidth = 1,
      na.rm = TRUE
    ) +

    # Thresholds
    geom_vline(
      xintercept = c(0, 25, 50, 75, 100),
      color = "grey90",
      linetype = "dotted",
      linewidth = 1
    ) +

    # Overall indicator mean triangle (shape 17)
    geom_point(
      data = data$indicator_means,
      aes(x = xbar_ind, y = 1),
      shape = 17,
      size = 7,
      fill = "grey50",
      color = "grey50",
      show.legend = FALSE,
      na.rm = TRUE
    ) +

    # Group mean dot per indicator
    geom_point(
      data = data$group_means,
      aes(x = xbar, y = 1, color = .data[[data$group_var]]),
      shape = 18,
      size = 10,
      stroke = 1,
      na.rm = TRUE
    ) +

    # Facet by indicator
    facet_wrap(
      ~ var_name,
      ncol = facet_ncol,
      scales = "free_y",
      labeller = label_wrap_gen(width = 40)
    ) +

    # Scales
    ggthemes::scale_color_solarized(
      name = legend_title
    ) +
    scale_x_continuous(breaks = seq(0, 100, 25), limits = c(0, 100)) +
    scale_y_continuous(breaks = NULL) +

    # Labels
    labs(
      x = "CTF score (0-100)",
      y = NULL
    ) +

    # Theme
    theme(
      panel.grid = element_blank(),
      legend.position = "top",
      strip.text = element_text(face = "bold", hjust = 0)
    )

  # Add country labels and/or points based on geom_type
  if (geom_type %in% c("point")) {
    p <- p +
      geom_boxplot(
        aes(fill = .data[[data$group_var]]),
        # size = 3.5,
        alpha = 0.4,
        # width = 0.05
      ) +
      ggthemes::scale_fill_solarized(
        name = legend_title
      )
  }
  
  if (geom_type %in% c("label")) {
    p <- p +
      ggrepel::geom_text_repel(
        data = data$labels,
        aes(label = country_code, color = .data[[data$group_var]], y = 1.05),
        size = country_label_size,
        segment.color = NA,
        box.padding = 0.1,
        direction = "y",
        max.overlaps = Inf,
        na.rm = TRUE
      )
  }

  return(p)
}

#' Plot distribution with range, mean, and jittered points
#'
#' Creates a horizontal plot showing the range (min-max), mean, and individual
#' data points for each group, ordered by average value.
#'
#' @param data Data frame containing the grouping variable and outcome variable.
#' @param group_var Character vector. One or more column names for grouping (e.g.,
#'   \code{c("income_group", "region")}). The first variable is used for y-axis
#'   ordering and color.
#' @param outcome_var Character string. Column name for the outcome variable (e.g.,
#'   "mean", "score").
#' @param facet_var Optional character string. Column name for faceting. Default: NULL.
#' @param point_size Numeric. Size of min/max/mean points. Default: 5.
#' @param jitter_height Numeric. Vertical jitter amount. Default: 0.2.
#'
#' @return A ggplot object.
#'
#' @examples
#' \dontrun{
#' # Single group variable
#' plot_distribution_range(
#'   data = my_data,
#'   group_var = "income_group",
#'   outcome_var = "score",
#'   facet_var = "var_name"
#' )
#'
#' # Multiple group variables
#' plot_distribution_range(
#'   data = my_data,
#'   group_var = c("income_group", "var_name"),
#'   outcome_var = "score",
#'   facet_var = "var_name"
#' )
#' }
#'
#' @import ggplot2
#' @importFrom dplyr group_by mutate ungroup across all_of
#' @importFrom ggthemes scale_color_solarized
#' @importFrom stats reorder
#' 
#' @export
plot_distribution_range <- function(
  data,
  group_var,
  outcome_var,
  facet_var = NULL,
  legend_name = NULL
) {
  # Primary group variable drives y-axis ordering and color
  primary_var <- group_var[[1]]

  # Prepare data with summary statistics grouped by all group_var
  plot_data <- data |>
    group_by(across(all_of(group_var))) |>
    mutate(
      average = mean(.data[[outcome_var]], na.rm = TRUE),
      upper = max(.data[[outcome_var]], na.rm = TRUE),
      lower = min(.data[[outcome_var]], na.rm = TRUE)
    ) |>
    ungroup()
  
  # Create plot
  p <- plot_data |>
    ggplot(
      aes(y = stats::reorder(.data[[primary_var]], .data[["average"]]), color = .data[[primary_var]])
    ) +
    geom_linerange(
      aes(xmin = .data[["lower"]], xmax = .data[["upper"]])
    ) +
    geom_point(
      aes(x = .data[["upper"]]),
      size = 2
    ) +
    geom_point(
      aes(x = .data[["lower"]]),
      size = 2
    ) +
    geom_point(
      aes(x = .data[["average"]]),
      shape = 15,
      size = 8
    ) +
    geom_jitter(
      aes(x = .data[[outcome_var]]),
      height = 0.4,
      size = 5,
      width = 0,
      shape = 1
    ) +
    ggthemes::scale_color_solarized(
      name = legend_name
    ) +
    labs(
      x = "Benchmarking score",
      y = NULL
    ) +
    guides(
      color = guide_legend(nrow = 2)
    ) +
    theme(
      legend.position = "bottom",
      legend.text = element_text(size = 18),
      axis.text.y = element_blank(),
      strip.text.x = element_text(size = 24)
    )

  if (!is.null(facet_var)) {
    p <- p +
      facet_wrap(
        vars(.data[[facet_var]]),
        ncol = 1,
        scales = "free_y",
        labeller = label_wrap_gen(width = 40)
      )
  }
  
  return(p)
}