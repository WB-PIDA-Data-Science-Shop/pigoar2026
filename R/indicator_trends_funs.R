

#' Prepare Indicator Data with Group and Overall Means for Plotting
#'
#' This function prepares institutional capacity indicator data for visualization
#' by computing group-level means, overall indicator means, and organizing data
#' for plotting on a horizontal sliding scale plot. It filters data by institutional family,
#' scales scores to 0-100, reorders indicators by overall mean score (descending), 
#' reorders grouping variable by mean performance (descending), and creates
#' separate data frames for different plot layers.
#'
#' @param data A data frame containing indicator scores with the following required columns:
#'   \itemize{
#'     \item \code{family_name}: Name of the institutional capacity family
#'     \item \code{score}: Closeness-to-frontier (CTF) score (0-1 scale)
#'     \item \code{var_name}: Human-readable indicator name
#'     \item \code{country_code}: ISO country code
#'   }
#' @param family_name_value Character string specifying which institutional family
#'   to filter. Examples: "Public Human Resource Management Institutions",
#'   "Digital Institutions", "Degree of Integrity".
#' @param select_var_name Character string. A vector with names of the variables to be selected for analysis.
#' @param group_var Unquoted column name for grouping variable (e.g., \code{income_group},
#'   \code{region}). Used to calculate group-level means and order data.
#'
#' @return A named list with six components:
#'   \describe{
#'     \item{\code{plot_data}}{Main data frame with scaled scores (0-100), reordered
#'       indicator names and grouping variable as factors, and numeric y-positions for plotting.
#'       Indicators are ordered by mean performance (descending). Grouping variable is ordered
#'       by overall performance (descending).}
#'     \item{\code{group_means}}{Data frame with mean scores by indicator and grouping variable,
#'       containing columns \code{var_name}, grouping variable, \code{xbar} (mean score), and \code{y}.
#'       Both factors are ordered by performance (descending).}
#'     \item{\code{indicator_means}}{Data frame with overall mean scores by indicator,
#'       containing \code{var_name}, \code{xbar_ind} (overall mean), and \code{y}.
#'       Indicators are ordered by mean performance (descending).}
#'     \item{\code{labels}}{Filtered data frame for country labels, excluding missing values.
#'       Both indicator and grouping variable factors are ordered.}
#'     \item{\code{y_levels}}{Character vector of indicator names in factor order (high to low performance)}
#'     \item{\code{group_var}}{Character string of the grouping variable name}
#'   }
#'
#' @details
#' The function performs the following transformations:
#' \itemize{
#'   \item Filters data to the specified institutional family
#'   \item Converts scores from 0-1 scale to 0-100 scale
#'   \item Calculates group-level means for each indicator
#'   \item Calculates overall means across all countries for each indicator
#'   \item Reorders indicators by overall mean score (descending - highest at top)
#'   \item Reorders grouping variable levels by overall performance (descending - best first)
#'   \item Updates numeric y-positions based on new ordering
#'   \item Prepares a clean labels data frame for country annotations
#' }
#'
#' Missing values are handled by excluding them from mean calculations and
#' filtering them out of the labels data frame.
#'
#' @examples
#' \dontrun{
#' # Prepare HRM institutional data grouped by income
#' hrm_data <- prep_benchmark_data(
#'   data = indicator_wide_scores,
#'   family_name_value = "Public Human Resource Management Institutions",
#'   group_var = income_group
#' )
#'
#' # Access components
#' hrm_data$plot_data          # Main plotting data
#' hrm_data$group_means        # Income group means (ordered by performance)
#' hrm_data$indicator_means    # Overall indicator means (ordered by performance)
#'
#' # Prepare data grouped by region instead
#' pfm_data <- prep_benchmark_data(
#'   data = indicator_wide_scores,
#'   family_name_value = "Public Finance Institutions",
#'   group_var = region
#' )
#' }
#'
#' @seealso \code{\link{prep_indicator_data}} for a simpler version without means calculation
#'
#' @export
prep_benchmark_data <- function(data, family_name_value, select_var_name = NULL, group_var) {
  
  # Filter and scale scores
  df <- data |>
    filter(family_name == family_name_value) |>
    mutate(score_100 = score * 100)

  # Add a filter for selecting indicators
  if(!is.null(select_var_name)){
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
      xbar = ifelse(all(is.na(score_100)), NA_real_, mean(score_100, na.rm = TRUE)),
      y = first(y),
      .groups = "drop"
    )
  
  # Overall indicator means
  df_indicator_mean <- df_plot |>
    group_by(var_name) |>
    summarise(
      xbar_ind = ifelse(all(is.na(score_100)), NA_real_, mean(score_100, na.rm = TRUE)),
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
    filter(is.finite(score_100), !is.na(y), !is.na(country_code), !is.na(!!group_var_sym))
  
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

#' Plot Indicator Benchmark Means Visualization
#'
#' Creates a horizontal sliding scale visualization displaying individual country 
#' benchmark scores against group-level means (colored dots) and overall benchmark 
#' means (grey triangles) for institutional capacity indicators.
#'
#' @param data A list object returned by \code{\link{prep_benchmark_data}}
#'   containing six named components:
#'   \itemize{
#'     \item \code{plot_data}: Main data frame with benchmark scores and positions (pre-ordered)
#'     \item \code{group_means}: Group-level benchmark mean scores (pre-ordered)
#'     \item \code{indicator_means}: Overall benchmark mean scores (pre-ordered)
#'     \item \code{labels}: Country labels for annotation (pre-ordered)
#'     \item \code{y_levels}: Ordered indicator names (high to low benchmark performance)
#'     \item \code{group_var}: Name of the grouping variable
#'   }
#' @param title Character string for the plot title. If NULL, no title is added.
#' @param subtitle Character string for the plot subtitle. If NULL, uses a default
#'   subtitle describing the benchmark visualization elements.
#' @param color_palette Character string specifying the RColorBrewer palette name.
#'   Default is "Set2".
#' @param legend_title Character string for the legend title. Default is "Income Group".
#' @param y_label_width Integer specifying the character width for wrapping y-axis
#'   labels. Default is 15.
#'
#' @return A ggplot2 object showing the benchmark means visualization.
#'
#' @details
#' The visualization includes:
#' \itemize{
#'   \item Grey horizontal segments showing individual country benchmark scores (0 to score)
#'   \item Vertical dotted lines at 0, 25, 50, 75, and 100 as benchmark reference thresholds
#'   \item Grey triangles indicating overall benchmark mean scores across all countries
#'   \item Colored dots showing group-level benchmark means (e.g., by income group or region)
#'   \item Country code labels colored by grouping variable
#' }
#'
#' Indicators are ordered from highest to lowest benchmark performance (top to bottom),
#' and colors are assigned to groups based on their overall benchmark performance 
#' (best performing group receives the first color in the palette).
#'
#' @examples
#' \dontrun{
#' # Prepare data
#' hrm_data <- prep_benchmark_data(
#'   data = indicator_wide_scores,
#'   family_name_value = "Public Human Resource Management Institutions",
#'   group_var = income_group
#' )
#'
#' # Create plot with default settings
#' plot_benchmark(hrm_data)
#'
#' # Customize plot
#' plot_benchmark(
#'   data = hrm_data,
#'   title = "HRM Institutions Benchmark Performance",
#'   color_palette = "Dark2",
#'   legend_title = "Income Level"
#' )
#' }
#'
#' @seealso \code{\link{prep_benchmark_data}} for preparing the input data
#'
#' @export
plot_benchmark <- function(data,
                                         title = NULL,
                                         color_palette = "Paired",
                                         legend_title = "Income Group",
                                         y_label_width = 15) {
  
  # Validate input
  required_names <- c("plot_data", "group_means", "indicator_means", 
                      "labels", "y_levels", "group_var")
  if (!all(required_names %in% names(data))) {
    stop("Input data must contain: ", paste(required_names, collapse = ", "))
  }
  
  # Create plot (data is already ordered by prep_benchmark_data)
  p <- ggplot(data$plot_data, aes(x = score_100, y = y)) +
    
    # Country segments (0 -> score) per row
    geom_segment(
      aes(x = 0, xend = score_100, y = y, yend = y),
      color = "grey90",
      linewidth = 1,
      na.rm = TRUE
    ) +
    
    # Thresholds
    geom_vline(
      xintercept = c(0, 25, 50, 75, 100),
      color = "grey90", linetype = "dotted", linewidth = 1
    ) +
    
    # Overall indicator mean triangle (shape 17)
    geom_point(
      data = data$indicator_means,
      aes(x = xbar_ind, y = y),
      shape = 17, size = 7,
      fill = "grey50", color = "grey50",
      show.legend = FALSE,
      na.rm = TRUE
    ) +
    
    # Group mean dot per indicator
    geom_point(
      data = data$group_means,
      aes(x = xbar, y = y, color = .data[[data$group_var]]),
      shape = 19, size = 5, stroke = 1,
      na.rm = TRUE
    ) +
    
    # Country code labels
    ggrepel::geom_text_repel(
      data = data$labels,
      aes(label = country_code, color = .data[[data$group_var]]),
      size = 4.5,
      segment.color = NA,
      box.padding = 0.25,
      max.overlaps = Inf,
      na.rm = TRUE
    ) +
    
    # Scales
    scale_color_brewer(palette = color_palette, name = legend_title) +
    scale_y_continuous(
      breaks = seq_along(data$y_levels),
      labels = \(i) str_wrap(data$y_levels[i], width = y_label_width),
      limits = c(0.5, length(data$y_levels) + 0.5)  # Add padding
    ) +
    scale_x_continuous(breaks = seq(0, 100, 25), limits = c(0, 100)) +
    
    # Labels
    labs(
      title = title,
      x = "CTF score (0-100)", 
      y = NULL
    ) +
    
    # Theme
    theme(
      panel.grid = element_blank(),
      legend.position = "top"
    )
  
  return(p)
}