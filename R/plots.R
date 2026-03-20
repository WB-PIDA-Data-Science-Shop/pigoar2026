#' Plot correlation between a numeric outcome and a predictor with regional coloring
#'
#' Creates a scatter plot with points colored by \code{region} and overlays a dashed
#' quadratic polynomial regression line. Optionally saves the plot to disk.
#'
#' @param data A data frame or tibble containing the variables to plot. Must include
#'   columns named by \code{x}, \code{y}, and a \code{region} column for color mapping.
#' @param x A string naming the predictor column in \code{data}. Used on the x-axis.
#' @param y A string naming the outcome column in \code{data}. Used on the y-axis.
#' @param filename Optional string path to save the plot. If \code{NULL}, the plot
#'   is not saved. Passed to \code{ggplot2::ggsave()}.
#' @param group A string naming the grouping column in \code{data} (e.g., "region")
#'
#' @return A \code{ggplot} object.
#'
#' @details
#' - The smoothing line is a quadratic polynomial fit via \code{method = "lm"}.
#' - Colors use \code{scale_color_brewer(palette = "Paired")}.
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#' library(dplyr)
#'
#' df <- tibble(
#'   region = rep(c("North", "South", "East", "West"), each = 25),
#'   predictor = runif(100),
#'   outcome = 10 + 5 * predictor + rnorm(100)
#' )
#'
#' # Create the plot
#' p <- ggplot_correlation(df, x = "predictor", y = "outcome")
#'
#' # Save the plot
#' ggplot_correlation(df, x = "predictor", y = "outcome", filename = "figs/corr.png")
#' }
#'
#' @import ggplot2
#' @importFrom scales pretty_breaks
#' 
#' @export
ggplot_correlation <- function(data, x, y, group, filename = NULL){
    plot <- data |> 
      ggplot(
        aes(
          y = .data[[y]], 
          x = .data[[x]], 
          color = .data[[group]]
        )
      ) +
      geom_point(size = 4) +
      geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE, color = "black", linetype = "dashed") +
      scale_color_solarized() +
      scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2)) +
      scale_y_continuous(
        breaks = scales::pretty_breaks(n = 5), 
        limits = c(min(data[[y]]), max(data[[y]]))
      ) +
      theme(legend.position = "bottom")
    
    if(!is_null(filename)){
      ggsave(
        plot = plot,
        filename = filename,
        width = 12, 
        height = 12, 
        dpi = 300, 
        bg = "white"
      )
    }
    
    return(plot)
  }

#' Plot indexed event trends by quarter with baseline = 100
#'
#' Creates a faceted line plot showing the number of events over time, indexed
#' to the first quarter (baseline = 100). Events are aggregated by quarter and
#' a grouping variable, then indexed within each group. Useful for comparing
#' relative growth across categories.
#'
#' @param data A data frame or tibble containing the event data. Must include
#'   a \code{week} column (date or date-time) and an \code{events} column (numeric).
#'   Must also include the column named by \code{group}.
#' @param group A string naming the grouping column in \code{data} (e.g., "region",
#'   "income_group"). Used for faceting and color mapping.
#' @param group_name A string for the legend title corresponding to \code{group}.
#' @param facet_group Logical indicating whether to facet the plot by \code{group}.
#'
#' @return A \code{ggplot} object.
#'
#' @examples
#' \dontrun{
#' library(dplyr)
#' library(ggplot2)
#' library(lubridate)
#'
#' # Sample event data
#' events_df <- tibble(
#'   week = seq.Date(as.Date("2023-01-01"), as.Date("2024-12-31"), by = "week"),
#'   events = sample(50:200, length(week), replace = TRUE),
#'   region = sample(c("North", "South", "East", "West"), length(week), replace = TRUE)
#' )
#'
#' # Plot indexed trends by region
#' plot_events_index(events_df, group = "region", group_name = "Region")
#' }
#'
#' @import ggplot2
#' @importFrom lubridate quarter
#' @importFrom ggthemes scale_color_solarized
#' @importFrom ggplot2 label_wrap_gen
#'
#' @export
plot_events_index <- function(data, group, group_name, facet_group = FALSE) {
  plot <- data |>
    mutate(
      quarter = lubridate::quarter(.data[["week"]], type = "date_first")
    ) |>
    compute_summary(
      cols = c("events"),
      fns = "sum",
      groups = c("quarter", group),
      output = "wide"
    ) |>
    filter(
      !is.na(.data[[group]])
    ) |>
    group_by(
      .data[[group]]
    ) |>
    mutate(
      events_index = .data[["events_sum"]] / .data[["events_sum"]][quarter == min(quarter)] * 100
    ) |>
    ungroup() |>
    ggplot(
      aes(x = quarter, y = .data[["events_index"]], color = .data[[group]])
    ) +
    geom_line(
      linewidth = 1.2
    ) +
    geom_hline(
      yintercept = 100,
      linetype = "dashed"
    ) +
    scale_y_continuous(
      limits = c(0, NA)
    ) +
    scale_color_solarized(
      name = group_name
    ) +
    theme(
      legend.position = "bottom"
    ) +
    labs(
        x = "Time",
        y = "Protests (Baseline = 100)"
    )
  
  if(facet_group){
    plot <- plot +
      facet_wrap(
        vars(.data[[group]]),
        labeller = label_wrap_gen(width = 20)
      )
  }

  plot
}

#' Plot quantile-classified jittered points with group means
#'
#' Creates a scatter plot with jittered points colored by quantile classification
#' (Weak/Emerging/Strong), overlaid with group mean points and a global average
#' reference line.
#'
#' @param .data A data frame.
#' @param x Character string. Column name for the x-axis.
#' @param y Character string. Column name for the y-axis.
#' @param quantile_group Character vector. Column name(s) used to compute quantile.
#' @param facet_group Character string. Column name used to label facets. Not faceted if NULL.
#' @param reorder Logical. If TRUE, reorders x-axis labels by mean of \code{y}
#'   (descending).
#'
#' @return A ggplot object with jittered points colored by quantile level, large
#'   orange points for group means, and a dashed global average line.
#'
#' @examples
#' \dontrun{
#' indicator_wide_scores |>
#'   filter(var_name == "Core government systems index (cgsi)") |>
#'   plot_quantile(
#'     x = "income_group",
#'     y = "score",
#'     quantile_group = "indicator",
#'     facet_group = "indicator_name",
#'     reorder_x = TRUE
#'   )
#' }
#'
#' @import ggplot2
#' @importFrom dplyr group_by mutate summarise across all_of between
#' @importFrom forcats fct_reorder
#'
#' @export
plot_quantile <- function(.data, x, y, quantile_group, facet_group = NULL, reorder = FALSE){
  data_quantile <- .data |> 
    group_by(
      across(all_of(quantile_group))
    ) |> 
    mutate(
      quantile_indicator = case_when(
        .data[[y]] < quantile(.data[[y]], c(0.25)) ~ "Weak",
        between(.data[[y]], quantile(.data[[y]], c(0.25)), quantile(.data[[y]], c(0.5))) ~ "Emerging",
        .data[[y]] > quantile(.data[[y]], c(0.5)) ~ "Strong"
      )
    ) |>
    ungroup()

  # Optionally reorder x by mean of y (descending)
  if (reorder) {
    data_quantile <- data_quantile |>
      mutate(
        !!x := fct_reorder(.data[[x]], .data[[y]], .fun = base::mean, .desc = TRUE)
      )
  }

  plot_quantile <- data_quantile |> 
    ggplot(
      aes(x = .data[[x]], y = .data[[y]])
    ) +
    stat_summary(
      aes(group = .data[[x]]),
      fun = mean,
      geom = "point",
      shape = 21,
      size = 12,
      fill = "orange2",
      color = "grey20",
      stroke = 1.5
    ) +
    geom_point(
      aes(color = .data[["quantile_indicator"]]),
      position = position_jitter(seed = 42, width = 0.2),
      shape = 1,
      size = 4,
      stroke = 1,
      alpha = 0.8
    ) +
    scale_color_manual(
      values = c(
        "Weak" = "red",
        "Emerging" = "goldenrod2",
        "Strong" = "forestgreen"
      ),
      name = "Global Level",
      na.value = "grey60"
    ) +
    theme(
      legend.position = "bottom"
    ) +
    labs(x = "", y = "") +
    scale_x_discrete(
      labels = \(x) str_wrap(x, width = 15)
    )

  if(!is.null(facet_group)){
    plot_quantile <- plot_quantile +
      facet_wrap(
        vars(.data[[facet_group]]),
        labeller = label_wrap_gen(width = 40),
        ncol = 1
      ) +
      geom_hline(
        aes(yintercept = .data[[y]]),
        linetype = "dashed",
        linewidth = 0.8,
        color = "grey40",
        data = \(d) d |>
          group_by(across(all_of(facet_group))) |>
          summarise(!!y := mean(.data[[y]], na.rm = TRUE), .groups = "drop")
      ) +
      geom_text(
        aes(x = Inf, y = .data[[y]], label = "Global average"),
        hjust = 1.1,
        vjust = -0.5,
        size = 6,
        color = "grey40",
        inherit.aes = FALSE,
        data = \(d) d |>
          group_by(across(all_of(facet_group))) |>
          summarise(!!y := mean(.data[[y]], na.rm = TRUE), .groups = "drop")
      )
  }else{
    plot_quantile <- plot_quantile +
      geom_hline(
        aes(yintercept = .data[[y]]),
        linetype = "dashed",
        linewidth = 0.8,
        color = "grey40",
        data = \(d) d |>
          group_by(across(all_of(quantile_group))) |>
          summarise(!!y := mean(.data[[y]], na.rm = TRUE), .groups = "drop")
      ) +
      geom_text(
        aes(x = Inf, y = .data[[y]], label = "Global average"),
        hjust = 1.1,
        vjust = -0.5,
        size = 6,
        color = "grey40",
        inherit.aes = FALSE,
        data = \(d) d |>
          group_by(across(all_of(quantile_group))) |>
          summarise(!!y := mean(.data[[y]], na.rm = TRUE), .groups = "drop")
      )
  }
  
  plot_quantile
}

#' Plot group mean trends over time as connected points
#'
#' Summarises a numeric outcome by a grouping variable and a time variable (x),
#' then plots group means as points connected by lines. A dashed horizontal
#' reference line is drawn at y = 100. Rows with missing group values are dropped.
#'
#' @param .data A data frame or tibble.
#' @param x A string naming the time/x-axis column in \code{.data}.
#' @param y A string naming the numeric outcome column in \code{.data}.
#' @param group A string naming the grouping column in \code{.data} (e.g.,
#'   \code{"income_group"}). Used for color mapping and faceting.
#'
#' @return A \code{ggplot} object.
#'
#' @examples
#' \dontrun{
#' budget_execution |>
#'   plot_point_line(
#'     x = "year",
#'     y = "budget_execution_rate",
#'     group = "income_group"
#'   )
#' }
#'
#' @import ggplot2
#' @importFrom dplyr group_by summarise filter
#' @importFrom ggthemes scale_color_solarized
#'
#' @export
plot_point_line <- function(.data, x, y, group) {
  .data |>
    group_by(.data[[group]], .data[[x]]) |>
    summarise(
      y_mean = mean(.data[[y]], na.rm = TRUE),
      n = n(),
      .groups = "drop"
    ) |>
    filter(!is.na(.data[[group]])) |>
    ggplot(
      aes(x = .data[[x]], y = .data[["y_mean"]], color = .data[[group]])
    ) +
    geom_point(
      size = 5
    ) +
    geom_line(
      linewidth = 2
    ) +
    geom_hline(
      aes(yintercept = 100),
      linetype = "dashed"
    ) +
    scale_color_solarized(
      name = group
    ) +
    theme(
      legend.position = "bottom"
    ) +
    labs(x = x, y = y)
}