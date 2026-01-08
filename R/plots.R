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
ggplot_correlation <- function(data, x, y, filename = NULL){
    plot <- data |> 
      ggplot(
        aes(
          y = .data[[y]], 
          x = .data[[x]], 
          color = region
        )
      ) +
      geom_point(size = 2) +
      geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE, color = "black", linetype = "dashed") +
      labs(
        y = y,
        x = paste(x, "(2019-2023)"),
        color = "Region"
      ) +
      scale_color_brewer(palette = "Paired") +
      scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2)) +
      scale_y_continuous(
        breaks = scales::pretty_breaks(n = 5), 
        limits = c(min(data[[y]]), max(data[[y]]))
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 0, hjust = 0.5, size = 18),
        axis.text.y = element_text(size = 18),
        axis.title = element_text(size = 20),
        plot.margin = margin(t = 10, r = 10, b = 15, l = 10),
        legend.position = "bottom",
        legend.title = element_text(face = "bold", size = 16),
        legend.text = element_text(size = 18),
        plot.title = element_text(size = 18),
        plot.subtitle = element_text(size = 16)
      ) +
      ggtitle(
        label = paste0("Correlation between: ", y),
        subtitle = paste("and", x)
      )
    
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
      quarter = lubridate::quarter(week, type = "date_first")
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
      events_index = events_sum / events_sum[quarter == min(quarter)] * 100
    ) |>
    ungroup() |>
    ggplot(
      aes(x = quarter, y = events_index, color = .data[[group]])
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