#' Plot correlation between a numeric outcome and a predictor with regional coloring
#'
#' Creates a scatter plot with points colored by \code{region} and overlays a dashed
#' quadratic polynomial regression line (lm with \code{y ~ poly(x, 2)}). The x-axis
#' is constrained to [0, 1] with ticks every 0.2. Optionally saves the plot to disk.
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
#' - X-axis limits are fixed at [0, 1]; y-axis limits span the range of \code{data[[y]]}.
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