#' Plot Regional Average Indicators by Family
#'
#' Creates a lollipop chart showing regional average Closeness to Frontier (CTF)
#' scores for indicators within a specified family. The plot facets by region
#' and color-codes indicators by capacity level (Strong, Emergent, Weak).
#' Automatically saves the plot to the figures directory.
#'
#' @param family A character string specifying the family name to filter and plot.
#'   Example: "Justice Institutions"
#' @param data A data frame containing the regional indicator data. Must include
#'   columns: family_name, var_name, avg_score, strength, and region.
#'   Default is `region_indicator_avg`.
#'
#' @return A ggplot2 object displaying the regional indicator overview.
#'   Also saves the plot as a PNG file in the figures directory.
#'
#' @import ggplot2
#' @import dplyr
#' @importFrom scales number_format
#' @importFrom here here
#'
#' @examples
#' # Plot Justice Institutions indicators
#' plot_regional_indicators("Justice Institutions")
#'
#' # Plot another family
#' plot_regional_indicators("Economic Governance")
#'
#' @export
plot_regional_indicators <- function(family, data) {
  
  # Filter data for the specified family
  plot_data <- data |>
    filter(family_name == family)
  
  # Create the plot
  p <- ggplot(plot_data, aes(x = var_name, y = avg_score)) +
    # Grey reference lines on the score axis
    geom_hline(
      yintercept = c(0.25, 0.50, 0.75),
      color = "grey80",
      linetype = "dashed",
      linewidth = 0.4
    ) +
    geom_hline(
      yintercept = c(0, 1),
      color = "grey80",
      linetype = "solid",
      linewidth = 0.5
    ) +
    # Segments
    geom_segment(
      aes(xend = var_name, y = 0, yend = avg_score, color = strength),
      linewidth = 1
    ) +
    # Points at the end
    geom_point(aes(color = strength), size = 3) +
    coord_flip() +
    facet_wrap(~ region, ncol = 1, scales = "free_y") +
    scale_y_continuous(
      limits = c(0, 1),
      breaks = seq(0, 1, by = 0.25),
      labels = number_format(accuracy = 0.01)
    ) +
    scale_color_manual(
      name   = "Capacity level",
      values = c(
        "Strong (0.50–1.00)"   = "darkgreen",
        "Emergent (0.25–0.49)" = "goldenrod",
        "Weak (0–0.24)"        = "red3"
      )
    ) +
    labs(
      title    = "Regional Average Closeness to Frontier Scores by Indicator",
      subtitle = paste0(family, ", Latin America & Caribbean"),
      x        = "Indicator",
      y        = "CTF average score (0–1)",
      caption  = "Source: World Bank Closeness to Frontier Database."
    ) +
    theme_minimal() +
    facet_grid(~ region) +
    theme(
      axis.text.x      = element_text(size = 9),
      axis.text.y      = element_text(size = 9),
      strip.text       = element_text(size = 14, face = "bold"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position  = "bottom"
    )
  
  # Create filename from family name (replace spaces with underscores)
  filename <- paste0("regional_ctf_indicator_", gsub(" ", "_", family), ".png")
  
  # Save the plot
  ggsave_db(
    here("figures", filename)
  )
  
  return(p)
}
