#' Generate a Min/Average/Max Labeled Plot by Region
#'
#' Creates a plot with labels for a specific dimension, with shaded
#' capacity bands (Weak / Emergent / Strong) in the background.
#'
#' @param data A data frame containing the input data.
#' @param dimension The dimension identifier used to filter the data
#'   (e.g. "vars_pol_avg"). Must match the `dimension` column in `data`.
#' @return A ggplot object.
#'
#' @importFrom dplyr filter mutate
#' @importFrom ggplot2 ggplot aes geom_point geom_segment geom_rect
#'   scale_shape_manual scale_fill_manual scale_y_continuous theme element_text
#' @importFrom ggrepel geom_text_repel
#' @export
generate_capacity_levels_plot <- function(data, dimension) {

  # --------------------------------------------------------------------------
  # 1. Define capacity bands (become horizontal stripes in the background)
  # --------------------------------------------------------------------------
  capacity_levels <- c(
    "Weak (0-0.24)",
    "Emergent (0.25-0.49)",
    "Strong (0.50-1.00)"
  )

  capacity_colors <- c(
    "Weak (0-0.24)"        = "red3",
    "Emergent (0.25-0.49)" = "goldenrod",
    "Strong (0.50-1.00)"   = "darkgreen"
  )

  bands <- tibble::tibble(
    ymin = c(0.00, 0.25, 0.50),
    ymax = c(0.24, 0.49, 1.00),
    band = factor(capacity_levels, levels = capacity_levels)
  )

  # --------------------------------------------------------------------------
  # 2. Filter and prep data for the selected dimension
  # --------------------------------------------------------------------------
  filtered_data <- data |>
    dplyr::filter(dimension == !!dimension) |>
    dplyr::mutate(
      region_w = wrap_facet_titles(region_long, width = 15)
    )

  # --------------------------------------------------------------------------
  # 3. Build plot with shaded bands + min/avg/max structure
  # --------------------------------------------------------------------------
  ggplot2::ggplot(
    filtered_data,
    ggplot2::aes(
      x     = region_w,
      y     = value,
      shape = type,
      color = "white" #New
    )
  ) +
    # Background capacity bands (Weak / Emergent / Strong)
    ggplot2::geom_rect(
      data        = bands,
      ggplot2::aes(
        ymin  = ymin,
        ymax  = ymax,
        xmin  = -Inf,
        xmax  = Inf,
        fill  = band
      ),
      inherit.aes = FALSE,
      alpha       = 0.5
    ) +

    # Vertical segment from min to max values
    ggplot2::geom_segment(
      ggplot2::aes(
        x    = region_w,
        xend = region_w,
        y    = min_value,
        yend = max_value
      ),
      linewidth = 1.5
    ) +

    # Points for Min / Avg / Max
    ggplot2::geom_point(size = 8) +

    # Country code labels for the average
    ggrepel::geom_text_repel(
      ggplot2::aes(
        x     = region_w,
        y     = country_dimension_av,
        label = country_code
      ),
      data          = filtered_data |> dplyr::filter(type == "Average"),
      size          = 4,
      nudge_x       = -0.3,
      segment.color = NA,
      box.padding   = 0.15
    ) +

    # Custom shapes (filled circles for Min/Max, triangle for Avg)
    ggplot2::scale_shape_manual(
      values = c(
        "Min"     = 19,
        "Average" = 24,
        "Max"     = 19
      )
    ) +

    # Y scale fixed to CTF range with band legend
    ggplot2::scale_y_continuous(
      limits = c(0, 1),
      breaks = seq(0, 1, by = 0.25)
    ) +

    # Legend for the shaded CTF bands
    ggplot2::scale_fill_manual(
      name   = "Capacity level (CTF score)",
      values = capacity_colors,
      limits = capacity_levels,
      drop   = FALSE
    ) +

    # Adjust x-axis text
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(hjust = 1)
    )
}
