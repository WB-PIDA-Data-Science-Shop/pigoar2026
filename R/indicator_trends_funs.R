#' Prepare data for indicator CTF plot (segments + indicator mean + group mean)
#'
#' @param wide_data Data frame with at least: family_name, score, var_name, country_code, and grouping var.
#' @param family_name_value Character. family_name to filter.
#' @param group_var Grouping column (unquoted). Default: income_group.
#' @param segment_half_height Half height of indicator-mean vertical segment (default 0.5).
#' @return A list with df_plot, df_group_mean, df_indicator_mean, df_labels, y_levels, family_name, group_var_name
#' @export
prep_indicator_data <- function(wide_data,
                                         family_name_value,
                                         group_var = income_group,
                                         segment_half_height = 0.5) {

  group_var_q <- rlang::enquo(group_var)
  group_var_nm <- rlang::as_name(group_var_q)

  df <- indicator_wide_scores |>
    dplyr::filter(.data$family_name == family_name_value) |>
    dplyr::mutate(score_100 = .data$score * 100)

  if (nrow(df) == 0) {
    warning(sprintf("No rows found for family_name == '%s'. Returning NULL.", family_name_value))
    return(NULL)
  }

  df_plot <- df |>
    dplyr::mutate(
      var_name = forcats::fct_reorder(.data$var_name, .data$score_100, .fun = median, na.rm = TRUE),
      y = as.numeric(.data$var_name)
    )

  # group mean per indicator (diamond)
  df_group_mean <- df_plot |>
    dplyr::group_by(.data$var_name, !!group_var_q) |>
    dplyr::summarise(
      xbar = dplyr::if_else(all(is.na(.data$score_100)), NA_real_, mean(.data$score_100, na.rm = TRUE)),
      y    = dplyr::first(.data$y),
      .groups = "drop"
    )

  # overall indicator mean (grey segment)
  df_indicator_mean <- df_plot |>
    dplyr::group_by(.data$var_name) |>
    dplyr::summarise(
      xbar_ind = dplyr::if_else(all(is.na(.data$score_100)), NA_real_, mean(.data$score_100, na.rm = TRUE)),
      y        = dplyr::first(.data$y),
      .groups  = "drop"
    ) |>
    dplyr::mutate(
      ymin = .data$y - segment_half_height,
      ymax = .data$y + segment_half_height
    )

  y_levels <- levels(df_plot$var_name)

  df_labels <- df_plot |>
    dplyr::filter(is.finite(.data$score_100), !is.na(.data$y), !is.na(.data$country_code)) |>
    dplyr::filter(!is.na(!!group_var_q))

  list(
    df_plot = df_plot,
    df_group_mean = df_group_mean,
    df_indicator_mean = df_indicator_mean,
    df_labels = df_labels,
    y_levels = y_levels,
    family_name = family_name_value,
    group_var_name = group_var_nm
  )
}



#' Plot indicator CTF distribution with country segments, indicator mean, and group mean
#'
#' @param wide_data Data frame used for prep.
#' @param family_name_value Character. family_name to plot.
#' @param group_var Grouping column (unquoted). Default: income_group.
#' @param thresholds Numeric vector of vertical threshold lines.
#' @param palette Brewer palette name for grouping colors.
#' @param mean_shape Shape for group means (default 5 = diamond). Use 23 for filled diamond.
#' @param mean_size Size for group mean markers.
#' @param mean_stroke Stroke for group mean markers.
#' @param segment_half_height Half height of indicator-mean vertical segment.
#' @param title Optional plot title (defaults to family name).
#' @param subtitle Optional plot subtitle.
#' @return A ggplot object
#' @export
plot_indicator_ctf <- function(wide_data,
                               family_name_value,
                               group_var = income_group,
                               thresholds = c(0, 25, 50, 75, 100),
                               palette = "Set2",
                               mean_shape = 5,
                               mean_size = 5,
                               mean_stroke = 1,
                               segment_half_height = 0.5,
                               title = NULL,
                               subtitle = "Grey segment is overall indicator mea.; squares are group means") {

  prep <- prep_indicator_ctf_plot_data(
    indicator_wide_scores = indicator_wide_scores,
    family_name_value = family_name_value,
    group_var = {{ group_var }},
    segment_half_height = segment_half_height
  )
  if (is.null(prep)) return(NULL)

  group_var_q <- rlang::enquo(group_var)
  group_label <- prep$group_var_name

  if (is.null(title)) title <- paste0("CTF Scores — ", family_name_value)

  ggplot2::ggplot(prep$df_plot, ggplot2::aes(x = .data$score_100, y = .data$y)) +

    # country segments (0 -> score) per row
    ggplot2::geom_segment(
      ggplot2::aes(x = 0, xend = .data$score_100, y = .data$y, yend = .data$y),
      color = "grey90",
      linewidth = 1,
      na.rm = TRUE
    ) +

    # thresholds
    ggplot2::geom_vline(
      xintercept = thresholds,
      color = "grey90", linetype = "dashed", linewidth = .75
    ) +

    # overall indicator mean segment
    ggplot2::geom_segment(
      data = prep$df_indicator_mean,
      ggplot2::aes(x = .data$xbar_ind, xend = .data$xbar_ind, y = .data$ymin, yend = .data$ymax),
      color = "grey90", linewidth = 3,
      show.legend = FALSE,
      na.rm = TRUE
    ) +

    # group mean marker per indicator
    ggplot2::geom_point(
      data = prep$df_group_mean,
      ggplot2::aes(x = .data$xbar, y = .data$y, color = !!group_var_q),
      shape = mean_shape, size = mean_size, stroke = mean_stroke,
      na.rm = TRUE
    ) +

    # country labels (colored by grouping)
    ggrepel::geom_text_repel(
      data = prep$df_labels,
      ggplot2::aes(label = .data$country_code, color = !!group_var_q),
      size = 2.5,
      segment.color = NA,
      box.padding = 0.25,
      max.overlaps = Inf,
      na.rm = TRUE
    ) +

    ggplot2::scale_color_brewer(palette = palette, name = group_label) +
    ggplot2::scale_y_continuous(
      breaks = seq_along(prep$y_levels),
      labels = \(i) stringr::str_wrap(prep$y_levels[i], width = 15)
    ) +
    ggplot2::scale_x_continuous(breaks = seq(0, 100, 25), limits = c(0, 100)) +
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      x = "CTF score (0–100)", y = NULL
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      legend.position = "top"
    )
}
