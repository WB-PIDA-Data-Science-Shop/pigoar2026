library(dplyr)
library(forcats)
library(ggplot2)
library(stringr)
library(ggrepel)
library(here)
library(purrr)

# ---- helpers ----
save_plot_safely <- function(p, path, width = 14, height = 9, dpi = 300) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)

  # If you have ggsave_long(), use it; otherwise fall back to ggplot2::ggsave()
  if (exists("ggsave_long") && is.function(get("ggsave_long"))) {
    ggsave_long(path, plot = p, width = width, height = height, dpi = dpi)
  } else {
    ggplot2::ggsave(filename = path, plot = p, width = width, height = height, dpi = dpi)
  }
}

make_family_plot <- function(indicator_wide_scores, family_name_value) {

  df <- indicator_wide_scores |>
    filter(.data$family_name == family_name_value) |>
    mutate(score_100 = .data$score * 100)

  if (nrow(df) == 0) {
    warning(sprintf("No rows found for family_name == '%s' (skipping).", family_name_value))
    return(NULL)
  }

  # Reorder once, reuse levels everywhere
  df_plot <- df |>
    mutate(var_name = fct_reorder(.data$var_name, .data$score_100, .fun = median, na.rm = TRUE)) |>
    mutate(y = as.numeric(.data$var_name))

  # Mean "vline" per indicator × income group
  df_income_mean <- df_plot |>
    group_by(.data$var_name, .data$income_group) |>
    summarise(
      xbar = ifelse(all(is.na(.data$score_100)), NA_real_, mean(.data$score_100, na.rm = TRUE)),
      y    = first(.data$y),
      .groups = "drop"
    ) |>
    mutate(ymin = .data$y - 0.32, ymax = .data$y + 0.32)

  y_levels <- levels(df_plot$var_name)

  ggplot(df_plot, aes(x = .data$score_100, y = .data$y)) +

    # thresholds
    geom_vline(
      xintercept = c(0, 25, 50, 100),
      color = "grey90", linetype = "dashed", linewidth = .75
    ) +

    # mean "vertical bars" per indicator × income group
    geom_segment(
      data = df_income_mean,
      aes(x = .data$xbar, xend = .data$xbar, y = .data$ymin, yend = .data$ymax, color = .data$income_group),
      linewidth = 2, alpha = 0.85,
      na.rm = TRUE
    ) +

    # country points
    geom_point(
      aes(color = .data$income_group),
      position = position_jitter(height = 0.15, width = 0),
      size = 2, alpha = 0.75,
      na.rm = TRUE
    ) +

    # country labels (NO color mapping)
    ggrepel::geom_text_repel(
      aes(label = .data$country_code),
      color = "grey20",
      size = 2.5,
      segment.color = NA,
      box.padding = 0.25,
      max.overlaps = Inf,
      position = position_jitter(height = 0.15, width = 0),
      na.rm = TRUE
    ) +

    scale_color_brewer(palette = "Set2", name = "Income Group") +

    scale_y_continuous(
      breaks = seq_along(y_levels),
      labels = \(i) str_wrap(y_levels[i], width = 15)
    ) +
    scale_x_continuous(breaks = seq(0, 100, 25), limits = c(0, 100)) +
    labs(
      title = paste0("CTF Scores — ", family_name_value),
      subtitle = "Dots are countries; short vertical bars are income-group means within each indicator",
      x = "CTF score (0–100)", y = NULL
    ) +
    theme_minimal(base_size = 11) +
    theme(
      panel.grid.major.y = element_blank(),
      legend.position = "top"
    )
}

# ---- families to plot ----
families <- c(
  "Public Human Resource Management Institutions",
  "Digital and Data Institutions",
  "Degree of Integrity",
  "Transparency and Accountability Institutions",
  "Public Finance Institutions"
)

out_dir <- here("analysis", "figs", "indicators_ctf")

# ---- loop: build + save ----
walk(families, function(fam) {
  p <- make_family_plot(indicator_wide_scores, fam)
  if (is.null(p)) return(invisible(NULL))

  file_stub <- fam |>
    str_to_lower() |>
    str_replace_all("[^a-z0-9]+", "_") |>
    str_replace_all("^_|_$", "")

  out_file <- here(out_dir, paste0(file_stub, "_indicators_mean_text.png"))

  save_plot_safely(p, out_file, width = 14, height = 9, dpi = 300)
})
