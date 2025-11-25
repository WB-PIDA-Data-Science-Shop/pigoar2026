# set-up ------------------------------------------------------------------
library(haven)
library(dplyr)
library(stringi)
library(here)
library(tidyverse)
library(ggplot2)
library(janitor)
library(reshape2)
library(scales)
library(stringr)
library(readr)
library(readxl)
library(labelled)
library(cowplot)
library(openxlsx)
library(RColorBrewer)
library(countrycode)
library(extrafont)
library(devtools)
library(ggrepel)
library(forcats)
library(patchwork)
library(purrr)

devtools::load_all()

theme_set(
  theme_minimal() +
    theme(
      text = element_text(family = "Segoe UI Semibold"),
      axis.text.x = element_text(size = 14, hjust = .5),
      axis.text.y = element_text(size = 14),
      plot.title = element_text(size = 22, face = "bold"),
      plot.subtitle = element_text(size = 16),
      plot.background = element_blank(),
      plot.caption = element_text(hjust = 0, size = 12),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      legend.position = "none"
    )
)


ggsave_db <- partial(
  ggplot2::ggsave,
  bg = "white",
  width = 14,
  height = 9
)


set.seed(101010)

# data-load ---------------------------------------------------------------

prev_ctf_static <- read_dta( # 2024
  here("data-raw", "input", "static_ctf_032525.dta")) |>
  filter(country_group == 0) |>
  filter(region != "North America")  # Exclude North America


ctf_static <- cliaretl::closeness_to_frontier_static |>
  filter(country_group == 0) |>
  filter(region != "North America")  # 2025

country_name <- cliaretl::wb_country_list


# data-prepare ------------------------------------------------------------

prev_avg <- prev_ctf_static |>
  select(1:5, ends_with("_avg")
  ) |>
  rename(
    `Degree of Integrity` = vars_anticorruption_avg,
    `Energy and Enviroment Institutions` = vars_climate_avg,
    `Justice Institutions` = vars_leg_avg,
    `Political Institutions` = vars_pol_avg,
    `Social Institutions` = vars_social_avg,
    `Digital and Data Use` = vars_digital_avg,
    `Justice Institutions` = vars_leg_avg,
    `Transparency Institutions` = vars_transp_avg,
    `Bussines Enviroment` = vars_mkt_avg,
    `Public Financial Management` = vars_pfm_avg,
    `Public Sector Employment` = vars_hrm_avg
  ) |>
  pivot_longer(cols = 6:last_col(),
               names_to = "cluster",
               values_to = "value"
  )


curr_avg <- ctf_static |>
  select(1:5, ends_with("_avg")
  ) |>
  rename(
    `Degree of Integrity` = vars_anticorruption_avg,
    `Energy and Enviroment Institutions` = vars_climate_avg,
    `Justice Institutions` = vars_leg_avg,
    `Political Institutions` = vars_pol_avg,
    `Social Institutions` = vars_social_avg,
    `Digital and Data Use` = vars_digital_avg,
    `Justice Institutions` = vars_leg_avg,
    `Transparency Institutions` = vars_transp_avg,
    `Bussines Enviroment` = vars_mkt_avg,
    `Public Financial Management` = vars_pfm_avg,
    `Public Sector Employment` = vars_hrm_avg
  ) |>
  pivot_longer(cols = 6:last_col(),
               names_to = "cluster",
               values_to = "value"
  )

# merge datasets
merged_avg <-
  prev_avg |>
    select(country_code, region, cluster, value) |>
    rename(prev_value = value) |>
    left_join(
      curr_avg |>
        select(country_code, cluster, value) |>
        rename(curr_value = value),
      by = c("country_code", "cluster")
    )

merged_avg_plot <-
  merged_avg|>
  left_join(
    ctf_static |>
      select(country_name, country_code),
    by = c("country_code")
  ) |>
  select(country_name, country_code, region, cluster, everything()) |>
  pivot_longer(
    cols = 5:6,
    names_to = "ctf_time",
    values_to = "value"
  ) |>
  mutate(
    country_name = if_else(
      is.na(country_name),
      "Viet Nam",
      country_name
    )
  )

regional_avg <- merged_avg_plot |>
  group_by(region, cluster, ctf_time) |>
  summarise(
    reg_value = mean(value, na.rm = TRUE)
  ) |>
  ungroup()

# clean & order once
regional_avg_clean <- regional_avg |>
  filter(!is.na(reg_value)) |>
  mutate(
    ctf_time = factor(ctf_time, levels = c("prev_value", "curr_value"))
  )


# 2. Radar color plot -------------------------------------------------

plot_df <- regional_avg |>
  filter(ctf_time == "curr_value") |>
  mutate(cluster_lab = str_wrap(cluster, 12))

plot_df |>
  ggplot(aes(x = reorder(cluster_lab, reg_value), y = reg_value)) +
  geom_col(aes(fill = cluster_lab), alpha = 0.75, show.legend = FALSE) +
  geom_segment(
    aes(y = 0, yend = 1, xend = cluster_lab, color = cluster_lab),
    linetype = "dashed",
    show.legend = FALSE
  ) +
  coord_polar(direction = 1) +
  scale_y_continuous(
    limits = c(0, 1),
    breaks = seq(0, 1, by = 0.25),
    labels = scales::number_format(accuracy = 0.01) # or percent_format()
  ) +
  facet_wrap(~ region) +
  labs(
    title = "Institutional Capacity overview by Region (2020-2024)",
    subtitle = "Regional Average CTF Scores by Institutional Cluster",
    y = "CTF cluster score",
    x = NULL
  ) +
  theme_minimal() +
  theme(
    # show radial scale labels:
    axis.text.y      = element_text(size = 8),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey90")
  )

ggsave_db(
  here("figures","institutional-capacity-radar-by-region.png")
)


# drafts ------------------------------------------------------------------


################ __family, country change

merged_avg_plot |>
  filter(cluster == "Degree of Integrity", !is.na(value)) |>
  # reorder countries by their (max) value so y-axis is value-ordered
  mutate(
    country_name = reorder(country_name, value, FUN = max)   # largest at top with coord_flip()
  ) |>
  ggplot(aes(x = country_name, y = value, color = as.factor(ctf_time))) +
  geom_segment(aes(xend = country_name, y = 0, yend = value), linewidth = 1) +
  geom_point(size = 2, alpha = 0.6) +
  geom_hline(
    yintercept = 0,
    linetype = "solid",
    linewidth = .5,
    alpha = 0.75,
    color =  "lightgrey"
  ) +
  labs(
    x = "Country by Region",
    y = "Change in CTF Score"
  ) +
  facet_wrap(~region, scales = "free_y", nrow = 2) +
  theme_minimal() +
  theme(
    axis.text.x  = element_text(size = 18, hjust = .5),
    axis.text.y  = element_text(size = 16),
    axis.title.y = element_text(size = 20, face = "bold"),
    axis.title.x = element_text(size = 20, face = "bold"),
    plot.background   = element_blank(),
    plot.caption      = element_text(hjust = 0, size = 10),
    panel.grid.major  = element_blank(),
    panel.grid.minor  = element_blank(),
    panel.border      = element_blank(),
    strip.text        = element_text(size = 20)
  ) +
  scale_y_continuous(limits = c(0, 1)) +
  coord_flip()


######################## __region, family change


# # segment data: one row per region–cluster, colored by whoever is higher
# segment_df <- regional_avg_clean |>
#   group_by(region, cluster) |>
#   summarise(
#     max_value = max(reg_value, na.rm = TRUE),
#     seg_winner = case_when(
#       all(is.na(reg_value))              ~ "tie",
#       dplyr::n_distinct(reg_value) == 1L ~ "tie",  # same prev & curr
#       TRUE                               ~ as.character(ctf_time[which.max(reg_value)])
#     ),
#     .groups = "drop"
#   )
#
# # shared colors
# pal <- c(
#   "prev_value" = "orange1",
#   "curr_value" = "steelblue4",
#   "tie"        = "grey60"
# )
#
# ggplot() +
#   # segments: colored by winner
#   geom_segment(
#     data = segment_df,
#     aes(
#       x    = cluster,
#       xend = cluster,
#       y    = 0,
#       yend = max_value,
#       color = seg_winner
#     ),
#     linewidth = 1
#   ) +
#   # points: colored & shaped by ctf_time
#   geom_point(
#     data  = regional_avg_clean,
#     aes(
#       x     = cluster,
#       y     = reg_value,
#       color = ctf_time,
#       shape = ctf_time
#     ),
#     size  = 5,
#     alpha = 0.6
#   ) +
#   geom_hline(
#     yintercept = 0,
#     linetype   = "solid",
#     linewidth  = .5,
#     alpha      = 0.75,
#     color      = "lightgrey"
#   ) +
#   labs(
#     x = "Country by Region",
#     y = "Change in CTF Score",
#     color = "CTF period",
#     shape = "CTF period"
#   ) +
#   facet_grid(~region, scales = "free_y") +
#   theme_minimal() +
#   theme(
#     axis.text.x   = element_text(size = 18, hjust = .5),
#     axis.text.y   = element_text(size = 16),
#     axis.title.y  = element_text(size = 20, face = "bold"),
#     axis.title.x  = element_text(size = 20, face = "bold"),
#     plot.background  = element_blank(),
#     plot.caption     = element_text(hjust = 0, size = 10),
#     panel.grid.major = element_blank(),
#     panel.grid.minor = element_blank(),
#     panel.border     = element_blank(),
#     strip.text       = element_text(size = 20)
#   ) +
#   scale_y_continuous(limits = c(0, 0.75)) +
#   coord_flip() +
#   scale_shape_manual(values = c(
#     "prev_value" = 0,   # open square
#     "curr_value" = 15   # filled square
#   )) +
#   scale_color_manual(
#     values = pal,
#     breaks = c("prev_value", "curr_value"),
#     name   = "CTF period"
#   )

#_________________

# ## alternative: line plot with arrows
#
# regional_avg |>
#   mutate(ctf_time = as.factor(ctf_time)) |>
#   ggplot(aes(x = reg_value, y = cluster)) +
#   # line with arrow, one "trajectory" per cluster
#   geom_line(
#     aes(group = cluster),
#     arrow = arrow(
#       length = grid::unit(0.30, "cm"),
#       ends   = "last",      # arrow at the latest ctf_time
#       type   = "closed"
#     ),
#     linewidth = 1
#   ) +
#   # points coloured by ctf_time
#   geom_point(aes(colour = ctf_time), size = 5, alpha = 0.5) +
#   geom_vline(
#     xintercept = 0,
#     linetype   = "solid",
#     linewidth  = 0.5,
#     alpha      = 0.75,
#     color      = "lightgrey"
#   ) +
#   labs(
#     x      = "Change in CTF Score",
#     y      = "Cluster",
#     colour = "CTF period"
#   ) +
#   facet_wrap(~region, scales = "free_y", nrow = 2) +
#   theme_minimal() +
#   theme(
#     axis.text.x   = element_text(size = 18, hjust = .5),
#     axis.text.y   = element_text(size = 16),
#     axis.title.y  = element_text(size = 20, face = "bold"),
#     axis.title.x  = element_text(size = 20, face = "bold"),
#     plot.background  = element_blank(),
#     plot.caption     = element_text(hjust = 0, size = 10),
#     panel.grid.major = element_blank(),
#     panel.grid.minor = element_blank(),
#     panel.border     = element_blank(),
#     strip.text       = element_text(size = 20)
#   ) +
#   scale_x_continuous(limits = c(0, 0.75))

#_________________

# plot_semaforo_df <- regional_avg |>
#   filter(ctf_time == max(ctf_time, na.rm = TRUE)) |>
#   mutate(
#     cluster_lab = str_wrap(cluster, 12),
#     strength = case_when(
#       reg_value < 0.25           ~ "Weak (0–0.25)",
#       reg_value < 0.50           ~ "Emergent (0.25–0.50)",
#       reg_value <= 1             ~ "Strong (0.50–1.00)",
#       TRUE                       ~ NA_character_
#     ),
#     strength = factor(
#       strength,
#       levels = c("Weak (0–0.25)",
#                  "Emergent (0.25–0.50)",
#                  "Strong (0.50–1.00)")
#     )
#   )
#
#
# ggplot(plot_df,
#        aes(x = reorder(cluster_lab, reg_value), y = reg_value)) +
#   geom_col(aes(fill = strength), alpha = 0.75, color = "grey20") +
#   geom_segment(
#     aes(y = 0, yend = 1, xend = cluster_lab, color = strength),
#     linetype = "dashed",
#     linewidth = 0.4,
#     show.legend = FALSE
#   ) +
#   coord_polar(direction = 1) +
#   scale_y_continuous(
#     limits = c(0, 1),
#     breaks = seq(0, 1, by = 0.25),
#     labels = scales::number_format(accuracy = 0.01)
#   ) +
#   scale_fill_manual(
#     values = c(
#       "Weak (0–0.25)"        = "red3",
#       "Emergent (0.25–0.50)" = "gold",
#       "Strong (0.50–1.00)"   = "darkgreen"
#     )
#   ) +
#   scale_color_manual(
#     values = c(
#       "Weak (0–0.25)"        = "red3",
#       "Emergent (0.25–0.50)" = "gold",
#       "Strong (0.50–1.00)"   = "darkgreen"
#     )
#   ) +
#   facet_wrap(~ region) +
#   labs(
#     title    = "Institutional Capacity Overview by Region",
#     subtitle = "Regional Average CTF Scores by Institutional Cluster",
#     y        = "CTF cluster score",
#     x        = NULL,
#     fill     = "Institutional Strength"
#   ) +
#   theme_minimal() +
#   theme(
#     axis.text.y      = element_text(size = 8),
#     panel.grid.minor = element_blank()
#   )



