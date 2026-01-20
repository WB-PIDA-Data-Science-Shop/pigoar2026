# UNPACKING CLUSTERS: INDICATORS SLIDING SCALES



# set-up ------------------------------------------------------------------
library(haven)
library(dplyr)
library(here)
library(tidyverse)
library(ggplot2)
library(janitor)
library(scales)
library(stringr)
library(readr)
library(labelled)
library(cowplot)
library(RColorBrewer)
library(ggrepel)
library(forcats)
library(patchwork)
library(purrr)
library(cliaretl)

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

ggsave_long <- partial(
  ggplot2::ggsave,
  bg = "white",
  width = 10,
  height = 18
)

options(ggrepel.max.overlaps = Inf)

set.seed(101010)


# data-load ---------------------------------------------------------------

ctf_static <- cliaretl::closeness_to_frontier_static |>
  filter(country_group == 0)

dictionary <- cliaretl::db_variables



# prepare data ------------------------------------------------------------

#Pivot ctf static
ctf_static_long <- ctf_static |>
  pivot_longer(
    cols = (6:last_col()),
    names_to = "indicator",
    values_to = "score"
  )

# Add families to inticators
ctf_static_fam <- ctf_static_long |>
  left_join(
    dictionary |>
      select(variable,
             var_name,
             family_var,
             family_name,
             benchmark_static_family_aggregate_download),
    by = c("indicator" = "variable")
  ) |> # Filter only benchmarked indicators with families
  filter(!is.na(family_var)) |>
  filter(benchmark_static_family_aggregate_download == "Yes") |>
  select(1:5,
         var_name,
         family_var,
         family_name,
         benchmark_static_family_aggregate_download,
         indicator,
         score)


# Drop sectors
center_gov <-
  ctf_static_fam |>
  filter(
    !family_name %in% c(
      "Business Environment", # Drop sector-specific families
      "Energy and Environment Institutions"
    )
  ) |>
  select(-benchmark_static_family_aggregate_download)



# Rename regions and create cliar areas
indicator_wide_scores <- center_gov |>
  group_by(region, family_name, indicator, var_name, score) |>
  mutate(
    region = case_when(
      region == "East Asia & Pacific" ~ "EAP",
      region == "Europe & Central Asia" ~ "ECA",
      region == "Latin America & Caribbean" ~ "LAC",
      region == "Middle East, North Africa, Afghanistan & Pakistan" ~ "MENAAP",
      region == "South Asia" ~ "SAR",
      region == "Sub-Saharan Africa" ~ "SSA",
      TRUE ~ region
    )
  ) |>
  mutate(
    cliar_area = case_when(
      family_name %in% c("Social Institutions", "Political Institutions") ~ "pol arena",
      TRUE                                                               ~ "center of gov"
    )
  ) |>
  drop_na(score, income_group) |>
  select(country_code, income_group, region, family_name, var_name, indicator, score, cliar_area)




# hrm ---------------------------------------------------------------------

### data preparation

df <- indicator_wide_scores |>
  filter(family_name == "Public Human Resource Management Institutions") |>
  mutate(score_100 = score * 100)

df_plot <- df |>
  mutate(var_name = fct_reorder(var_name, score_100, .fun = median, na.rm = TRUE)) |>
  mutate(y = as.numeric(var_name))

# income-group mean (grey triangle)
df_income_mean <- df_plot |>
  group_by(var_name, income_group) |>
  summarise(
    xbar = ifelse(all(is.na(score_100)), NA_real_, mean(score_100, na.rm = TRUE)),
    y    = first(y),
    .groups = "drop"
  )

# indicator mean across ALL countries (grey segment)
df_indicator_mean <- df_plot |>
  group_by(var_name) |>
  summarise(
    xbar_ind = ifelse(all(is.na(score_100)), NA_real_, mean(score_100, na.rm = TRUE)),
    y        = first(y),
    .groups  = "drop"
  ) |>
  mutate(ymin = y - 0.5, ymax = y + 0.5)   # segment height around the row

y_levels <- levels(df_plot$var_name)

### Plot
df_labels <- df_plot |>
  filter(is.finite(score_100), !is.na(y), !is.na(country_code), !is.na(income_group))

ggplot(df_plot, aes(x = score_100, y = y)) +

  # country segments (0 -> score) per row
  geom_segment(
    aes(x = 0, xend = score_100, y = y, yend = y),
    color = "grey90",
    linewidth = 1,
    na.rm = TRUE
  ) +

  # thresholds
  geom_vline(
    xintercept = c(0, 25, 50, 75, 100),
    color = "grey90", linetype = "dotted", linewidth = 1
  ) +

  # overall indicator mean triangle (shape 23)
  geom_point(
    data = df_indicator_mean,
    aes(x = xbar_ind, y = y),
    shape = 17, size = 7,
    fill = "grey90", color = "grey90",
    show.legend = FALSE,
    na.rm = TRUE
  ) +

  # income-group mean dot per indicator
  geom_point(
    data = df_income_mean,
    aes(x = xbar, y = y, color = income_group),
    shape = 19, size = 5, stroke = 1,
    na.rm = TRUE
  ) +

  ggrepel::geom_text_repel(
    data = df_labels,
    aes(label = country_code, color = income_group),
    size = 2.5,
    segment.color = NA,
    box.padding = 0.25,
    max.overlaps = Inf,
    na.rm = TRUE
  ) +

  scale_color_brewer(palette = "Set2", name = "Income Group") +
  scale_y_continuous(
    breaks = seq_along(y_levels),
    labels = \(i) str_wrap(y_levels[i], width = 15)
  ) +
  scale_x_continuous(breaks = seq(0, 100, 25), limits = c(0, 100)) +
  labs(
    title = "Benchmarking Public Sector Employment Institutions: CTF scores (2020–2024 average)",
    subtitle = "Overall mean (grey triangle) and income-group means (colored dots) across indicators",
    x = "CTF score (0–100)", y = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid = element_blank(),
    legend.position = "top"
  )

ggsave_long(here("analysis","figs", "indicators_ctf", "hrm_institutions_indicators_final.png"
    )
  )
# digital -----------------------------------------------------------------


difital_df <- prep_indicator_data(
  data = indicator_wide_scores,
  family_name_value = "Digital Institutions",
  group_var = income_group)



ggsave_long(here("analysis","figs", "indicators_ctf", "digital_institutions_indicators.png"
)
)


# integrity ---------------------------------------------------------------


df <- indicator_wide_scores |>
  filter(family_name == "Degree of Integrity") |>
  mutate(score_100 = score * 100)

# label ALL countries (one label per var_name x country_code)
df_lab <- df |>
  distinct(var_name, country_code, .keep_all = TRUE) |>
  mutate(
    lab_col = case_when(
      score_100 < 25 ~ "red4",
      score_100 < 50 ~ "yellow4",
      TRUE           ~ "green4"
    )
  )

df |>
  mutate(var_name = fct_reorder(var_name, score_100, .fun = median)) |>
  ggplot(aes(x = score_100, y = var_name)) +
  geom_vline(xintercept = 0,   color = "red4",    linetype = "dashed", linewidth = 1) +
  geom_vline(xintercept = 25,  color = "yellow4", linetype = "dashed", linewidth = 1) +
  geom_vline(xintercept = 50,  color = "yellow4", linetype = "dashed", linewidth = 1) +
  geom_vline(xintercept = 100, color = "green4",  linetype = "dashed", linewidth = 1) +
  geom_point(
    aes(shape = income_group),
    position = position_jitter(height = 0.12, width = 0),
    size = 2.5, color = "black", alpha = 0.5,
    na.rm = TRUE
  ) +
  ggrepel::geom_text_repel(
    data = df_lab,
    aes(label = country_code, color = lab_col),
    size = 2.5,
    segment.color = NA,
    box.padding = 0.25,
    max.overlaps = Inf,
    na.rm = TRUE
  ) +
  ggtitle(
    "CTF Scores for Degree of Integrity Institutions Indicators",
    subtitle = "Scores shown for individual countries (2020-2024 average)"
  ) +
  scale_color_identity(guide = "none") +
  scale_y_discrete(labels = \(x) str_wrap(x, width = 15)) +
  scale_x_continuous(breaks = seq(0, 100, 25)) +
  coord_cartesian(xlim = c(0, 100)) +
  labs(x = "CTF score (0–100)", y = NULL, shape = "Income Group") +
  theme_minimal(base_size = 11) +
  theme(panel.grid.major.y = element_blank(),
        legend.position = "top")



ggsave_long(here("analysis","figs", "indicators_ctf", "integrity_institutions_indicators.png"
)
)



# transparency ------------------------------------------------------------


df <- indicator_wide_scores |>
  filter(family_name == "Transparency and Accountability Institutions") |>
  mutate(score_100 = score * 100)

# label ALL countries (one label per var_name x country_code)
df_lab <- df |>
  distinct(var_name, country_code, .keep_all = TRUE) |>
  mutate(
    lab_col = case_when(
      score_100 < 25 ~ "red4",
      score_100 < 50 ~ "yellow4",
      TRUE           ~ "green4"
    )
  )

df |>
  mutate(var_name = fct_reorder(var_name, score_100, .fun = median)) |>
  ggplot(aes(x = score_100, y = var_name)) +
  geom_vline(xintercept = 0,   color = "red4",    linetype = "dashed", linewidth = 1) +
  geom_vline(xintercept = 25,  color = "yellow4", linetype = "dashed", linewidth = 1) +
  geom_vline(xintercept = 50,  color = "yellow4", linetype = "dashed", linewidth = 1) +
  geom_vline(xintercept = 100, color = "green4",  linetype = "dashed", linewidth = 1) +
  geom_point(
    aes(shape = income_group),
    position = position_jitter(height = 0.12, width = 0),
    size = 2.5, color = "black", alpha = 0.5,
    na.rm = TRUE
  ) +
  ggrepel::geom_text_repel(
    data = df_lab,
    aes(label = country_code, color = lab_col),
    size = 2.5,
    segment.color = NA,
    box.padding = 0.25,
    max.overlaps = Inf,
    na.rm = TRUE
  ) +
  ggtitle(
    "CTF Scores for Transparency and Accountability  Institutions Indicators",
    subtitle = "Scores shown for individual countries (2020-2024 average)"
  ) +
  scale_color_identity(guide = "none") +
  scale_y_discrete(labels = \(x) str_wrap(x, width = 15)) +
  scale_x_continuous(breaks = seq(0, 100, 25)) +
  coord_cartesian(xlim = c(0, 100)) +
  labs(x = "CTF score (0–100)", y = NULL, shape = "Income Group") +
  theme_minimal(base_size = 11) +
  theme(panel.grid.major.y = element_blank(),
        legend.position = "top")



ggsave_long(here("analysis","figs", "indicators_ctf", "transparency_institutions_indicators.png"
)
)


# pfm ---------------------------------------------------------------------


df <- indicator_wide_scores |>
  filter(family_name == "Public Finance Institutions") |>
  mutate(score_100 = score * 100)

# label ALL countries (one label per var_name x country_code)
df_lab <- df |>
  distinct(var_name, country_code, .keep_all = TRUE) |>
  mutate(
    lab_col = case_when(
      score_100 < 25 ~ "red4",
      score_100 < 50 ~ "yellow4",
      TRUE           ~ "green4"
    )
  )

df |>
  mutate(var_name = fct_reorder(var_name, score_100, .fun = median)) |>
  ggplot(aes(x = score_100, y = var_name)) +
  geom_vline(xintercept = 0,   color = "red4",    linetype = "dashed", linewidth = 1) +
  geom_vline(xintercept = 25,  color = "yellow4", linetype = "dashed", linewidth = 1) +
  geom_vline(xintercept = 50,  color = "yellow4", linetype = "dashed", linewidth = 1) +
  geom_vline(xintercept = 100, color = "green4",  linetype = "dashed", linewidth = 1) +
  geom_point(
    aes(shape = income_group),
    position = position_jitter(height = 0.12, width = 0),
    size = 2.5, color = "black", alpha = 0.5,
    na.rm = TRUE
  ) +
  ggrepel::geom_text_repel(
    data = df_lab,
    aes(label = country_code, color = lab_col),
    size = 2.5,
    segment.color = NA,
    box.padding = 0.25,
    max.overlaps = Inf,
    na.rm = TRUE
  ) +
  ggtitle(
    "CTF Scores for Public Finance Management Institutions Indicators",
    subtitle = "Scores shown for individual countries (2020-2024 average)"
  ) +
  scale_color_identity(guide = "none") +
  scale_y_discrete(labels = \(x) str_wrap(x, width = 15)) +
  scale_x_continuous(breaks = seq(0, 100, 25)) +
  coord_cartesian(xlim = c(0, 100)) +
  labs(x = "CTF score (0–100)", y = NULL, shape = "Income Group") +
  theme_minimal(base_size = 11) +
  theme(panel.grid.major.y = element_blank(),
        legend.position = "top")



ggsave_long(here("analysis","figs", "indicators_ctf", "pfm_institutions_indicators.png"
)
)
