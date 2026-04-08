# CTF 2020 vs 2024 change: distance to frontier by income group and family

# Load necessary libraries
library(haven)
library(here)
library(readxl)
library(readr)
library(dplyr)
library(purrr)
library(stringr)
library(tidyr)
library(janitor)
library(ggthemes)
library(ggplot2)
library(readr)

devtools::load_all()

theme_set(
    theme_few(base_size = 14)
)

ggsave_frontier<- partial(
  ggplot2::ggsave,
  bg     = "white",
  width  = 12,
  height = 8
)

library(cliaretl)

# PART 1. Budget Exc-----------------------------------------------------
#load data
countryclass <- cliaretl::wb_income_and_region

# clean data
budget_execution <- pigoar2026::budget_execution |> 
  mutate(year = as.integer(year)) |> 
  # data coverage issues before 2007 (fewer countries)
  # in 2024, low-income countries coverage drops from 10 to 2
  filter(between(year, 2013, 2023)) |> 
  mutate(
    # Distance from perfect execution (100%): lower = better
    # e.g. 95% execution -> distance of 5; 110% execution -> distance of 10
    budget_execution_rate = abs(100 - budget_execution_rate)
  )


# use cliaretl fun to overwrite the db_variables and add the outturn indicator

# PART 2. financial stability ctf calculation ------------------------------------
# Important:
# To compute the distance to frontier change, the use cliarertl::rescale_indicator()
# and cliaretl::compute_ctf() functions require the original raw indicator values
#  (before rescaling) to ensure consistency in the calculation.

# dat-aload
country_list <- cliaretl::wb_country_list
income_and_region_class <- cliaretl::wb_income_and_region
db_variables <- cliaretl::db_variables_final

# Add the budget_execution_rate variable to db_variables metadata
db_variables <- cliaretl::addnew_db_variables(
  db_variables,
  new_variables = tibble::tibble(
    variable = "budget_execution_rate",
    var_name = "Budget execution rate (%)",
    family_name = "Public Financial Management Institutions",
    description = "Budget execution rate (outturn) as a percentage of the approved budget, averaged across all sectors. Source: PEFA assessments.",
    benchmarked_ctf = "Yes",
    benchmark_dynamic_indicator = "Yes"
  )
)
cliar_indicators <- read_rds(here("data-raw", "input", "compiled_indicators.rds")) |> 
    # filter to only years 2013 or later
  filter(
    year >= 2013
  )

# left join variable = budget_execution_rate indicator to compiled indicators
cliar_indicators_b <- cliar_indicators |> 
  left_join(
    budget_execution |> select(country_code, year, budget_execution_rate),
    by = c("country_code", "year")
  ) 


# data preparation and rescaling
vars_ctf <- db_variables |>
  filter(
    benchmarked_ctf == "Yes"
  ) |>
  pull(variable) 

var_lists <- get_variable_lists(db_variables)

## Add the new variable to the list of dynamic CTF variables for rescaling and CTF computation
var_lists$vars_dynamic_ctf <- c(var_lists$vars_dynamic_ctf, "bs_bti_q8_2", "budget_execution_rate")


# Rescale indicators and compute CTF for the dynamic set
cliar_indicators_rescaled <- cliar_indicators_b |>
  mutate(
    # OECD PMR (0–6): reverse; drop pre-2018
    across(starts_with("oecd_pmr"),
           ~ if_else(year < 2018, NA_real_, reverse_indicator(.x, min = 0, max = 6))),
    # Freedom House (1–7): reverse
    across(starts_with("fh_fiw"), ~ reverse_indicator(.x, min = 1, max = 7)),
    # Enterprise Survey (0–100): reverse (use the actual prefix you have)
    across(starts_with("wb_enterprisesurveys"), ~ reverse_indicator(.x, min = 0, max = 100)),
    # V-DEM: flip so higher = stronger institutions
    across(c(vdem_core_v2x_pubcorr, vdem_core_v2x_execorr, vdem_core_v2cacamps), flip_indicator),
    # WDI pupil–teacher ratios: rescale to 0–1, then flip so higher = stronger
    wdi_seprmenrltczs = flip_indicator(rescale_indicator(wdi_seprmenrltczs, scale_to = 1)),
    wdi_sesecenrltczs = flip_indicator(rescale_indicator(wdi_sesecenrltczs, scale_to = 1)),
    #ALSO RESCALE THE BTI INDICATOR:
    bs_bti_q8_2 = (rescale_indicator(bs_bti_q8_2, scale_to = 1)),
    # GFDB bank concentration (0–100): reverse
    wb_gfdb_oi_01 = reverse_indicator(wb_gfdb_oi_01, min = 0, max = 100),
    # And rescale to absolute terms distance and rescale (0-1) budget_execution_rate
    budget_execution_rate = rescale_indicator(budget_execution_rate, scale_to = 1)
  )
  

## Scores
# helper fns that don't warn on all-NA
safe_min <- function(x) if (all(is.na(x))) NA_real_ else min(x, na.rm = TRUE)
safe_max <- function(x) if (all(is.na(x))) NA_real_ else max(x, na.rm = TRUE)

## Dynamic min max
# note: there are quite a few cases of Infinite warnings (due to missingness)
min_max_dynamic <- cliar_indicators_rescaled |>
  filter(
    between(year,2014,2023)
  ) |>
  summarise(
    across(
      all_of(var_lists$vars_dynamic_ctf),
      list(
        min = ~ min(., na.rm = TRUE),
        max = ~ max(., na.rm = TRUE)
      ),
      .names = "{.col}-{.fn}"
    )
  ) |>
  pivot_longer(
    everything(),
    names_to = c("variable", ".value"),
    names_pattern = "(.*)-(.*)"
  ) |>
  filter(!is.infinite(min) & !is.infinite(max))


## Dynamic CTF 
# Dynamic
ctf_dynamic_fiscal_s <-
  cliaretl::compute_ctf(
    cliar_indicators_rescaled,
    var_lists$vars_dynamic_ctf,
    min_max_dynamic,
    c("country_code","year"),
    zero_floor = 0.01)




# data_transformation ----------------------------------------------------

metadata <- cliaretl::db_variables_final
ctf_dyn <- cliaretl::closeness_to_frontier_dynamic

## add the new benchmarked indicators to the ctf_dyn
ctf_dyn <- ctf_dyn |>
  left_join(
    ctf_dynamic_fiscal_s |> select(country_code, year, bs_bti_q8_2, budget_execution_rate),
    by = c("country_code", "year")
  )

# Pivot the data to long format and join with metadata 
ctf_dyn_joined <- ctf_dyn |>
  pivot_longer(cols = 7:last_col(), # Pivot all indicator columns
               names_to = "variable",
               values_to = "value") |>
  left_join(metadata, by = "variable") |> # Join with variable metadata
  select(country_code, year, income_group, country_group, family_name, variable, var_name, value, benchmark_dynamic_indicator, source) # Explicitly select and preserve income_group


# Filter benchmarcked
dyn_ctf_plot <- ctf_dyn_joined |> 
  filter(
    benchmark_dynamic_indicator == "Yes" | variable %in% c("bs_bti_q8_2", "budget_execution_rate")
  ) |> 
  filter(!is.na(income_group)) |>
  # Overwrite family_name for bs_bti_q8_2 and budget_execution_rate
  dplyr::mutate(
    family_name = dplyr::if_else(
      variable %in% c("bs_bti_q8_2", "budget_execution_rate"),
      "Public Financial Management Institutions",
      family_name
    )
  )


# Verify bs_bti_q8_2 is present and correctly assigned
dyn_ctf_plot |>
  dplyr::filter(variable == c("bs_bti_q8_2", "budget_execution_rate")) |>
  dplyr::distinct(variable, family_name, benchmark_dynamic_indicator)


# Use a lookup table and normalize keys to avoid whitespace/case mismatch issues.
cluster_mapping_tbl <- tibble::tibble(
  raw = c(
    "Public Human Resource Management Institutions",
    "Digital and Data Institutions",
    "Degree of Integrity",
    "Transparency and Accountability Institutions",
    "Public Financial Management Institutions"
  ),
  label = c(
    "Public Human Resources Management",
    "Information Systems",
    "Integrity",
    "Transparency and Accountability",
    "Public Financial Management"
  )
) |> 
  dplyr::mutate(
    raw_norm = stringr::str_to_lower(stringr::str_squish(raw))
  )

# Define the income levels in the desired order for plotting (if needed)
  income_levels <- c(
    "High income",
    "Upper middle income",
    "Lower middle income",
    "Low income"
  )

# Named vector: raw family name -> display label (used by compute_ctf_diff)
cluster_mapping <- setNames(cluster_mapping_tbl$label, cluster_mapping_tbl$raw)

# verify which family_name values in the data match the mapping keys
# Run interactively to catch any remaining mismatches. Shows original family_name,
# the normalized form, and whether a mapping exists.
matched_families <- dyn_ctf_plot |>
  dplyr::mutate(family_name_norm = stringr::str_to_lower(stringr::str_squish(family_name))) |>
  dplyr::distinct(family_name, family_name_norm) |>
  dplyr::left_join(cluster_mapping_tbl, by = c("family_name_norm" = "raw_norm")) |>
  dplyr::mutate(in_mapping = !is.na(label))
matched_families


# PART 3. visualizations -------------------------------------------------


# plot data 2020 to 2024 --------------------------------------------------------------
# Generate and save one plot per family using compute_ctf_diff() fun
output_dir <- here("analysis", "figs", "frontier_distance")
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

families <- unname(cluster_mapping)

purrr::walk(families, function(fam) {
  diff_data <- compute_ctf_diff(
    dyn_ctf_plot,
    family    = fam,
    from_year = 2020,
    to_year   = 2024,
    mapping   = cluster_mapping
  )
  p         <- generate_ctf_diff_plot(diff_data, income_order = income_levels)

  if (is.null(p)) return(invisible(NULL))

  fam_slug <- tolower(fam)
  fam_slug <- gsub("[^a-z0-9]+", "_", fam_slug)
  fam_slug <- gsub("_+",         "_", fam_slug)
  fam_slug <- gsub("^_|_$",      "",  fam_slug)

  ggsave_frontier(
    filename = file.path(output_dir, paste0("ctf_diff_2020_2024_", fam_slug, ".png")),
    plot     = p
  )
})


# Use compute_ctf_diff for each family and bind results
all_diffs <- purrr::map_dfr(families, function(fam) {
  compute_ctf_diff(dyn_ctf_plot, family = fam, from_year = 2020, to_year = 2024, mapping = cluster_mapping)
})

# % of countries that improved vs declined by family and income group
# Excludes countries not classified in an income group (already handled upstream)
improvement_table <- all_diffs |>
  dplyr::mutate(
    income_group     = factor(income_group, levels = income_levels),
    change_direction = dplyr::case_when(
      difference > 0  ~ "Improved",
      difference < 0  ~ "Declined",
      difference == 0 ~ "Stagnated",
      TRUE            ~ NA_character_
    )
  ) |>
  dplyr::group_by(family_name, income_group, change_direction) |>
  dplyr::summarise(count = n(), .groups = "drop") |>
  dplyr::group_by(family_name, income_group) |>
  dplyr::mutate(percent = count / sum(count) * 100) |>
  dplyr::ungroup() |>
  dplyr::arrange(family_name, income_group, change_direction)

#drop Public Financial Management rows 2024
improvement_table_clean <- improvement_table |> 
  filter(!(family_name == "Public Financial Management"))


# also for digital 2020 to 2022

# Use compute_ctf_diff for each family and bind results
digital_diffs <- purrr::map_dfr(families, function(fam) {
  compute_ctf_diff(dyn_ctf_plot, family = fam, from_year = 2020, to_year = 2022)
})

# % of countries that improved vs declined by family and income group
# Excludes countries not classified in an income group (already handled upstream)
digital_table <- digital_diffs |>
  dplyr::mutate(
    income_group     = factor(income_group, levels = income_levels),
    change_direction = dplyr::case_when(
      difference > 0  ~ "Improved",
      difference < 0  ~ "Declined",
      difference == 0 ~ "Stagnated",
      TRUE            ~ NA_character_
    )
  ) |>
  dplyr::group_by(family_name, income_group, change_direction) |>
  dplyr::summarise(count = n(), .groups = "drop") |>
  dplyr::group_by(family_name, income_group) |>
  dplyr::mutate(percent = count / sum(count) * 100) |>
  dplyr::ungroup() |>
  dplyr::arrange(family_name, income_group, change_direction) |> 
  filter(family_name %in% c("Information Systems","Public Financial Management")
  )


# bind rows to improbements table
improvement_summary <- bind_rows(
  improvement_table_clean,
  digital_table,

)


# plot data 2020 to 2022 --------------------------------------------------------------
purrr::walk(families, function(fam) {
  diff_data <- compute_ctf_diff(dyn_ctf_plot, family = fam, from_year = 2020, to_year = 2022)
  p         <- generate_ctf_diff_plot(diff_data, income_order = income_levels)

  if (is.null(p)) return(invisible(NULL))

  fam_slug <- tolower(fam)
  fam_slug <- gsub("[^a-z0-9]+", "_", fam_slug)
  fam_slug <- gsub("_+",         "_", fam_slug)
  fam_slug <- gsub("^_|_$",      "",  fam_slug)

  ggsave_frontier(
    filename = file.path(output_dir, paste0("ctf_diff_2020_2022_", fam_slug, ".png")),
    plot     = p
  )
})



# summary infographic ----------------------------------------------------

glimpse(improvement_summary)

# Calculate overall success rate (% Improved) for each family to order them
family_success_rates <- improvement_summary |>
  filter(!is.na(change_direction)) |>
  group_by(family_name, change_direction) |>
  summarise(total_percent = sum(percent), .groups = "drop") |>
  pivot_wider(names_from = change_direction, values_from = total_percent, values_fill = 0) |>
  mutate(success_rate = Improved / (Improved + Declined + Stagnated) * 100) |>
  arrange(success_rate) |>
  pull(family_name)

# Improvement by family and income group - stacked bar chart
improvement_summary |>
  filter(!is.na(change_direction)) |>
  mutate(
    family_name = str_wrap(family_name, width = 20),
    family_name = factor(family_name, levels = str_wrap(family_success_rates, width = 20)),
    income_group = factor(income_group, levels = income_levels),
    change_direction = factor(change_direction, levels = c("Declined", "Stagnated", "Improved"))
  ) |>
  ggplot(aes(x = family_name, y = percent, fill = change_direction)) +
  geom_col(position = "stack", alpha = 0.85, width = 0.7) +
  geom_text(
    aes(label = paste0(round(percent, 0), "%")),
    position = position_stack(vjust = 0.5),
    size = 3,
    color = "white",
    fontface = "bold"
  ) +
  facet_wrap(~ income_group, nrow = 1) +
  scale_fill_manual(
    values = c("Improved" = "#2ecc71", "Stagnated" = "#f1c40f", "Declined" = "#e74c3c"),
    name = "Change Direction"
  ) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 20)) +
  labs(
    title = "Macro-Institutional Trajectories: Dynamics of Institutional Capacity (2020-2024)*",
    subtitle = "Proportion of countries within each income group that improved, stagnated, or declined\nacross institutional dimensions",
    x = NULL,
    y = "Percentage (%)"
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    axis.text.y = element_text(size = 10),
    strip.text = element_text(size = 11, face = "bold")
  ) +
  coord_flip()

ggsave_frontier(
  filename = file.path(output_dir, "bars_improvement_by_family_income_budget.png")
)

family_success_rates <- improvement_summary |>
  filter(!is.na(change_direction)) |>
  group_by(family_name, change_direction) |>
  summarise(total_percent = sum(percent), .groups = "drop") |>
  pivot_wider(names_from = change_direction, values_from = total_percent, values_fill = 0) |>
  mutate(success_rate = Improved / (Improved + Declined + Stagnated) * 100) |>
  arrange(desc(success_rate)) |>
  pull(family_name)

# Improvement by family and income group - donut chart grid
improvement_summary |>
  filter(!is.na(change_direction)) |>
  mutate(
    family_name = str_wrap(family_name, width = 15),
    family_name = factor(family_name, levels = str_wrap(family_success_rates, width = 15)),
    income_group = factor(income_group, levels = income_levels),
    change_direction = factor(change_direction, levels = c("Declined", "Stagnated", "Improved"))
  ) |>
  ggplot(aes(x = 2, y = percent, fill = change_direction)) +
  geom_col(width = 1, alpha = 0.85) +
  geom_text(
    aes(label = paste0(round(percent, 0), "%")),
    position = position_stack(vjust = 0.5),
    size = 4,
    color = "white",
    fontface = "bold"
  ) +
  facet_grid(family_name ~ income_group, switch = "y") +
  scale_fill_manual(
    values = c("Improved" = "#2ecc71", "Stagnated" = "#f1c40f", "Declined" = "#e74c3c"),
    name = "Change Direction"
  ) +
  xlim(0.5, 2.5) +
  coord_polar(theta = "y") +
  labs(
    title = "Macro-Institutional Trajectories: Dynamics of Institutional Capacity (2020-2024)",
    subtitle = "Proportion of countries within each income group that improved, stagnated, or declined\nacross institutional dimensions",
    y = "Percentage (%)"
  ) +
  theme_void() +
  theme(
    legend.position = "top",
    strip.text.x = element_text(size = 10, face = "bold"),
    strip.text.y.left = element_text(size = 9, angle = 0, hjust = 1),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5),
    plot.margin = margin(10, 10, 10, 10)
  )

ggsave_donas<- partial(
  ggplot2::ggsave,
  bg     = "white",
  width  = 12,
  height = 12
)

ggsave_donas(
  filename = file.path(output_dir, "donas_improvement_by_family_income_budget.png")
)












