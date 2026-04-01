# Trends analysis: CTF dynamic
# Income groups: Low-income, Lower-middle-income, Upper-middle-income, High-income
# Line plot with CTF dynamic scores over time for each country, colored by income group

# set-up -----------------------------------------------------------------
library(dplyr)
library(tidyverse)
library(ggplot2)
library(janitor)

devtools::load_all()

theme_set(
  theme_minimal(base_family = "Segoe UI Semilight") +
    theme(
      axis.text.x = element_text(size = 10, hjust = .5),
      axis.text.y = element_text(size = 9),
      plot.title = element_text(size = 16, face = "bold"),
      plot.subtitle = element_text(size = 12),
      plot.background = element_blank(),
      plot.caption = element_text(hjust = 0, size = 10),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      legend.position = "top"
    )
)

ggsave_trends <- partial(
  ggplot2::ggsave,
  bg     = "white",
  width  = 12,
  height = 8
)

library(cliaretl)

# data-load ---------------------------------------------------------------

#filter non classified countries as income groups 
ctf_dyn <- cliaretl::closeness_to_frontier_dynamic 

metadata <- cliaretl::db_variables_final

# data_transformation ----------------------------------------------------

# Pivot the data to long format and join with metadata 
ctf_dyn_joined <- ctf_dyn |>
  pivot_longer(cols = 7:last_col(), # Pivot all indicator columns
               names_to = "variable",
               values_to = "value") |>
  left_join(metadata, by = "variable") |> # Join with variable metadata
  select(country_code, year, income_group, country_group, family_name, variable, var_name, value, benchmark_dynamic_indicator) # Explicitly select and preserve income_group


# Filter benchmarcked
 dyn_subset <- ctf_dyn_joined |> 
  filter(
      benchmark_dynamic_indicator == "Yes" # Filter only benchmarked indicators
  )


# Map institutional clusters to human-readable names for plotting
cluster_mapping <- c(
  "Public Human Resource Management Institutions" = "Public Human Resources Management",
  "Digital and Data Institutions"               = "Information Systems",
  "Degree of Integrity"                           = "Integrity",
  "Transparency and Accountability Institutions"  = "Transparency and Accountability",
  "Public Financial Management Institutions"     = "Public Financial Management"
)

# Prepare data for plotting: average CTF score by income group, family, and year
plot_dyn <- dyn_subset |> 
  filter(
    country_group == 0 # Only retain countries
  ) |>
  select(country_code, year, income_group, family_name, variable, var_name, value) |>
  filter(family_name %in% names(cluster_mapping)) |>
  mutate(
    family_name = dplyr::recode(family_name, !!!cluster_mapping),
    year        = as.integer(year)
  ) |>
  group_by(income_group, family_name, year) |> 
  summarise(ctf_score = mean(value, na.rm = TRUE), .groups = "drop") |> 
  filter(!is.na(income_group))

# Families to plot are now directly the recoded family_name values
families_to_plot <- unique(plot_dyn$family_name)

# plot -------------------------------------------------------------------

# Plot all families at once (returns named list with file-safe names)
plots <- plot_ctf_time_trends(plot_dyn, family = families_to_plot)

# Save all plots
output_dir <- here("analysis", "figs", "time_trends")
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

purrr::iwalk(plots, ~ggsave_trends(
  filename = file.path(output_dir, paste0("ctf_time_trends_", .y, ".png")),
  plot     = .x
))


# Faceted plots by indicator ----------------------------------------------

# Prepare indicator-level data: average CTF score by income group, var_name, family, and year
plot_dyn_ind <- dyn_subset |>
  filter(country_group == 0) |>
  select(country_code, year, income_group, family_name, var_name, value) |>
  filter(family_name %in% names(cluster_mapping)) |>
  mutate(
    family_name = dplyr::recode(family_name, !!!cluster_mapping),
    year        = as.integer(year)
  ) |>
  group_by(income_group, family_name, var_name, year) |>
  summarise(ctf_score = mean(value, na.rm = TRUE), .groups = "drop") |>
  filter(!is.na(income_group))

# One faceted plot per family
facet_plots <- purrr::map(
  families_to_plot,
  ~plot_ctf_time_trends_facet(plot_dyn_ind, family = .x)
)
names(facet_plots) <- families_to_plot |>
  tolower() |>
  (\(x) gsub("[^a-z0-9]+", "_", x))() |>
  (\(x) gsub("_+", "_", x))() |>
  (\(x) gsub("^_|_$", "", x))()

# Save faceted plots — one file per family + var_name combination
purrr::iwalk(facet_plots, function(p, family_slug) {
  # Extract the var_names present in this family's plot data
  fam_label <- families_to_plot[
    tolower(families_to_plot) |>
      (\(x) gsub("[^a-z0-9]+", "_", x))() |>
      (\(x) gsub("_+", "_", x))() |>
      (\(x) gsub("^_|_$", "", x))() == family_slug
  ]

  var_names <- plot_dyn_ind |>
    dplyr::filter(family_name == fam_label) |>
    dplyr::pull(var_name) |>
    unique()

  purrr::walk(var_names, function(vn) {
    vn_slug <- tolower(vn) |>
      (\(x) gsub("[^a-z0-9]+", "_", x))() |>
      (\(x) gsub("_+", "_", x))() |>
      (\(x) gsub("^_|_$", "", x))()

    plot_single <- plot_ctf_time_trends_facet(
      plot_dyn_ind |> dplyr::filter(var_name == vn),
      family = fam_label,
      title  = vn
    )

    ggplot2::ggsave(
      filename = file.path(
        output_dir,
        paste0("ctf_time_trends_", family_slug, "_", vn_slug, ".png")
      ),
      plot   = plot_single,
      bg     = "white",
      width  = 12,
      height = 8
    )
  })
}) 








