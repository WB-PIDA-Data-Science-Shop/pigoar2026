# CTF 2020 to 2024 coverage by country

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
    theme_few(base_size = 16)
)

ggsave <- partial(
  ggplot2::ggsave,
  bg     = "white",
  width  = 10,   # scales with number of indicators
  height = 25,    # scales with number of countries
  dpi    = 150
)

ggsave_annex <- partial(
  ggplot2::ggsave,
  bg     = "white",
  width  = 20,
  height = 14,
  dpi    = 150
)


# load-data --------------------------------------------------------------
clusters_data <- readRDS(here("data-raw", "output", "cluster_coverage_data.rds"))


library(cliaretl)
db_variables <- cliaretl::db_variables

countryclass <- cliaretl::wb_income_and_region 

# Extract the reference year
ref_year <- attr(db_variables, "ref_year")
print(ref_year) 

# data-transf ------------------------------------------------------------
glimpse(clusters_data)


vars_ctf <- db_variables |>
  filter(
    benchmarked_ctf == "Yes"
  ) |>
  pull(variable)

# data_processing ---------------------------------------------------------

coverage_country_complete <- clusters_data |> 
  # 2025 release, should take the static 2020 to 2024 period
  filter(between(year,2020,2024)) 

# ── 0. Country lookup (adapt to your actual source) ──────────────────────────
# Assuming you have a country metadata table; adjust as needed
country_meta <- countryclass |>          # or a dedicated lookup
  distinct(country_code, region, country_name)

# cluster label: 
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
)



# PLOT: YEAR-COUNT BY REGION -------------------------------------------


# ── 1. Summarise: count available years per country × indicator ───────────────
coverage_year_count <- coverage_country_complete |>
  group_by(region, country_code, country_name, family_name, variable, var_name) |>
  summarise(
    years_available = sum(is_available),   # TRUE counts as 1
    .groups = "drop"
  )

# simpler and safer version of the above:
coverage_year_count <- coverage_country_complete |>
  group_by(region, country_code, country_name, family_name, variable, var_name) |>
  summarise(
    years_available = sum(is_available),   # TRUE counts as 1
    .groups = "drop"
  )

# ── 2. Bin into fill categories ───────────────────────────────────────────────
coverage_year_count <- coverage_year_count |>
  mutate(
    coverage_band = case_when(
      years_available == 0 ~ "0",
      years_available == 1 ~ "1",
      years_available == 2 ~ "2",
      years_available >= 3 ~ "3+"
    ) |> factor(levels = c("0", "1", "2", "3+")),

    # wrap var_name for strip labels
    var_name_wrap = stringr::str_wrap(var_name, width = 15)
  )

# ── 3. Factor orders (reuse country_order from Part 1) ───────────────────────
indicator_order_p2 <- coverage_year_count |>
  distinct(family_name, variable, var_name) |>
  arrange(family_name, var_name) |>
  pull(variable)

coverage_year_count <- coverage_year_count |>
  mutate(
    country_code = factor(country_code, levels = rev(country_order)),
    family_name  = factor(
      family_name,
      levels = cluster_mapping_tbl$label,
      labels = stringr::str_wrap(cluster_mapping_tbl$label, width = 15)
    ),
    variable     = factor(variable, levels = indicator_order_p2)
  ) |>
  filter(region != "North America")

# ── 4. Plot factory: one plot per region ──────────────────────────────────────
plot_yearcount_region <- function(data, region_name) {
  data |>
    filter(region == region_name) |>
    ggplot(aes(x = variable, y = country_code, fill = coverage_band)) +
    geom_tile(color = "white", linewidth = 0.3) +
    geom_text(
      aes(label = years_available),
      color    = "white",
      fontface = "bold",
      size     = 2.5
    ) +
    scale_fill_manual(
      values = c(
        "0"  = "#B0BEC5",
        "1"  = "#E53935",
        "2"  = "#FDD835",
        "3+" = "#4CAF50"
      ),
      name = "Years available"
    ) +
    scale_x_discrete(labels = NULL) +
    # ── native ggplot2 nested strips: no extra package needed ─────────────
    facet_grid(
      cols   = vars(family_name, var_name_wrap),
      scales = "free_x",
      space  = "free_x"
    ) +
    labs(
      title    = paste("Years of Data Available —", region_name),
      subtitle = "Count of years with data across 2020–2024",
      x = NULL, y = NULL
    ) +
    theme_minimal(base_size = 10) +
    theme(
      panel.grid       = element_blank(),
      axis.text.x      = element_blank(),
      axis.ticks.x     = element_blank(),
      axis.text.y      = element_text(size = 7),

      # ── top strip = family name ───────────────────────────────────────────
      strip.text.x.top = element_text(face = "bold", size = 8,
                                      margin = margin(t = 4, b = 4)),

      # ── bottom strip = indicator name ─────────────────────────────────────
      strip.text.x     = element_text(size = 8, lineheight = 0.85,
                                      margin = margin(t = 3, b = 3)),

      strip.background = element_rect(fill = "grey92", colour = NA),
      strip.clip       = "off",

      legend.position  = "top",
      legend.direction = "horizontal",
      legend.title     = element_text(size = 8, face = "bold", vjust = 0.8),
      legend.text      = element_text(size = 8),
      plot.margin      = margin(t = 8, r = 16, b = 8, l = 8)
    )
}

# ── 5. Generate all plots ─────────────────────────────────────────────────────
regions_p2 <- sort(unique(coverage_year_count$region))

yearcount_plots <- setNames(
  purrr::map(regions_p2, ~plot_yearcount_region(coverage_year_count, .x)),
  regions_p2
)

# ── 6. Save ───────────────────────────────────────────────────────────────────
purrr::iwalk(yearcount_plots, \(p, nm) {
  ggsave_annex(
    filename = here(
      "analysis", "figs", "coverage",
      paste0("region_yearcount_", janitor::make_clean_names(nm), ".png")
    ),
    plot   = p
  )
})

# CODE END