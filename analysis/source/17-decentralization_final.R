# Decentralization and subnational governance figures 
# for the World Bank's 2026 PIGO Chapter 3.

library(tidyverse)    
library(countrycode)
library(rlang)
library(sf)
library(ggrepel)
library(ggthemes)
library(rnaturalearth)
library(rdhs)
library(here)
library(haven)
library(dotwhisker)
library(survey)
library(broom)


#figure 23- taken directly from source


# figure 24 --------------------------------------------------------------

croatia <- read_dta(
  here("data-raw/input/decentralization/Croatia-2023-full-data.dta")
)

# --- 2. Create binary indicator variables ---
df <- croatia %>%
  mutate(
    elec_constraint = as.integer(c30a >= 3),
    elec_outage     = as.integer(c6 == 1)
  )

# --- 3. Define survey design ---
svy <- svydesign(
  ids     = ~id,
  strata  = ~strata,
  weights = ~wmedian,
  data    = df,
  nest    = TRUE
)

# --- 4. Region labels ---
region_labels <- c(
  "1" = "Pannonian Croatia\n(including Osijek)",
  "3" = "City of Zagreb",
  "4" = "Northern Croatia\n(including Varaždin)",
  "2" = "Adriatic Croatia\n(including Rijeka and Split)"
)


# --- 5. Compute weighted regional means ---
options(survey.lonely.psu = "adjust")

compute_mean <- function(var, design) {
  svyby(as.formula(paste0("~", var)), ~a3a, design, svymean, na.rm = TRUE) %>%
    as.data.frame() %>%
    select(a3a, value = all_of(var)) %>%
    mutate(value = value * 100)
}

constraint_reg <- compute_mean("elec_constraint", svy) %>% mutate(indicator = "Identify electricity as a major constraint")
outage_reg     <- compute_mean("elec_outage",     svy) %>% mutate(indicator = "Experience electrical outages")

regional <- bind_rows(constraint_reg, outage_reg) %>%
  mutate(
    region = recode(as.character(a3a), !!!region_labels),
    region = factor(region, levels = rev(region_labels))
  )

# --- 6. Colors ---
bar_colors <- c(
  "Identify electricity as a major constraint" = "#1f4e79",
  "Experience electrical outages"              = "#e6b800"
)

# --- 7. Plot ---
p <- ggplot(regional, aes(x = value, y = region, fill = indicator)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  
  scale_fill_manual(values = bar_colors) +
  
  scale_x_continuous(
    limits = c(0, 32),
    breaks = seq(0, 30, by = 10),
    labels = function(x) paste0(x, "%")
  ) +
  
  labs(
    x    = "Percent of firms",
    y    = NULL,
    fill = NULL
  ) +
  
  theme_minimal(base_size = 15) +
  theme(
    legend.position    = "bottom",
    legend.text        = element_text(size = 9),
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_line(color = "grey85", linetype = "dotted"),
    axis.text.y        = element_text(size = 15),
    axis.title.x       = element_text(size = 15),
    plot.margin        = margin(t = 15, r = 10, b = 10, l = 10)
  )



# --- Licensing & permits graph ---
df2 <- croatia %>%
  mutate(
    licensing_constraint = ifelse(j30c < 0, NA, as.integer(j30c >= 3)),
    mgmt_time            = ifelse(j2 < 0, NA, j2)
  )

svy2 <- svydesign(
  ids     = ~id,
  strata  = ~strata,
  weights = ~wmedian,
  data    = df2,
  nest    = TRUE
)

compute_mean_pct <- function(var, design) {
  svyby(as.formula(paste0("~", var)), ~a3a, design, svymean, na.rm = TRUE) %>%
    as.data.frame() %>%
    select(a3a, value = all_of(var))
}

licensing_reg <- compute_mean("licensing_constraint", svy2) %>%
  mutate(indicator = "Identify licensing & permits as a major constraint")

mgmt_reg <- compute_mean_pct("mgmt_time", svy2) %>%
  mutate(indicator = "Time spent by management on regulatory compliance")

regional2 <- bind_rows(licensing_reg, mgmt_reg) %>%
  mutate(
    region = recode(as.character(a3a), !!!region_labels),
    region = factor(region, levels = rev(region_labels))
  )

bar_colors2 <- c(
  "Identify licensing & permits as a major constraint" = "#1f4e79",
  "Time spent by management on regulatory compliance"  = "#e6b800"
)

p2 <- ggplot(regional2, aes(x = value, y = region, fill = indicator)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  scale_fill_manual(values = bar_colors2) +
  scale_x_continuous(
    limits = c(0, 22),
    breaks = seq(0, 20, by = 5),
    labels = function(x) paste0(x, "%")
  )+
  labs(x = "Percent of total", y = NULL, fill = NULL) +
  theme_minimal(base_size = 15) +
  theme(
    legend.position    = "bottom",
    legend.text        = element_text(size = 9),
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_line(color = "grey85", linetype = "dotted"),
    axis.text.y        = element_text(size = 15),
    axis.title.x       = element_text(size = 15),
    plot.margin        = margin(t = 15, r = 10, b = 10, l = 10)
  )





## romania

romania <- read_dta(
  here("data-raw/input/decentralization/Romania-2023-full-data.dta")
)

# --- Region labels ---
region_labels_ro <- c(
  "1" = "North-West\n(including Cluj-Napoca, Oradea)",
  "2" = "Centre\n(including Brașov)",
  "3" = "North-East\n(including Iași)",
  "4" = "South-East\n(including Constanța)",
  "5" = "South Muntenia\n(including Ploiești)",
  "6" = "Bucharest-Ilfov",
  "7" = "South-West Oltenia\n(including Craiova)",
  "8" = "West\n(including Timișoara)"
)
# Run table(romania$a2) to confirm codes match

# --- Chart 1: Electricity ---
df_ro1 <- romania %>%
  mutate(
    elec_constraint = ifelse(c30a < 0, NA, as.integer(c30a >= 3)),
    elec_outage     = as.integer(c6 == 1)
  )

options(survey.lonely.psu = "adjust")

svy_ro1 <- svydesign(
  ids     = ~id,
  strata  = ~strata,
  weights = ~wmedian,
  data    = df_ro1,
  nest    = TRUE
)

compute_mean_ro <- function(var, design) {
  svyby(as.formula(paste0("~", var)), ~a2, design, svymean, na.rm = TRUE) %>%
    as.data.frame() %>%
    select(a2, value = all_of(var)) %>%
    mutate(value = value * 100)
}

compute_mean_pct_ro <- function(var, design) {
  svyby(as.formula(paste0("~", var)), ~a2, design, svymean, na.rm = TRUE) %>%
    as.data.frame() %>%
    select(a2, value = all_of(var))
}

constraint_ro <- compute_mean_ro("elec_constraint", svy_ro1) %>%
  mutate(indicator = "Identify electricity as a major constraint")

outage_ro <- compute_mean_ro("elec_outage", svy_ro1) %>%
  mutate(indicator = "Experience electrical outages")

regional_ro1 <- bind_rows(constraint_ro, outage_ro) %>%
  mutate(
    region = recode(as.character(a2), !!!region_labels_ro),
    region = factor(region, levels = rev(region_labels_ro))
  )
bar_colors1 <- c(
  "Identify electricity as a major constraint" = "#1f4e79",
  "Experience electrical outages"              = "#e6b800"
)

p_ro1 <- ggplot(regional_ro1, aes(x = value, y = region, fill = indicator)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  scale_fill_manual(values = bar_colors1) +
  scale_x_continuous(
    limits = c(0, 60),
    breaks = seq(0, 60, by = 10),
    labels = function(x) paste0(x, "%")
  ) +
  labs(x = "Percent of firms", y = NULL, fill = NULL) +
  theme_minimal(base_size = 15) +
  theme(
    legend.position    = "bottom",
    legend.text        = element_text(size = 9),
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_line(color = "grey85", linetype = "dotted"),
    axis.text.y        = element_text(size = 15),
    axis.title.x       = element_text(size = 15),
    plot.margin        = margin(t = 15, r = 10, b = 10, l = 10)
  )

print(p_ro1)

# --- Chart 2: Licensing & permits ---
df_ro2 <- romania %>%
  mutate(
    licensing_constraint = ifelse(j30c < 0, NA, as.integer(j30c >= 3)),
    mgmt_time            = j2
  )

svy_ro2 <- svydesign(
  ids     = ~id,
  strata  = ~strata,
  weights = ~wmedian,
  data    = df_ro2,
  nest    = TRUE
)

licensing_ro <- compute_mean_ro("licensing_constraint", svy_ro2) %>%
  mutate(indicator = "Identify licensing & permits as a major constraint")

mgmt_ro <- compute_mean_pct_ro("mgmt_time", svy_ro2) %>%
  mutate(indicator = "Time spent by management on regulatory compliance")

regional_ro2 <- bind_rows(licensing_ro, mgmt_ro) %>%
  mutate(
    region = recode(as.character(a2), !!!region_labels_ro),
    region = factor(region, levels = rev(region_labels_ro))
  )

bar_colors2 <- c(
  "Identify licensing & permits as a major constraint" = "#1f4e79",
  "Time spent by management on regulatory compliance"  = "#e6b800"
)

p_ro2 <- ggplot(regional_ro2, aes(x = value, y = region, fill = indicator)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  scale_fill_manual(values = bar_colors2) +
  scale_x_continuous(
    limits = c(0, 30),
    breaks = seq(0, 30, by = 5),
    labels = function(x) paste0(x, "%")
  ) +
  labs(x = "Percent of total", y = NULL, fill = NULL) +
  theme_minimal(base_size = 15) +
  theme(
    legend.position    = "bottom",
    legend.text        = element_text(size = 9),
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_line(color = "grey85", linetype = "dotted"),
    axis.text.y        = element_text(size = 15),
    axis.title.x       = element_text(size = 15),
    plot.margin        = margin(t = 15, r = 10, b = 10, l = 10)
  )

print(p_ro2)
 
ggsave("analysis/figs/decentralization/fig24a.png",
        plot   = p,
        width  = 10,
        height = 9,
        dpi    = 300
)

ggsave( "analysis/figs/decentralization/fig24b.png",
        plot   = p2,
        width  = 10,
        height = 9,
        dpi    = 300
)

ggsave( "analysis/figs/decentralization/fig24c.png",
        plot   = p_ro1,
        width  = 10,
        height = 9,
        dpi    = 300
)

ggsave( "analysis/figs/decentralization/fig24d.png",
        plot   = p_ro2,
        width  = 10,
        height = 9,
        dpi    = 300
)



# ============================================================================
# FIGURE 25 — Subnational coverage: water, vaccination, education (DHS data)
# ============================================================================

# --- 1. Define countries of interest ----------------------------------------
country_ids <- c(
  "ET",  # Ethiopia
  "KE",  # Kenya
  "TZ",  # Tanzania
  "GH",  # Ghana
  "NG",  # Nigeria
  "SN",  # Senegal
  "ML",  # Mali
  "ZM",  # Zambia
  "MZ",  # Mozambique
  "PK",  # Pakistan
  "KH",  # Cambodia
  "PH"   # Philippines
)

# --- 2. Define indicators ---------------------------------------------------
indicators <- tibble(
  id    = c("WS_SRCE_H_IMP",
            "CH_VACC_C_DP3",
            "ED_GARP_B_BTH"),
  label = c("Improved water source",
            "DPT3 vaccination coverage\nchildren 12–23 months",
            "Gross primary school\nattendance")
)

# --- 3. Pull subnational data from DHS API ----------------------------------
message("Fetching data from DHS API...")

pull_indicator <- function(ind_id) {
  dhs_data(
    indicatorIds = ind_id,
    countryIds   = country_ids,
    breakdown    = "subnational",
    returnFields = c(
      "CountryName", "DHS_CountryCode",
      "SurveyYear", "SurveyYearLabel",
      "CharacteristicLabel",
      "Value", "IsTotal"
    )
  ) %>%
    as_tibble() %>%
    mutate(IndicatorId = ind_id)
}

raw <- map_dfr(indicators$id, pull_indicator)

# Diagnostic: check record counts per indicator
# If any show 0, verify the indicator ID against the DHS STATcompiler API.
raw %>%
  count(IndicatorId) %>%
  print()

# --- 4. Clean data ----------------------------------------------------------
df <- raw %>%
  filter(IsTotal == 0, !is.na(Value)) %>%
  mutate(Value = as.numeric(Value)) %>%
  # Keep only the most recent survey per country × indicator
  group_by(DHS_CountryCode, IndicatorId) %>%
  filter(SurveyYear == max(SurveyYear)) %>%
  ungroup() %>%
  left_join(indicators, by = c("IndicatorId" = "id")) %>%
  mutate(label = factor(label, levels = indicators$label))

# --- 5. Sort countries by average coverage across indicators ----------------
year_labels <- df %>%
  group_by(CountryName) %>%
  summarise(
    year_label = {
      yrs <- sort(unique(SurveyYearLabel))
      if (length(yrs) == 1) yrs else paste(min(SurveyYear), max(SurveyYear), sep = "–")
    },
    .groups = "drop"
  )

country_order <- df %>%
  group_by(CountryName, IndicatorId) %>%
  summarise(mean_val = mean(Value, na.rm = TRUE), .groups = "drop") %>%
  group_by(CountryName) %>%
  summarise(avg_mean = mean(mean_val, na.rm = TRUE), .groups = "drop") %>%
  arrange(avg_mean) %>%
  pull(CountryName)

df <- df %>%
  left_join(year_labels, by = "CountryName") %>%
  mutate(
    CountryLabel = paste0(CountryName, " (", year_label, ")"),
    CountryLabel = factor(
      CountryLabel,
      levels = paste0(
        country_order, " (",
        year_labels$year_label[match(country_order, year_labels$CountryName)],
        ")"
      )
    )
  )

# --- 6. Plot ----------------------------------------------------------------
# Okabe-Ito palette (colourblind-friendly)
pal <- c(
  "Improved water source"                            = "#0072B2",
  "DPT3 vaccination coverage\nchildren 12–23 months" = "#E69F00",
  "Gross primary school\nattendance"                 = "#009E73"
)

p_fig25 <- ggplot(df, aes(x = CountryLabel, y = Value, fill = label)) +
  geom_boxplot(
    colour        = "black",
    outlier.shape = 16,
    outlier.size  = 0.9,
    outlier.alpha = 0.6,
    linewidth     = 0.35,   # replaces deprecated `size` for line elements
    width         = 0.6,
    position      = position_dodge(width = 0.75)
  ) +
  scale_fill_manual(values = pal, name = NULL) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 25)) +
  coord_flip() +
  labs(
    x       = NULL,
    y       = "Coverage (%)",
    caption = paste0(
      "Source: DHS Program STATcompiler API\n",
      "Each box = country; whiskers = 1.5×IQR; dots = outlier ADM1 regions\n",
      "Water: WS_SRCE_H_IMP. Vaccination: CH_VACC_C_DP3. Education: ED_GARP_B_BTH.",
      " All most recent survey."
    )
  ) +
  theme_bw(base_size = 11) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_line(colour = "grey85", linewidth = 0.4),
    panel.border       = element_rect(colour = "grey70"),
    axis.text.y        = element_text(size = 9,  colour = "black"),
    axis.text.x        = element_text(size = 9,  colour = "black"),
    axis.title.x       = element_text(size = 10, margin = margin(t = 6)),
    legend.position    = "top",
    legend.text        = element_text(size = 9),
    legend.key.size    = unit(0.5, "cm"),
    plot.caption       = element_text(colour = "grey50", size = 7, lineheight = 1.3),
    plot.margin        = margin(10, 15, 10, 10)
  )

ggsave( "analysis/figs/decentralization/fig25.png",
  plot   = p_fig25,
  width  = 10,
  height = 9,
  dpi    = 300
)



# ============================================================================
# FIGURE 26 — Subnational debt in the OECD
# ============================================================================


debt_data <- read.csv("data-raw/input/decentralization/oecd_debt.csv") %>%
  filter(!country %in% c("OECD9", "OECD36", "OECD27", "EU27")) %>%
  mutate(country = fct_reorder(country, pct_debt))

p_debt <- ggplot(debt_data, aes(x = country, y = pct_debt)) +
  geom_col(fill = "#0072B2", alpha = 0.85) +
  coord_flip() +
  labs(
    x       = NULL,
    y       = "Subnational share of public debt (%)",
    caption = "Source: OECD"
  ) +
  theme_bw(base_size = 11) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_line(colour = "grey85", linewidth = 0.4),
    panel.border       = element_rect(colour = "grey70"),
    axis.text          = element_text(size = 9, colour = "black"),
    axis.title.x       = element_text(size = 10, margin = margin(t = 6)),
    plot.caption       = element_text(colour = "grey50", size = 7)
  )
ggsave( "analysis/figs/decentralization/fig26.png",
        plot   = p_debt,
        width  = 10,
        height = 9,
        dpi    = 300
)



# ============================================================================
# FIGURE 27 — Procurement delays and dismissals in Brazilian municipalities
# ============================================================================
load("data-raw/input/decentralization/rais_mun.rda")
load("data-raw/input/decentralization/mides.rda")
theme_set(
  theme_minimal()
)


# clean ------------------------------------------------------------------
subnational_hr <- rais_mun |>
  rename(
    municipality_code = id_municipio,
    year = ano
  ) |>
  # weigh local governments by headcount, changing weights each year
  group_by(year) |>
  mutate(
    weighted_share_dismissed = (total_headcount / sum(total_headcount)) *
      share_dismissed * 100
  )

subnational_gov <-mides |>
  filter(
    between(year, 2008, 2021)
  ) |>
  mutate(
    municipality_code = as.character(municipality_code),
    gdp_per_capita_quartile = ntile(gdp_per_capita, 4) |>
      as.factor()
  ) |>
  left_join(
    subnational_hr,
    by = c("municipality_code", "year")
  ) 



# regression -------------------------------------------------------------
baseline_lm <- lm(
  weighted_average_delay ~ weighted_share_dismissed,
  data = subnational_gov|>
    mutate(
      year = as.character(year),
      across(
        is.numeric,
        \(col) scale(col)
      )
    )
)

controls_lm <- update(baseline_lm, . ~ . + gdp_per_capita + idhm + population)
fe_lm <- update(controls_lm, . ~ . + as.factor(state) + as.factor(year))

list(
  "Baseline" = baseline_lm,
  "Controls" = controls_lm,
  "Fixed-effects" = fe_lm
) |>
  purrr::map_dfr(tidy, .id = "model") |>
  filter(
    grepl("weighted_share_dismissed", term)
  ) |>
  dwplot(
    dot_args = list(
      aes(colour = model, shape = model),
      size = 5
    ),
    whisker_args = list(size = 1.5)
  ) |>
  relabel_predictors(
    c(
      weighted_share_dismissed = "Dismissal Rate",
      gdp_per_capita = "GDP per capita",
      population = "Population"
    )
  ) +
  xlab("Coefficient") +
  ylab("") +
  geom_vline(
    xintercept = 0,
    colour = "grey60",
    linetype = 2
  ) +
  theme_bw() +
  labs(x = "Coefficient Estimate with 95% CIs", y = "") +
  theme(
    legend.position = "bottom"
  ) +
  scale_shape_discrete(
    name = "Models",
    breaks = c(0, 1)
  ) +
  scale_colour_grey(
    start = .3,
    end = .7,
    name = "Models"
  )

ggsave(
 "analysis/figs/decentralization/fig27.png",
  height = 6,
  width = 9,
  bg = "white"
)
# ============================================================================
# FIGURE 28 — Fiscal decentralization: revenue vs. expenditure scatter
# ============================================================================

# --- Load and prepare IMF data ----------------------------------------------
groupings=read.csv("data-raw/input/decentralization/groupings.csv")
imf_data <- read.csv( "data-raw/input/decentralization/IMF.csv") %>%
  mutate(country_code = countrycode(COUNTRY, "country.name", "iso3c")) %>%
  left_join(groupings, by = "country_code") %>%   # left_join preferred over merge()
  filter(SECTOR == "Central government")

data_expenditure <- filter(
  imf_data,
  INDICATOR == "Expenditure decentralization (ratio of own spending to general government spending), Percent"
)

data_revenue <- filter(
  imf_data,
  INDICATOR == "Tax revenue decentralization, Ratio of this level of government's revenue from this item to total general government revenue from this item, Percent"
)

data_admin <- filter(
  imf_data,
  INDICATOR == "Compensation of employees, Ratio of this level of government's expenditure on this item to total general government expenditure on this, Percent"
)

# --- Pivot year columns to long format --------------------------------------
pivot_year_data <- function(df, value_name) {
  df %>%
    pivot_longer(
      cols      = matches("^X\\d{4}$"),  # selects columns named X1973, X1974, ... etc.
      names_to  = "Year",
      values_to = value_name
    ) %>%
    mutate(Year = as.numeric(str_remove(Year, "^X")))
}

data_exp_long   <- pivot_year_data(data_expenditure, "expenditure")
data_rev_long   <- pivot_year_data(data_revenue,     "revenue")
data_admin_long <- pivot_year_data(data_admin,        "adminexpenditure")

# --- Merge and filter -------------------------------------------------------
data_scatter <- data_exp_long %>%
  left_join(data_rev_long,   by = c("country_code", "Year")) %>%
  left_join(data_admin_long, by = c("country_code", "Year")) %>%
  filter(Year > 2014)

# --- Country averages -------------------------------------------------------
country_avg <- data_scatter %>%
  group_by(country_code, income_group.x) %>%
  summarise(
    mean_rev      = mean(revenue,          na.rm = TRUE),
    mean_exp      = mean(expenditure,      na.rm = TRUE),
    mean_adminexp = mean(adminexpenditure, na.rm = TRUE),
    adminexp15    = adminexpenditure[Year == 2015],
    .groups = "drop"
  ) %>%
  mutate(
    income_group.x = factor(
      income_group.x,
      levels  = c("High income", "Upper middle income",
                  "Lower middle income", "Low income"),
      ordered = TRUE
    )
  )

# --- Shared plot components -------------------------------------------------
scatter_theme <- list(
  theme_minimal(base_size = 14),
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(linewidth = 0.3, color = "gray90"),
    legend.position  = "bottom",
    legend.title     = element_blank()
  ),
  scale_color_solarized(),
  guides(color = guide_legend(nrow = 2, byrow = TRUE))
)

reference_lines <- list(
  geom_abline(intercept = 0,   slope = 1, color = "black", linetype = "dashed", linewidth = 0.5),
  geom_abline(intercept = 0.5, slope = 0, color = "gray",  linetype = "dashed", linewidth = 0.5),
  geom_vline(xintercept = 0.5,            color = "gray",  linetype = "dashed", linewidth = 0.5)
)

# --- Figure 28: Revenue vs. Expenditure -------------------------------------
p_fig28 <- ggplot(country_avg, aes(x = mean_rev, y = mean_exp, color = income_group.x)) +
  geom_point(size = 2.5, alpha = 0.8) +
  geom_text_repel(
    aes(label = country_code),
    size         = 3,
    show.legend  = FALSE,
    max.overlaps = Inf
  ) +
  labs(
    x     = "Central revenue collection",
    y     = "Central expenditures",
    color = "Income level"
  ) +
  reference_lines +
  scatter_theme

ggsave("analysis/figs/decentralization/fig28.png",
  plot = p_fig28,
  dpi  = 300
)



# ============================================================================
# FIGURE 29 — Administrative decentralization
# ============================================================================

# Load and prepare admin data
admin_data <- read.csv("data-raw/input/decentralization/admindecent.csv") %>%
  filter(hiring.practices != 0.75) %>%  # Remove oddly coded observation
  mutate(
    hiring_reversed = abs(hiring.practices - 1),
    country_code = countrycode(country, 'country.name', 'iso3c')
  )

# Merge with country averages
country_avg_admin <- merge(country_avg, admin_data, by = 'country_code') %>%
  mutate(
    hiring_disc = factor(
      hiring_reversed,
      levels = c(0, 0.5, 1),
      labels = c("Full local authority", "Shared authority", 
                 "Full central authority")
    )
  )


p_fig29 <-ggplot(country_avg_admin, 
                 aes(x = adminexp15, y = hiring_disc, color = income_group.x)) +
  geom_point(size = 2.5, alpha = 0.8) +
  geom_text_repel(
    aes(label = country_code),
    size = 3,
    show.legend = FALSE,
    max.overlaps = Inf
  ) +
  labs(
    x = "Central expenditures on personnel",
    y = "Centralized Hiring Practices",
    color = "Income level"
  ) +
  scatter_theme
ggsave(
  "analysis/figs/decentralization/fig29.png",
  plot = p_fig28,
  dpi  = 300
)


