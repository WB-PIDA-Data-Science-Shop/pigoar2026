#' @title ACLED conflict data
#' @description ACLED demonstration data, aggregated by country and year.
#' @format A data frame with 2726 rows and 3 variables:
#' \describe{
#'   \item{\code{country}}{character The name of the country}
#'   \item{\code{country_code}}{World Bank country code}
#'   \item{\code{year}}{double The year of the demonstration events}
#'   \item{\code{events}}{double The number of demonstration events}
#'   \item{\code{country_name}}{character. Official World Bank country name}
#'   \item{\code{region}}{character. Official World Bank regional classification}
#'   \item{\code{income_group}}{character. Official World Bank income classification}
#'}
#' @details #' The Armed Conflict Location & Event Data Project (ACLED) is a comprehensive dataset of political violence, protest, and disorder events across the world. This dataset aggregates ACLED event data for Asia at the weekly level, including event types such as battles, protests, riots, and violence against civilians. Each record summarizes the number of events, fatalities, and estimated population exposure for a specific week, country, and administrative region. Geographic centroids are provided for spatial analysis. For more information, see the official ACLED codebook: https://acleddata.com/resources/codebooks/
#' @source https://acleddata.com/aggregated/number-demonstration-events-country-year
"acled"

#' @title ACLED Asia Weekly Conflict Event Data
#' @description This dataset provides weekly aggregated conflict and disorder event data for countries in Asia, derived from the Armed Conflict Location & Event Data Project (ACLED). It includes event counts, fatalities, population exposure, and geographic information at the region and country level.
#' @format A data frame with 198945 rows and 13 variables:
#' \describe{
#'   \item{\code{week}}{double. The week number corresponding to the aggregation period.}
#'   \item{\code{country}}{character. The country in which the event occurred.}
#'   \item{\code{admin1}}{character. The first-level administrative division (e.g., province, state) where the event took place.}
#'   \item{\code{event_type}}{character. The broad category of the event (e.g., Battles, Protests, Violence against civilians).}
#'   \item{\code{sub_event_type}}{character. The more specific type of event within the event_type (e.g., Armed clash, Peaceful protest).}
#'   \item{\code{events}}{double. The number of events recorded for the given week and location.}
#'   \item{\code{fatalities}}{double. The total number of reported fatalities associated with the events.}
#'   \item{\code{population_exposure}}{double. The estimated population exposed to the events in the given week and location.}
#'   \item{\code{disorder_type}}{character. The type of disorder (e.g., Political violence, Demonstrations).}
#'   \item{\code{id}}{double. Unique identifier for the aggregated record.}
#'   \item{\code{centroid_latitude}}{double. Latitude of the centroid of the administrative unit where events occurred.}
#'   \item{\code{centroid_longitude}}{double. Longitude of the centroid of the administrative unit where events occurred.}
#'   \item{\code{year}}{double. Year.}
#'   \item{\code{country_code}}{World Bank country code}
#'   \item{\code{country_name}}{character. Official World Bank country name}
#'   \item{\code{region}}{character. Official World Bank regional classification}
#'   \item{\code{income_group}}{character. Official World Bank income classification}
#' }
#' @details The Armed Conflict Location & Event Data Project (ACLED) is a comprehensive dataset of political violence, protest, and disorder events across the world. This dataset aggregates ACLED event data for Asia at the weekly level, including event types such as battles, protests, riots, and violence against civilians. Each record summarizes the number of events, fatalities, and estimated population exposure for a specific week, country, and administrative region. Geographic centroids are provided for spatial analysis. For more information, see the official ACLED codebook: https://acleddata.com/resources/codebooks/
"acled_regional"

#' @title Microdados de Despesas de Entes Subnacionais (MiDES)
#' @description This dataset contains annual panel data on public procurement and public expenditure of Brazilian municipalities.
#' @format A data frame with 43,298 rows and 10 variables:
#' \describe{
#'   \item{\code{state}}{character. Two-letter abbreviation of the Brazilian state (UF) to which the municipality belongs.}
#'   \item{\code{year}}{double. Year of observation.}
#'   \item{\code{municipality_code}}{double. IBGE 7-digit code identifying each municipality.}
#'   \item{\code{weighted_average_delay}}{double. Average years of schooling delay among enrolled students, weighted by enrollment size.}
#'   \item{\code{population}}{double. Total resident population of the municipality in the given year.}
#'   \item{\code{gdp}}{double. Gross Domestic Product of the municipality, in constant BRL.}
#'   \item{\code{gdp_per_capita}}{double. GDP per capita, calculated as GDP divided by total population.}
#'   \item{\code{total_students}}{double. Total number of students enrolled in basic education (public and private).}
#'   \item{\code{formal_market_workers}}{double. Number of formally employed workers (i.e., with a signed labor contract) in the municipality.}
#'   \item{\code{idhm}}{double. Municipal Human Development Index (Índice de Desenvolvimento Humano Municipal), a composite measure of education, income, and longevity.}
#'}
#' @details The dataset was constructed by merging data from multiple official sources, including the IBGE (Brazilian Institute of Geography and Statistics), INEP (National Institute of Educational Studies), and RAIS (Annual Report of Social Information). All monetary values are adjusted to constant prices. Data is cleaned and harmonized to ensure consistency across years and municipalities.
"mides"

#' @title RAIS Municipal Dataset
#' @description Summary statistics on headcount, hiring and dismissals at the municipality-year level
#' @format A data frame with 107076 rows and 7 variables:
#' \describe{
#'   \item{\code{id_municipio}}{character Municipality identifier (IBGE code)}
#'   \item{\code{ano}}{integer Year}
#'   \item{\code{total_headcount}}{integer Total headcount}
#'   \item{\code{total_new_hire}}{double Total new hires}
#'   \item{\code{total_dismissed}}{double Total dismissals}
#'   \item{\code{share_new_hire}}{double Share of new hires}
#'   \item{\code{share_dismissed}}{double Share of dismissals}
#'}
#' @details Data extracted from the Base dos Dados
#' @source: https://basedosdados.org/dataset/3e7c4d58-96ba-448e-b053-d385a829ef00?table=dabe5ea8-3bb5-4a3e-9d5a-3c7003cd4a60
"rais_mun"

#' @title Country credit rating
#' @description Average of scores across the rating of the four top rating agencies (S&P, Moody’s, Fitch and DBRS). Scoring the creditworthiness of a country between 100 (riskless) and 0 (likely to default), assigned according to Trading Economics’ methodology and based on Standard & Poor, Moody’s and DBRS sovereign debt credit rating.
#' @format A data frame with 369 rows and 3 variables:
#' \describe{
#'   \item{\code{country_code}}{character World Bank country code.}
#'   \item{\code{year}}{character Year.}
#'   \item{\code{credit_rating}}{double Country credit rating, 0-100 best} 
#'}
#' @source World Bank Data 360. https://data360.worldbank.org/en/int/indicator/WEF_TTDI_INDCCREDITRATE
"credit_rating"

#' @title Population, Total
#' @description Total population is based on the de facto definition of population, which counts all residents regardless of legal status or citizenship. The values shown are midyear estimates.
#' @format A data frame with 17195 rows and 3 variables:
#' \describe{
#'   \item{\code{country_code}}{character World Bank country code.}
#'   \item{\code{year}}{double Year.}
#'   \item{\code{total_population}}{double Population, total.} 
#'}
#' @details World Bank Data 360. https://data360.worldbank.org/en/int/indicator/WB_WDI_SP_POP_TOTL
"population"

#' Labour income share (SDG 10.4.1), ILOSTAT
#'
#' Country-year estimates of the labour income share as a percent of GDP
#' from ILOSTAT (SDG indicator 10.4.1), annual series of modelled estimates.
#'
#' @format A tibble with one row per country (or aggregate) and year, with 4 variables:
#' \describe{
#'   \item{\code{country_code}}{Character. World Bank economy code from \code{countrycode(..., dest = "wb")}.
#'     Entries that are aggregates (e.g., regions, income groups, world) are \code{NA}.}
#'   \item{\code{year}}{Double. Reference year.}
#'   \item{\code{labor_income}}{Double. Labour income share as a percent of GDP.}
#'   \item{\code{status_label}}{Character. Observation status as provided by ILOSTAT
#'     (e.g., "Imputation", "Model-based extrapolation", or \code{NA}).}
#' }
#'
#' @details
#' - Source indicator: \code{SDG_1041_NOC_RT_A} (annual, percent of GDP).
#' - Retrieved via the ILO rplumber API and minimally transformed:
#'   country names mapped to World Bank codes, columns renamed, and status kept.
#' - Aggregates and regions in the source retain \code{NA} in \code{country_code};
#'   filter these out to keep only countries.
#'
#' @source ILOSTAT SDG 10.4.1 — Labour income share as a percent of GDP:
#'   https://ilostat.ilo.org/topics/labour-income/ • API:
#'   https://rplumber.ilo.org/data/indicator/?id=SDG_1041_NOC_RT_A
#'
#' @seealso \code{countrycode::countrycode()} for country code mappings,
#'   ILOSTAT indicator metadata for SDG 10.4.1.
"labor_income"

#' UN World Population Prospects 2024 - Income Group Population Growth Rates
#'
#' A dataset containing annual population growth rate projections by income group
#' for the period 2025-2035, extracted from the UN World Population Prospects 2024.
#'
#' @format A data frame with 7 columns:
#' \describe{
#'   \item{country_code}{Character. ISO 3166-1 alpha-3 country code.}
#'   \item{group_class}{Character. Income group or region classification
#'         (e.g. "High income", "Low income").}
#'   \item{type}{Character. Aggregation type, either Region or Income Group.}
#'   \item{year}{Numeric. Reference year of the projection (2025-2035).}
#'   \item{population_growth_rate}{Numeric. Annual population growth rate
#'         expressed as a percentage.}
#'   \item{total_population}{Numeric. Total population as of 1 January,
#'         in thousands.}
#'   \item{median_age}{Numeric. Median age of the population as of 1 July,
#'         in years.}
#' }
#'
#' @source UN Population Division, World Population Prospects 2024.
#'   Standard Projections, Most Used indicators.
#'   \url{https://population.un.org/wpp/downloads?folder=Standard%20Projections&group=Most%20used}
#'
#' @examples
#' \dontrun{
#'   data(unwpp_data)
#'   head(unwpp_data)
#' }
"unwpp_data"

#' Business Ready Dataset
#'
#' Scores for regulatory framework, public services, and operational efficiency by country.
#'
#' @format A data frame with 101 rows and 5 columns:
#' \describe{
#'   \item{economy}{Country name (character)}
#'   \item{country_code}{ISO country code (character)}
#'   \item{pillar_1_regulatory_framework}{Score for regulatory framework (numeric)}
#'   \item{pillar_2_public_services}{Score for public services (numeric)}
#'   \item{pillar_3_operational_efficiency}{Score for operational efficiency (numeric)}
#' }
#' @source World Bank B-READY. \url{https://www.worldbank.org/content/dam/sites/b-ready/documents/excel/B-READY_ALL_DATA_2025.zip}
"bready"

#' B-READY Topic-Level Pillar Scores
#'
#' B-READY pillar scores at the topic level, covering Labor, Taxation,
#' Dispute Resolution, and Market Competition.
#'
#' @format A data frame with 404 rows and 5 variables:
#' \describe{
#'   \item{\code{country_code}}{character. World Bank country code.}
#'   \item{\code{pillar_1_overall}}{double. Overall score for Pillar 1 (Regulatory Framework).}
#'   \item{\code{pillar_2_overall}}{double. Overall score for Pillar 2 (Public Services).}
#'   \item{\code{pillar_3_overall}}{double. Overall score for Pillar 3 (Operational Efficiency).}
#'   \item{\code{topic}}{character. Business environment topic: one of \code{"labor"},
#'     \code{"taxation"}, \code{"dispute_resolution"}, or \code{"market_competition"}.}
#' }
#' @source World Bank B-READY. \url{https://www.worldbank.org/content/dam/sites/b-ready/documents/excel/B-READY_ALL_DATA_2025.zip}
"bready_topic"

#' Budget Execution Rates
#'
#' Annual budget execution rates by country.
#'
#' @format A tibble with 2368 rows and 3 columns:
#' \describe{
#'   \item{country_code}{ISO country code (character)}
#'   \item{year}{Year (character)}
#'   \item{budget_execution_rate}{Budget execution rate (numeric)}
#' }
#' @source Internal compilation
"budget_execution"

#' Country Classification Table
#'
#' Country-level classification by region and income group.
#'
#' @format A data frame with 266 rows and 4 columns:
#' \describe{
#'   \item{economy}{Country name (character)}
#'   \item{country_code}{ISO country code (character)}
#'   \item{region}{World Bank region (character)}
#'   \item{income_group}{World Bank income group (character)}
#' }
#' @source World Bank and internal sources
"countryclass"

#' Global Survey of Public Sector (GSPS)
#'
#' Survey responses on public sector indicators by country, year, and topic.
#'
#' @format A tibble with 229,467 rows and 16 columns:
#' \describe{
#'   \item{country_code}{ISO country code (character)}
#'   \item{economy}{Country name (character)}
#'   \item{category}{Survey category (character)}
#'   \item{year}{Year (numeric)}
#'   \item{region}{World Bank region (character)}
#'   \item{income_group}{World Bank income group (character)}
#'   \item{respondent_group}{Respondent group (character)}
#'   \item{topic_group}{Topic group (character)}
#'   \item{indicator}{Indicator code (character)}
#'   \item{indicator_group}{Indicator group (character)}
#'   \item{question_text}{Survey question (character)}
#'   \item{mean}{Mean response (numeric)}
#'   \item{lower_ci}{Lower confidence interval (numeric)}
#'   \item{upper_ci}{Upper confidence interval (numeric)}
#'   \item{scale}{Response scale (character)}
#'   \item{response_rate}{Response rate (numeric)}
#' }
#' @source GSPS survey data
"gsps"

#' Open Budget Survey Scores
#'
#' Scores from the Open Budget Survey by country and year.
#'
#' @format A tibble with 918 rows and 7 columns:
#' \describe{
#'   \item{country_code}{ISO country code (character)}
#'   \item{year}{Year (numeric)}
#'   \item{budget_transparency_score}{Budget transparency score (numeric)}
#'   \item{supreme_audit_oversight_score}{Supreme audit institution oversight score (numeric)}
#'   \item{oversight_score}{Overall oversight score (numeric)}
#'   \item{legislative_oversight_score}{Legislative oversight score (numeric)}
#'   \item{public_participation_score}{Public participation score (numeric)}
#' }
#' @source International Budget Partnership
"open_budget"




