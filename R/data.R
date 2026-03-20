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

#' @title World Bank Map 
#' @description A shapefile containing all relevant country boundaries and disputed areas used by the World Bank.
#' @format A data frame with 526 rows and 2 variables:
#' \describe{
#'   \item{\code{country_code}}{character World Bank country code}
#'   \item{\code{geometry}}{list Geometry of the country boundaries}
#'}
#' @details This dataset includes both the official country boundaries and any disputed areas as recognized by the World Bank.
#' @source https://datacatalogfiles.worldbank.org/ddh-published/0038272/5/DR0095369/World%20Bank%20Official%20Boundaries%20(GeoJSON)/World%20Bank%20Official%20Boundaries%20-%20Admin%200.geojson
"wb_map"

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

#' Brazilian municipality boundaries (IBGE)
#'
#' Municipal boundary polygons for Brazil from IBGE, suitable for subnational
#' analysis and mapping. Provided as an `sf` object with attributes for region,
#' state, and municipality identifiers.
#'
#' @format An `sf` data frame with 5,573 rows (municipalities) and 16 variables:
#' \describe{
#'   \item{\code{municipality_code}}{Character. IBGE 7-digit municipality code.}
#'   \item{\code{nm_mun}}{Character. Municipality name.}
#'   \item{\code{cd_rgi}}{Character. Immediate geographic region code (IBGE).}
#'   \item{\code{nm_rgi}}{Character. Immediate geographic region name.}
#'   \item{\code{cd_rgint}}{Character. Intermediate geographic region code (IBGE).}
#'   \item{\code{nm_rgint}}{Character. Intermediate geographic region name.}
#'   \item{\code{cd_uf}}{Character. State code (IBGE).}
#'   \item{\code{nm_uf}}{Character. State name.}
#'   \item{\code{sigla_uf}}{Character. State acronym (UF).}
#'   \item{\code{cd_regia}}{Character. Macro-region code (IBGE).}
#'   \item{\code{nm_regia}}{Character. Macro-region name (IBGE).}
#'   \item{\code{sigla_rg}}{Character. Macro-region acronym (if applicable).}
#'   \item{\code{cd_concu}}{Character. Mesoregion/microregion legacy code (if present).}
#'   \item{\code{nm_concu}}{Character. Mesoregion/microregion legacy name (if present).}
#'   \item{\code{area_km2}}{Double. Municipality area in square kilometers.}
#'   \item{\code{geometry}}{List-column. Simple features polygon geometry (EPSG:4674).}
#' }
#'
#' @details
#' - Coordinate reference system (CRS): SIRGAS 2000 (EPSG:4674, geographic).
#' - Geometry type: MULTIPOLYGON/POLYGON; includes islands and multipart features.
#' - Source data from IBGE’s official municipal boundary dataset; lightly cleaned
#'   and renamed for consistency. Attributes follow IBGE’s regional hierarchy:
#'   macro-region > state (UF) > intermediate > immediate > municipality.
#'
#' @source Instituto Brasileiro de Geografia e Estatística (IBGE) — Malha Municipal.
#'   https://www.ibge.gov.br/geociencias/organizacao-do-territorio/malhas-territoriais.html
"brazil_mun_shp"

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

#' World Bank Documents API: 2025 document catalog (flattened)
#'
#' A tibble of documents retrieved from the World Bank Documents & Reports API,
#' with authors flattened to a single semicolon-separated string per document
#' and selected metadata fields standardized. Data were fetched for the period
#' 2025-01-01 to 2025-12-31 using the v3 API.
#'
#' @format A tibble with one row per document and the following columns:
#' \describe{
#'   \item{document_id}{Character. Unique document identifier (id).}
#'   \item{authors}{Character. Semicolon-separated list of authors extracted from the nested authors/authr field.}
#'   \item{count}{Character. Country or count field as returned by the API (often country name).}
#'   \item{doc_type}{Character. Document type (docty).}
#'   \item{theme}{Character. Comma-separated themes associated with the document.}
#'   \item{theme_category}{Character. Tab separated thematic categories associated with the document. Programmatically encoded using World Bank theme taxonomy.}
#'   \item{lang}{Character. Language code/name.}
#'   \item{doc_date}{Character or date-time string. Document date (docdt) as returned by the API.}
#'   \item{display_title}{Character. Human-readable title.}
#'   \item{pdfurl}{Character. Direct URL to the PDF if available.}
#'   \item{projectid}{Character. Project identifier when applicable.}
#'   \item{guid}{Character. Global unique identifier.}
#'   \item{url}{Character. Landing page URL.}
#'   \item{orig_unit}{Character. Originating unit (origu).}
#'   \item{owner}{Character. Owning unit/department.}
#'   \item{gov_unit}{Numeric. Flag for whether the owning unit is mapped to Governance.}
#'   \item{abstract}{Character. Abstract text(s) as returned by the API; may include multiple language versions.}
#' }
#'
#' @details
#' - Data are retrieved via the World Bank Documents & Reports Search API (v3).
#' - The nested `authors`/`authr` field is collapsed to a single character string
#'   per document using semicolons as separators.
#'
#' @source World Bank Documents & Reports API:
#'   https://documents.worldbank.org/en/publication/documents-reports/api
#'
#' @seealso
#' - API search endpoint: https://search.worldbank.org/api/v3/wds
#' - API field list parameter (`fl`) for selecting returned fields
#'
"wb_documents"

#' Governance and Institutional Units mapping
#'
#' A tibble mapping World Bank owning units (from Documents & Reports API)
#' to their short unit codes extracted from the unit label.
#'
#' @format A tibble with 2 columns:
#' \describe{
#'   \item{owner}{Character. Owning unit label from the API (e.g., "EFI-AFR1-GOV-FM & PS-1 (EAEG1)").}
#'   \item{owner_code}{Character. Short unit code parsed from parentheses (e.g., "EAEG1").}
#' }
#'
#' @source Derived from World Bank Documents & Reports API unit labels.
"gov_unit"

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

#' @title Global Survey of Public Servants
#' @description This dataset is a set of surveys of public servants produced by the Bureaucracy Lab at the World Bank and partnering academic institutions.
#' @format A data frame with 229467 rows and 10 variables:
#' \describe{
#'   \item{\code{country_code}}{World Bank country code}
#'   \item{\code{economy}}{character Country name}
#'   \item{\code{category}}{character Category name}
#'   \item{\code{year}}{double Year}
#'   \item{\code{region}}{character World Bank region}
#'   \item{\code{income_group}}{character World Bank income group}
#'   \item{\code{respondent_group}}{character Respondent group}
#'   \item{\code{topic_group}}{character Topic grouping}
#'   \item{\code{indicator}}{character Indicator}
#'   \item{\code{indicator_group}}{character Indicator grouping}
#'   \item{\code{question_text}}{character Survey question}
#'   \item{\code{mean}}{double Average for the group. See scale}
#'   \item{\code{lower_ci}}{double Lower bound for the average}
#'   \item{\code{upper_ci}}{double Upper bound for the average}
#'   \item{\code{scale}}{character Scale for the average}
#'   \item{\code{response_rate}}{double Response rate for the group}
#'}
#' @source <https://www.globalsurveyofpublicservants.org/data-downloads>
"gsps"

#' World Bank Country and Lending Groups
#'
#' This dataset is produced by the World Bank Group to classify countries as to their income levels and other groups.
#'
#' @format ## `countryclass`
#' A data frame with 267 rows and 4 columns:
#' \describe{
#'   \item{country_code}{World Bank country code}
#'   \item{economy}{Country name}
#'   \item{region}{World Bank region}
#'   \item{income_group}{World Bank income classification}
#'   ...
#' }
#' @source <https://ddh-openapi.worldbank.org/resources/DR0095333/download/>
"countryclass"

#' @title Budget Execution Rate
#' @description Primary government expenditures as a proportion of original approved budget (%)
#' @source <https://data360.worldbank.org/en/indicator/WB_WDI_GF_XPD_BUDG_ZS>
#' @format A data frame with 3154 rows and 3 variables:
#' \describe{
#'   \item{\code{country_code}}{character World Bank country code}
#'   \item{\code{year}}{character Year}
#'   \item{\code{budget_execution_rate}}{double Primary government expenditure (%)}
#'}
"budget_execution"

#' @title Open Budget Survey: Legislature and Super Audit Institution Oversight Score
#' @description The role that legislatures and supreme audit institutions play in the budget process and the extent to which they are able to provide robust oversight of the budget.
#' @format A data frame with 918 rows and 8 variables:
#' \describe{
#'   \item{\code{country_code}}{character World Bank country code}
#'   \item{\code{year}}{double Year}
#'   \item{\code{budget_transparency_score}}{double Budget transparency score Budget transparency score (previously known as the Open Budget Index): assesses the public availability of the eight key budget documents, which taken together provide a complete view of how public resources have been raised, planned, and spent during the budget year. To be considered "publicly available", documents must be published online, in a timely manner, and must include information that is comprehensive and useful. A score of 61 or above indicates a country is likely publishing enough material to support informed public debate on the budget.}
#'   \item{\code{supreme_audit_oversight_score}}{double Supreme Audit Institution Oversight Score: The role that supreme audit institutions play in the budget process and the extent to which they are able to provide robust oversight of the budget.}
#'   \item{\code{oversight_score}}{double Legislature and Supreme Audit Institution Oversight Score: The role that legislatures and supreme audit institutions play in the budget process and the extent to which they are able to provide robust oversight of the budget.}
#'   \item{\code{legislative_oversight_score}}{double Legislature oversight score: The role that legislatures play in the budget process and the extent to which they are able to provide robust oversight of the budget.}
#'   \item{\code{public_participation_score}}{double Public participation score: The degree to which the executive, the legislature, and the supreme audit institution each provides opportunities for the public to engage during different cycles of the budget process.}
#'}
#' @details DETAILS
"open_budget"