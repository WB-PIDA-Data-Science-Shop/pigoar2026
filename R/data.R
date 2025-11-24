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
#'   \item{\code{region}}{character. The ACLED-defined region in which the event occurred.}
#'   \item{\code{country}}{character. The country in which the event occurred.}
#'   \item{\code{country_code}}{World Bank country code}
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

#' @title Brazilian Municipality Shapefiles
#' @description Shapesfiles for municipal boundaries in Brazil (IBGE)
#' @format A data frame with 5573 rows and 16 variables:
#' \describe{
#'   \item{\code{municipality_code}}{character Municipality code (IBGE)}
#'   \item{\code{nm_mun}}{character Municipality name}
#'   \item{\code{cd_rgi}}{character Region code}
#'   \item{\code{nm_rgi}}{character Region name}
#'   \item{\code{cd_rgint}}{character COLUMN_DESCRIPTION}
#'   \item{\code{nm_rgint}}{character COLUMN_DESCRIPTION}
#'   \item{\code{cd_uf}}{character State code}
#'   \item{\code{nm_uf}}{character State name}
#'   \item{\code{sigla_uf}}{character State acronym}
#'   \item{\code{cd_regia}}{character COLUMN_DESCRIPTION}
#'   \item{\code{nm_regia}}{character COLUMN_DESCRIPTION}
#'   \item{\code{sigla_rg}}{character COLUMN_DESCRIPTION}
#'   \item{\code{cd_concu}}{character COLUMN_DESCRIPTION}
#'   \item{\code{nm_concu}}{character COLUMN_DESCRIPTION}
#'   \item{\code{area_km2}}{double COLUMN_DESCRIPTION}
#'   \item{\code{geometry}}{list COLUMN_DESCRIPTION}
#'}
#' @details DETAILS
"brazil_mun_shp"