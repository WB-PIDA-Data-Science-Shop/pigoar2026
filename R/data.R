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