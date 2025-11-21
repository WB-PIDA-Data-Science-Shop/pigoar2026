#' World Bank Regional Map Projections
#'
#' Provides a unified interface to retrieve region-specific map projections
#' commonly used in World Bank geospatial visualization work. Each region uses
#' a Lambert Azimuthal Equal-Area (LAEA) projection centered on the region,
#' except "World", which uses the Robinson projection, consistent with
#' World Bank Atlas conventions.
#'
#' @details
#' Supported World Bank regions:
#' \itemize{
#'   \item East Asia and Pacific
#'   \item Europe and Central Asia
#'   \item Latin America and Caribbean
#'   \item Middle East and North Africa
#'   \item North America
#'   \item South Asia
#'   \item Sub-Saharan Africa
#'   \item World
#' }
#'
#' Internally, region names must match exactly, but users can pass any value
#' returned by the \code{wb_region} field of WDI, poverty, ACLED, or HRMIS datasets.
#'
#' @param wb_region A character string specifying the World Bank region.
#' @return A \code{coord_sf()} object with an appropriate CRS for the region.
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#' ggplot() +
#'   geom_sf(data = eap_countries) +
#'   get_wb_projection("East Asia and Pacific")
#' }
#'
#' @export
get_wb_projection <- function(wb_region) {

  wb_projections <- list(
    "East Asia & Pacific" = ggplot2::coord_sf(
      crs = "+proj=laea +lat_0=20 +lon_0=110"
    ),
    "Europe & Central Asia" = ggplot2::coord_sf(
      crs = "+proj=laea +lat_0=55 +lon_0=15"
    ),
    "Latin America & Caribbean" = ggplot2::coord_sf(
      crs = "+proj=laea +lat_0=-15 +lon_0=-70"
    ),
    "Middle East & North Africa" = ggplot2::coord_sf(
      crs = "+proj=laea +lat_0=28 +lon_0=35"
    ),
    "North America" = ggplot2::coord_sf(
      crs = "+proj=laea +lat_0=50 +lon_0=-100"
    ),
    "South Asia" = ggplot2::coord_sf(
      crs = "+proj=laea +lat_0=20 +lon_0=80"
    ),
    "Sub-Saharan Africa" = ggplot2::coord_sf(
      crs = "+proj=laea +lat_0=0 +lon_0=20"
    ),
    "World" = ggplot2::coord_sf(
      crs = "+proj=robin"  # WB Atlas standard
    )
  )

  if (!wb_region %in% names(wb_projections)) {
    stop(
      "Region not recognized. Must be one of:\n",
      paste(names(wb_projections), collapse = ", "),
      call. = FALSE
    )
  }

  wb_projections[[wb_region]]
}

#' Plot Regional Demonstration Map
#'
#' Creates a World Bank–style map for a given region with demonstration events overlaid.
#'
#' @param wb_region Character. World Bank region to plot (e.g., "East Asia and Pacific").
#' @param data_region Data frame or tibble of point events with at least:
#'   \code{country_code}, \code{total_events}, and \code{region}.
#'
#' @return A \code{ggplot} object showing the regional map and events.
#'
#' @examples
#' \dontrun{
#' plot_regional_map("East Asia and Pacific", acled_asia_sf)
#' plot_regional_map("Sub-Saharan Africa", acled_africa_sf)
#' }
#'
#' @export
plot_regional_map <- function(wb_region, data_region){
  wb_map_region_sf <- pigoar2026::wb_map |> 
    left_join(
      wb_income_and_region,
      by = c("country_code")
    ) |> 
    filter(
      region == wb_region
    ) 

  map_out <- wb_map_region_sf |> 
    ggplot() +
    geom_sf(
      fill = "orange2", color = "white"
    ) +
    geom_sf(
      aes(
        size = total_events
      ),
      data = data_region |> filter(region == wb_region), color = "steelblue3", alpha = 0.6
    ) +
    scale_size_continuous(range = c(1, 10)) +
    labs(
      title = sprintf("Demonstrations in %s (2019-2024)", wb_region),
      x = "Longitude",
      y = "Latitude"
    ) +
    theme_void() +
    theme(
      legend.position = "bottom"
    ) +
    get_wb_projection(wb_region)

  return(map_out)
}
