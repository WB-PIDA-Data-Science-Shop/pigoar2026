#' Download and extract a shapefile from a zipped URL
#'
#' This function downloads a ZIP file from a given URL, extracts it to a temporary
#' directory, locates the first `.shp` file, and returns its full path. It is designed
#' to be used with `sf::st_read()` to load shapefiles that cannot be read directly
#' from the URL (e.g., IBGE's geoftp).
#'
#' @param zip_url A character string. The URL to a `.zip` file containing a shapefile
#' (must include `.shp`, `.shx`, `.dbf`, and optionally `.prj`).
#'
#' @return A character string: the full path to the extracted `.shp` file.
#' This path can be passed directly to `sf::st_read()`.
#'
#' @examples
#' \dontrun{
#' library(sf)
#' url <- "https://geoftp.ibge.gov.br/map.shp"
#' shp_path <- get_shapefile(url)
#' br_muni <- st_read(shp_path, options = "ENCODING=WINDOWS-1252")
#' }
#'
#' @export
get_shapefile <- function(zip_url) {
  # Create temporary file and directory
  temp_zip <- tempfile(fileext = ".zip")
  temp_dir <- tempfile()
  dir.create(temp_dir)

  # Download the zip file
  download.file(zip_url, temp_zip, mode = "wb")

  # Unzip contents
  unzip(temp_zip, exdir = temp_dir)

  # Locate the .shp file (there may be multiple—return all or just first)
  shp_files <- list.files(temp_dir, pattern = "\\.shp$", full.names = TRUE)

  if (length(shp_files) == 0) {
    stop("No .shp file found in the zip archive.")
  }

  return(shp_files[1])
}