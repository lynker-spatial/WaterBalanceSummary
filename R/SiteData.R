#' Import and Clean Site Data for Spatial Analysis
#'
#' This function reads site location data, either provided as a data frame or loaded from the included package dataset, cleans it, and converts it into a simple features (SF) object for spatial analysis.
#'
#' @param x A data frame containing site location data with columns for station IDs, elevation, site names, and coordinates. If `NULL`, the function will load the `SiteLocations` dataset from the package.
#'
#' @return A simple features (SF) object with site data including coordinates and spatial reference.
#'
#' @details
#' The `SiteData` function performs the following steps:
#' \itemize{
#'   \item If no input data is provided (`x = NULL`), it loads the `SiteLocations` dataset from the `TNCDangermond` package.
#'   \item Converts `station_id_dendra`, `elev`, and `SiteName` columns to character type.
#'   \item Cleans up the `station_id_dendra` and `elev` columns, handling missing values and inconsistencies.
#'   \item Transforms `SiteName` to replace spaces with underscores.
#'   \item Filters rows where `SiteName` starts with "Dangermond" and excludes those containing "Weather".
#'   \item Converts the cleaned data to an SF object with coordinates defined by `lon` and `lat`, and sets the coordinate reference system (CRS) to EPSG:4269.
#' }
#'
#'
#' @export



#Clean Site Data and convert it to an SF object
  SiteData <- function(x = NULL){

    SiteLocations <- station_id_dendra <- elev <- SiteName <- NULL

    # Load default data if x is not provided
    if (is.null(x)) {
      data("SiteLocations", envir = environment())  # Load SiteLocations into the current environment
      x <- SiteLocations
    }

  #convert Station ID and Elev columns to characters so that they can be cleaned
  data <- x %>%
    mutate(station_id_dendra = as.character(station_id_dendra),
           elev = as.character(elev),
           SiteName = as.character(SiteName))

  #cleanup
  data <- data %>%
    mutate(station_id_dendra = if_else(is.na(station_id_dendra), elev, station_id_dendra)) %>%
    mutate(elev = if_else(elev == station_id_dendra, NA_character_, elev)) %>%
    mutate(SiteName = str_replace_all(SiteName, " ", "_"))
  data <- data %>%
    filter(str_starts(SiteName, "Dangermond")) %>%
    filter(!str_detect(SiteName, "Weather"))


  sf <- st_as_sf(data, coords = c('lon','lat'), crs = 4269)
}
