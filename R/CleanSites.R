#' Clean and Process Site Data for Spatial Analysis
#'
#' This function creates and processes spatial layers for divides, flow paths, and sites that intersect with divides. It reads spatial data from a GeoPackage and performs spatial operations such as intersection and joining.
#'
#' @param x A simple features (SF) object representing site locations.
#'
#' @return This function does not return a value. Instead, it assigns three spatial layers to the global environment:
#' \itemize{
#'   \item `NewDivides`: A simple features (SF) object representing divides, transformed to CRS EPSG:4269.
#'   \item `dangermond_sites`: The intersection of the provided site locations with the divides.
#'   \item `dangermond_sites`: The result of joining `dangermond_sites` with `NewDivides`.
#' }
#'
#' @details
#' The `CleanSites` function performs the following steps:
#' \itemize{
#'   \item Reads spatial data from the `nextgen_hydorfabric.gpkg` GeoPackage, specifically the `divides` layer.
#'   \item Transforms the CRS of the `divides` layer to EPSG:4269 and stores it in a variable `NewDivides`.
#'   \item Computes the intersection of the input site data (`x`) with `NewDivides` and stores the result in `dangermond_sites`.
#'   \item Joins the intersected sites (`dangermond_sites`) with `NewDivides` and updates the `dangermond_sites` variable.
#' }
#'
#'
#' @export

CleanSites <- function(x){

  divide_id <- NewDivides <- NULL
  assign("NewDivides", st_read(dsn = "/Users/wamclean/Desktop/Lynker/jldp_ngen_nhdhr.gpkg", layer = "divides") %>% mutate(divide_id = as.character(divide_id)) %>%
           st_transform(crs = 4269), envir = WaterBalanceSummaryEnv)

  # assign("dangermond_sites", st_intersection(x, NewDivides), envir = WaterBalanceSummaryEnv)
  #
  # assign("dangermond_sites", st_join(dangermond_sites, NewDivides), envir = WaterBalanceSummaryEnv)
}
