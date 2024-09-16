#' Select Basin
#'
#' This function filters a dataset of divides (`NewDivides`) by a specified `divide_id` and selects the corresponding basin. It then finds the sites within that basin from the dataset `All_Sites_sf`.
#'
#' @param divide A character string representing the `divide_id` to filter by. Default is an empty string.
#' @return A data frame containing the sites (`All_Sites_sf`) that are within the specified basin.



# Function to select basin
select_basin <- function(divide = "") {

  NewDivides <- divide_id <- st_intersects <- All_Sites_sf <- NULL

  divide_selection <- NewDivides %>%
    filter(divide_id == divide)

  logi_point_in_div <- st_intersects(divide_selection, All_Sites_sf, sparse = FALSE)
  site_in_div <- All_Sites_sf[as.vector(logi_point_in_div), ]

  return(site_in_div)
}
