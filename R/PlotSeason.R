#' Plot Seasons
#'
#' This function generates standardized plots for the Dangermond Preserve based on seasonal data. The plots visualize the percent error across different spatial features for a given season. This function is designed to be used within the `GridSeasons()` function to arrange the plots in a grid layout.
#'
#' @param SeasonData A spatial dataframe containing seasonal data with columns such as `percent_error` and `type`. This data should be output from the `process_model_data()` function.
#' @param season_name A character string representing the name of the season (e.g., "Winter", "Spring", "Summer", "Fall"). This will be used as the title of the plot.
#'
#' @return A `ggplot` object displaying the percent error across the spatial features of the Dangermond Preserve for the specified season.
#'
#' @details
#' The `PlotSeason` function performs the following operations:
#' \itemize{
#'   \item **Plot Creation**: Generates a ggplot object with a map tile background, using OpenStreetMap data.
#'   \item **Coordinate System**: Uses the spatial coordinate reference system `st_crs(26910)`.
#'   \item **Visualization**: Plots the spatial data with `percent_error` values visualized using a gradient color scale from white to dark red.
#'   \item **Title**: Sets the plot title to the provided `season_name`.
#' }



PlotSeason <- function(SeasonData, season_name) {

  type <- percent_error <- NULL

  ggplot() +
    annotation_map_tile(type = "osm", zoomin = 1) +
    coord_sf(crs = st_crs(26910)) +
    ggtitle(glue("{season_name}")) +  # Use the season name for the title
    geom_sf(data = filter(SeasonData, type != "coastal"), color = "grey", aes(fill = percent_error)) +
    labs(fill = "Percent Error") +
    scale_fill_gradient(low = "white", high = "red4") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          plot.title = element_text(face = "bold"),
          plot.subtitle = element_text(hjust = 0.5))
}
