#' Create a Map of Correlations Between Well Water Level and Model Recharge
#'
#' This function generates a map visualizing the correlation between well water level changes and
#' model-reported recharge. It colors divides based on whether the correlation is positive or negative.
#' It also combines this map with a scatter plot showing the relationship between well water levels and
#' model recharge.
#'
#' The function takes in the output from the `Correlation()` function and uses it to create a map that
#' highlights divides with positive and negative correlations. It adds labels and includes various map
#' features like scale and north arrow. The combined map and scatter plot provide a comprehensive view
#' of the correlation analysis.
#'
#' @param x A data frame resulting from the `Correlation()` function, containing:
#' \describe{
#'   \item{Divide}{The divide ID.}
#'   \item{correlation}{The correlation coefficient between `mean_change_mm` and `deltaSTR`.}
#' }
#' @param y A `ggplot` object produced by the `ModelScatter` function, representing the scatter plot
#'          of well water level change vs. model recharge.
#' @param z A spatial data frame (sf object) with divide information, including the `divide_id` column
#'
#' @return A combined `ggplot` object with the correlation map and scatter plot side by side.
#'
#'
#'@export




ModelCorrMap <- function(x, y = ModelScatterPlot, z = NewDivides){
  
  ModelScatterPlot <- NewDivides <- divide_id <- geom <- correlation <- Divide <- toid <- type <- ds_id <- areasqkm <- id <- lengthkm <- tot_drainage_areasqkm <- has_flowline <- pos_neg <- geometry <- fill_var <- NULL
  
  z <- z %>% 
    rename(Divide = divide_id, geometry = geom)
  
  #Join with input data
  corr_divides <- left_join(z, x, by = "Divide")
  
  #Add a column to designate if the correlation is positive or negative and drop non essential fields (retaining spatial ones incase needed for future analysis)
  corr_divides <- corr_divides %>%
    mutate(pos_neg = ifelse(correlation > 0, "pos", "neg")) %>%
    dplyr::select(Divide, toid, type, ds_id, areasqkm, id, lengthkm, tot_drainage_areasqkm, has_flowline, correlation, pos_neg, geometry) %>%
    filter(Divide != "cat-52")
  
  #Add a column for fill variables when mapping
  corr_divides <- corr_divides %>%
    mutate(fill_var = case_when(
      pos_neg == "pos" ~ "Positive",
      pos_neg == "neg" ~ "Negative",
      TRUE ~ "Other" # Assuming other conditions exist or you can use NA for no fill
    )) %>%
    mutate(fill_var = factor(fill_var, levels = c("Positive", "Negative", "Other"))) %>% 
    st_as_sf()
  
  #Get centroids of divides
  corr_divides_centroids <- st_centroid(corr_divides) %>% 
    st_as_sf() %>% 
    distinct(Divide, .keep_all = TRUE)
  
  #assign("corr_divides_centroids", corr_divides_centroids, envir = .GlobalEnv)
  
  #Create a map of the divides with Positive and Negative color coding, to be paired with the scatter plots produced above
  corr_map <- ggplot()+
    annotation_map_tile(type = "osm", zoomin = 1) +
    xlab("Longitude") + ylab("Latitude") +
    coord_sf(crs = st_crs(4269)) +
    ggtitle("Correlation of deltaWWL and Model Recharge \n")+
    geom_sf(data = z)+
    #geom_sf(data = ActiveDivides, color = "grey", aes(fill = has_flowline)) +
    geom_sf(data = corr_divides , aes(fill = fill_var), color = "grey") +
    geom_sf(data = All_Sites_sf, aes(color = "Sites"), size = 0.5) +
    geom_label_repel(data = filter(corr_divides_centroids, !is.na(correlation)),
                     aes(geometry = geometry, label = Divide),
                     stat = "sf_coordinates", size = 2, color = "black",
                     box.padding = 0.5, segment.color = "grey50",
                     force = 1, segment.size = 0.5) +  # Adjust parameters as needed
    scale_fill_manual(values = c("Positive" = "green", "Negative" = "red", "Other" = "grey90"),
                      name = NULL,
                      labels = c("Positive Correlation", "Negative Correlation", "Other Divides"),
                      na.translate = FALSE) +
    scale_color_manual(values = c("Sites" = "black"),
                       name = "",
                       labels = c("TNC Sensors")) +
    annotation_scale(location = "br", width_hint = 0.29)+
    annotation_north_arrow(location = "br", which_north = "true",
                           pad_x = unit(0, "in"), pad_y = unit(0.25, "in"),
                           style = north_arrow_fancy_orienteering)+
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "right")
  
  #Combine Scatter Plot and Map
  
  ScatterCorrPlot <- grid.arrange(
    ggplotGrob(y),
    ggplotGrob(corr_map),
    ncol = 2, nrow = 1
  )
}