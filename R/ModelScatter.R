#' Create a Scatter Plot Comparing Well Water Level Change and Model Recharge
#'
#' This function generates a scatter plot comparing well water level changes (`mean_change_mm`)
#' with model-reported change in storage (`deltaSTR`) for different data sources.
#'
#' The scatter plot displays the relationship between well water level changes and model storage
#' changes, with separate facets for each set of plot titles. The plot helps in visualizing
#' how well the model predictions align with observed changes.
#'
#' @param x A data frame that must include:
#' \describe{
#'   \item{mean_change_mm}{The mean change in well water level in millimeters.}
#'   \item{deltaSTR}{The change in storage from the model.}
#'   \item{plot_titles}{Titles for facets, usually including divide ID and correlation coefficient.}
#'   \item{source}{The data source (e.g., CABCM, TerraClim).}
#' }
#'
#' @return A `ggplot` object showing the scatter plot comparing well water levels and model storage changes.





ModelScatter <- function(x){

mean_change_mm <- deltaSTR <- NULL

  title <- dplyr::case_when(
    "cabcm_v8" %in% unique(x$source) ~ "Dangermond vs. CABCM",
    "terraclim" %in% unique(x$source) ~ "Dangermond vs. TerraClim",
    TRUE ~ "Dangermond vs. NextGen"
  )
  scatterplot <- ggplot()+
    geom_point(x, mapping = aes(x = mean_change_mm, y = deltaSTR), color = "black")+
    facet_wrap(~plot_titles, scales = "free")+
    theme_gray()+
    labs(title = title,
         x = "Delta Well Water Level (mm)",
         y = "Recharge (mm)")
  scatterplot
}
