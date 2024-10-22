#' Plot Change in Well Water Level vs Model Recharge
#'
#' This function generates a bar plot comparing well water level changes (`mean_change_mm`)
#' with model-reported change in storage (`deltaSTR`) for different divides.
#'
#' The plot displays well water level changes and model-reported storage changes over time,
#' with separate facets for each divide. The fill color distinguishes between recharge and
#' delta storage.
#'
#' @param x A data frame that must include:
#' \describe{
#'   \item{date}{The date of the observation.}
#'   \item{mean_change_mm}{The mean change in well water level in millimeters.}
#'   \item{deltaSTR}{The change in storage from the model.}
#'   \item{Divide}{The divide ID.}
#'   \item{source}{The data source (e.g., CABCM, TerraClim).}
#' }
#'
#' @return A `ggplot` object showing the bar plot with well water levels and model storage changes.
#'@export


Plot_WWLvsSTR <- function(x){

SiteLocations <- station_id_dendra <- elev <- SiteName <- mean_change_mm <- deltaSTR <- NULL

  subtitle <- dplyr::case_when(
    "cabcm_v8" %in% unique(x$source) ~ "Dangermond Sites vs. CABCM",
    "terraclim" %in% unique(x$source) ~ "Dangermond Sites vs. TerraClim",
    TRUE ~ "Dangermond Sites vs. NextGen"
  )

  WWLvsSTR <- ggplot(data = x)+
    geom_col(data = filter(x, mean_change_mm >= 0),
             mapping = aes(x = date, y = mean_change_mm, fill = "Recharge"), color = "#039dfc")+
    geom_col(data = filter(x, deltaSTR >= 0),
             mapping = aes(x = date, y = deltaSTR, fill = "Delta STR (model)"), alpha = .8)+
    facet_wrap(~Divide, ncol = 5, scales = "free_y")+
    labs(title = "Well Water Level vs. Recharge 2009 - 2018",
         subtitle = subtitle,
         x = "Date",
         y = "mm",
         fill = ""
    )+
    scale_fill_manual(values = c("Recharge" = "#039dfc", "Delta STR" = "#959595"),
                      labels = c("Recharge" = "Delta WWL", "Delta STR" = "Delta Soil Moisture")) +
    theme_gray()+
    theme(legend.position = "bottom",
          axis.text.x = element_text(angle = 45, hjust = 1)
    )

  return(WWLvsSTR)
}
