#' Calculate Correlation Between Well Water Level and Streamflow Data
#'
#' This function computes the correlation between changes in well water levels and streamflow for each divide (basin). It calculates standard deviations of well water levels and streamflow, computes correlation coefficients, and merges these correlations back into the original dataset.
#'
#' @param x A data frame containing columns for `Divide`, `mean_change_mm` (change in well water level), and `deltaSTR` (change in streamflow).
#'
#' @return A data frame where correlation coefficients between well water level changes and streamflow are computed for each divide. The output includes:
#' \describe{
#'   \item{Divide}{The divide ID.}
#'   \item{correlation}{The correlation coefficient between well water level changes and streamflow for each divide.}
#'   \item{plot_titles}{A character column containing plot titles with divide information and the rounded correlation values.}
#' }
#'
#' @details
#' The `Correlation` function performs the following steps:
#' \itemize{
#'   \item Groups the input data by `Divide`.
#'   \item Computes the standard deviations of well water level changes (`mean_change_mm`) and streamflow (`deltaSTR`) for each divide.
#'   \item Calculates the correlation between these variables for each divide, handling cases where standard deviations are zero by returning `NA` for the correlation.
#'   \item Merges the calculated correlation values back into the original dataset.
#'   \item Creates a `plot_titles` column with divide information and the rounded correlation value for plotting purposes.
#' }
#'
#' @export


Correlation <- function(x){
  
  Divide <- mean_change_mm <- deltaSTR <- sd_mean_change <- sd_deltaSTR <- correlation <- NULL
  
  ##Model correlation
  model_correlation_by_divide <- x %>%
    group_by(Divide) %>%
    summarize(
      sd_mean_change = sd(mean_change_mm, na.rm = TRUE),
      sd_deltaSTR = sd(deltaSTR, na.rm = TRUE),
      correlation = ifelse(sd_mean_change == 0 | sd_deltaSTR == 0, NA, cor(mean_change_mm, deltaSTR, use = "complete.obs"))
    ) %>%
    dplyr::select(Divide, correlation)
  
  # Merge the correlation coefficients back into the original data
  model_corr_join <- x %>%
    left_join(model_correlation_by_divide, by = "Divide") %>%
    mutate(plot_titles = paste0(Divide, "\nCorrelation: ", round(correlation, 3)))
  
  return(model_corr_join)
}