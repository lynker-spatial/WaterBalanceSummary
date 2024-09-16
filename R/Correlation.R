#' Calculate Correlation Between Well Water Level Change and Soil Moisture Storage Change
#'
#' This function calculates the correlation between well water level changes and storage changes (e.g., from cabcm_ or terra_delta_str) for different basins. It joins the data, computes correlation statistics, and prepares the data for further analysis.
#'
#' @param x A data frame containing storage change data with columns including `var`, `value`, `date`, and `divide_id`. Defaults to `cabcm_delta_str` or `terra_delta_str` for different datasets.
#' @param y A data frame with monthly averages of well water level changes, typically produced by the `CleanWells` function. Defaults to `monthly_basin_average`.
#'
#' @return A data frame with the following columns:
#' \describe{
#'   \item{Divide}{The divide ID.}
#'   \item{date}{The date of the observation.}
#'   \item{mean_change_mm}{The mean change in well water level in millimeters.}
#'   \item{deltaSTR}{The change in storage from the storage change data.}
#'   \item{correlation}{The correlation coefficient between `mean_change_mm` and `deltaSTR`.}
#'   \item{plot_titles}{Titles for plots including divide ID and correlation coefficient.}
#' }
#'
#'
#'
#' @export


Correlation <- function(x, y = monthly_basin_average){

monthly_basin_average <- var <- value <- divide_id <- Divide <- UniqueDivides <- mean_change_mm <- deltaSTR <- sd_mean_change <- sd_deltaSTR <- correlation <- NULL

  model_str <- x %>%
    filter(var == "str") %>%
    filter(value >= 0)

  model_str <- model_str %>%
    mutate(date = as.Date(date)) %>%
    rename(Divide = divide_id) %>%
    filter(Divide %in% UniqueDivides$Divide) %>%
    filter(date >= "2009-10-27")

  model_str <- model_str %>%
    rename(deltaSTR = value)

  model_dangermond_join <- inner_join(y, model_str, by = c("date", "Divide"))

  ##Model correlation
  model_correlation_by_divide <- model_dangermond_join %>%
    group_by(Divide) %>%
    summarize(
      sd_mean_change = sd(mean_change_mm, na.rm = TRUE),
      sd_deltaSTR = sd(deltaSTR, na.rm = TRUE),
      correlation = ifelse(sd_mean_change == 0 | sd_deltaSTR == 0, NA, cor(mean_change_mm, deltaSTR, use = "complete.obs"))
    ) %>%
    dplyr::select(Divide, correlation)

  assign("model_correlation_by_divide", model_correlation_by_divide, envir = WaterBalanceSummaryEnv)

  # Merge the correlation coefficients back into the original data
  model_dangermond_join <- model_dangermond_join %>%
    left_join(model_correlation_by_divide, by = "Divide") %>%
    mutate(plot_titles = paste0(Divide, "\nCorrelation: ", round(correlation, 3)))



  return(model_dangermond_join)
}
