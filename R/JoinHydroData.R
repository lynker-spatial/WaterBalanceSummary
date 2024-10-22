#' Join Well Water Level and Streamflow Data
#'
#' This function merges well water level data with streamflow data for further analysis. The streamflow data is filtered to include only non-negative values and specific date ranges. The function also limits the data to basins present in the `UniqueDivides` dataset.
#'
#' @param x A data frame containing streamflow data, which includes columns such as `var`, `value`, `date`, and `divide_id`.
#' @param y A data frame containing well water level data, such as `monthly_basin_average`. This dataset includes columns for `date`, `Divide`, and `mean_change_mm`.
#' @param z A data frame with unique divides (basins) to be used as a filter. Defaults to `UniqueDivides`.
#'
#' @return A data frame resulting from the inner join of the well water level data and streamflow data. The output includes the combined columns from both datasets, filtered by divide and date.
#'
#' @details
#' The `JoinHydroData` function performs the following steps:
#' \itemize{
#'   \item Filters the streamflow data (`x`) to retain only non-negative values of `var == "str"`.
#'   \item Renames the `divide_id` column to `Divide` in both datasets.
#'   \item Filters the streamflow data to include only basins present in the `UniqueDivides` data frame (`z`).
#'   \item Restricts the data to dates on or after "2009-10-27".
#'   \item Performs an inner join between the well water level data (`y`) and streamflow data (`x`) based on `date` and `Divide`.
#' }
#'
#' @export
JoinHydroData <- function(x, y = monthly_basin_average, z = UniqueDivides){
  
  monthly_basin_average <- var <- value <- divide_id <- Divide <- UniqueDivides <- mean_change_mm <- deltaSTR <- sd_mean_change <- sd_deltaSTR <- correlation <- NULL
  
  # Filter streamflow data
  model_str <- x %>%
    filter(var == "str") %>%
    filter(value >= 0)
  
  # Prepare the streamflow data
  model_str <- model_str %>%
    mutate(date = as.Date(date)) %>%
    rename(Divide = divide_id) %>%
    filter(Divide %in% z$Divide) %>%
    filter(date >= "2009-10-27")
  
  # Rename value to deltaSTR
  model_str <- model_str %>%
    rename(deltaSTR = value)
  
  # Perform the inner join between well water level and streamflow data
  model_dangermond_join <- inner_join(y, model_str, by = c("date", "Divide"))
  
  return(model_dangermond_join)
}
