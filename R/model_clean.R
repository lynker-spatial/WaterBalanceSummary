#' Clean and Process Model Data
#'
#' This function processes the model data by separating soil moisture storage from the dataset, calculating changes in storage, and combining this information back with the original data.
#'
#' @param x A data frame acquired from the `CABCMParquetRead()` or `TerraClimParquetRead()` function. This data frame should include columns for `divide_id`, `var`, `date`, `value`, and `source`.
#'
#' @return A data frame containing the combined and cleaned data. The dataset includes:
#' \itemize{
#'   \item Data points where `var` is not "str", including columns `divide_id`, `var`, `date`, `value`, and `source`.
#'   \item Calculated changes in soil moisture storage where `var` is "str", which is computed as the day-to-day difference in `value`.
#'   \item The combined data with appropriate renaming and omission of missing values.
#' }
#'
#' @details
#' The `model_clean` function performs the following steps:
#' \itemize{
#'   \item Filters out data points where `var` is not "str" and retains only those columns needed.
#'   \item For data points where `var` is "str", calculates the day-to-day change in soil moisture storage.
#'   \item Renames the calculated changes and combines this cleaned data with the original dataset.
#' }
#'





model_clean <- function(x){

  divide_id <- var <- value <- day_to_day_change <- NULL

  no_str_data <- x %>%
    dplyr::select(divide_id, var, date, value, source) %>%
    mutate(date = as.Date(date)) %>%
    filter(var != "str") %>%
    na.omit(value)

  change_in_storage <- x %>%
    filter(var == "str") %>%
    mutate(date = as.Date(date)) %>%
    arrange(divide_id, date) %>%
    group_by(divide_id) %>%
    mutate(day_to_day_change = value - lag(value, default = first(value))) %>%
    ungroup()

  change_in_storage_clean <- change_in_storage %>%
    dplyr::select(divide_id, var, date, day_to_day_change, source)
  change_in_storage_clean <- change_in_storage_clean %>%
    dplyr::rename(value = day_to_day_change) %>%
    na.omit(value)

  data_delta_str <- bind_rows(no_str_data, change_in_storage_clean)

  return(data_delta_str)
}
