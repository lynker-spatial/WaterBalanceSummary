#' Balance Data
#'
#' This function transforms the input data from wide to long format, calculates errors, adjusts variable values as needed, and filters the data based on the variables used in water balance calculations. Specifically, it makes AET, RCH, and RUN negative so they can be subtracted from PPT rather than added. The processed data is then saved to the global environment under specific names depending on the variables present.
#'
#' @param x A data frame in wide format, typically containing columns for `divide_id`, `date`, `source`, and various water balance variables (e.g., `ppt`, `aet`, `rch`, `run`, `str`, `ERR`). The data should be the result of a previous processing step such as `model_clean()`.
#'
#' @return This function does not return a value. It saves the processed data to the global environment with the following names:
#' \itemize{
#'   \item **"cabcm_TimeSeries"**: Contains data with variables relevant to CABCM, including `aet`, `rch`, `run`, and `str` where applicable.
#'   \item **"terra_TimeSeries"**: Contains data with variables relevant to TerraClim, excluding `rch`.
#' }
#'
#' @details
#' The `balance_data` function performs the following operations:
#' \itemize{
#'   \item **Convert to Long Format**: Uses `pivot_longer` to transform the wide-format data into long format.
#'   \item **Filter and Adjust Data**: Filters the data to keep only the relevant variables and multiplies values for specific variables by -1 to facilitate subtraction from `ppt`.
#'   \item **Save to Global Environment**: Depending on the presence of the `rch` variable, the function saves the processed data to the global environment with an appropriate name for future use.
#' }
#' 
#'@export

balance_data <- function(x){


  divide_id <- var <- value <- NULL

  data_long <- x %>%
    pivot_longer(cols = -c(divide_id, date, source), names_to = "var", values_to = "value") %>%
    na.omit()
  if ("rch" %in% unique(data_long$var)) {
    data_long_balance <- data_long %>%
      filter(var %in% c("ppt", "aet", "rch", "run", "str", "ERR")) %>%
      mutate(value = if_else(var %in% c("aet", "rch", "run", "ERR"), value * -1, value))
  } else {
    data_long_balance <- data_long %>%
      filter(var %in% c("ppt", "aet", "run", "ERR", "str")) %>%
      mutate(value = if_else(var %in% c("aet", "run", "str"), value * -1, value))
  }

  #Assign Time Series Data to global Environment for later use
  cabcm_long_balance <- data_long_balance
  if ("rch" %in% unique(data_long$var)) {
    assign("cabcm_TimeSeries", data_long_balance %>%
             dplyr::select(divide_id, var, date, value, source), envir = WaterBalanceSummaryEnv)
  } else {
    assign("terra_TimeSeries", data_long_balance %>%
             dplyr::select(divide_id, var, date, value, source), envir = WaterBalanceSummaryEnv)
  }
}
