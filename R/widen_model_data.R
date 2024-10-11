#' Widen Model Data and Calculate Error
#'
#' This function processes the output of a cleaned model dataset by pivoting the data to a wide format and calculating the error (ERR) based on available variables. The error is calculated as the difference between `ppt`, `aet`, and other relevant variables such as `rch`, `run`, and `str`.
#'
#' @param data A data frame containing model output. The data should have columns for `var`, `value`, `ppt`, `aet`, `run`, `str`, and optionally `rch`.
#'
#' @return A data frame in wide format with an additional column `ERR`, representing the calculated error.
#'
#' @details
#' The function performs the following steps:
#' \itemize{
#'   \item **Pivot Wider**: Converts the data to wide format with separate columns for each variable.
#'   \item **Error Calculation**: Calculates the error (`ERR`) as `ppt - aet - rch - run - str` when `rch` is available, or `ppt - aet - run - str` when it's not.
#'   \item **Filter**: Filters out rows where `divide_id` is equal to `"cat-351"`.
#' }
#'
#' @note Ensure that the input data contains the required columns: `var`, `value`, `ppt`, `aet`, `run`, `str`, and optionally `rch`.
#'
#' @export


widen_model_data <- function(data) {
  
  
  var <- value <- ppt <- aet <- rch <- run <- str <- divide_id <- ERR <- NULL
  
  # Step 1: Pivot wider and calculate ERR
  data_wide <- data %>%
    pivot_wider(names_from = var, values_from = value)
  
  if ("rch" %in% names(data_wide)) {
    data_wide <- data_wide %>%
      mutate(ERR = ppt - aet - rch - run - str) %>%
      filter(divide_id != "cat-351")
  } else {
    data_wide <- data_wide %>%
      mutate(ERR = ppt - aet - run - str)
  }
  # Return a list of data frames for each season
  return(data_wide)
}