#' Process Model Data
#'
#' This function processes the output of `model_clean()`. It widens the dataframe, calculates the error for each timestamp, assigns seasons based on month, calculates average error per season, and the percent error against PPT. It then joins the seasonal data with the `NewDivides` dataset to create a spatial dataframe, which is split into four seasonal datasets. The final output is a list containing seasonal data that can be used for generating Percent Error Plots based on the season. Additionally, the function assigns the processed data to the global environment.
#'
#' @param data A data frame resulting from the `model_clean()` function. It should contain columns such as `divide_id`, `var`, `date`, `value`, and `source`.
#' @param NewDivides A spatial dataframe containing divide information, which should be read from the `nextgen_hydrofabric.gpkg` file.
#'
#' @return A list containing four data frames: `Winter`, `Spring`, `Summer`, and `Fall`. Each dataframe includes average seasonal error and percent error data, joined with spatial information from `NewDivides`.
#'
#' @details
#' The `process_model_data` function performs the following steps:
#' \itemize{
#'   \item **Pivot Wider**: Reshapes the data to have separate columns for each variable and calculates the error (ERR) as the difference between predicted and actual values.
#'   \item **Assign to Global Environment**: Stores the wide-format data in the global environment under the name `cabcm_data_wide` or `terra_data_wide`, depending on the dataset.
#'   \item **Season Assignment**: Adds a season column to the data based on the month extracted from the date.
#'   \item **Calculate Seasonal Error**: Computes average seasonal error and percentage error.
#'   \item **Join and Split**: Joins the error data with `NewDivides`, and splits the data into seasonal dataframes.
#' }
#'



# Define the function
process_model_data <- function(data, NewDivides) {


  var <- value <- ppt <- aet <- rch <- run <- str <- divide_id <- season <- ERR <- mean_ppt <- mean_error <- NULL

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

  if ("rch" %in% names(data_wide)) {
    assign("cabcm_data_wide", data_wide, envir = WaterBalanceSummaryEnv)
  } else {
    assign("terra_data_wide", data_wide, envir = WaterBalanceSummaryEnv)
  }

  # Step 2: Define get_season function
  get_season <- function(month) {
    if (month %in% 3:5) {
      return("Spring")
    } else if (month %in% 6:8) {
      return("Summer")
    } else if (month %in% 9:11) {
      return("Fall")
    } else {
      return("Winter")
    }
  }

  # Step 3: Add season information to the data
  data_seasons <- data_wide %>%
    mutate(date = as.Date(date)) %>%  # Ensure 'date' is of Date class
    mutate(month = month(date)) %>%   # Extract month from 'date'
    mutate(season = sapply(month, get_season))  # Apply get_season function to each month


  # Step 4: Calculate seasonal error and percentage error
  Average_seasonal_error <- data_seasons %>%
    group_by(divide_id, season, source) %>%
    summarize(
      mean_error = mean(ERR, na.rm = TRUE),
      mean_ppt = mean(ppt, na.rm = TRUE),
      percent_error = ifelse(mean_ppt == 0, NA, (mean_error / mean_ppt)*100)
    )


  # Step 5: Join with NewDivides and split by seasons
  ErrorSeasons <- left_join(NewDivides, Average_seasonal_error, by = "divide_id")

  Winter <- ErrorSeasons %>% filter(season == "Winter")
  Spring <- ErrorSeasons %>% filter(season == "Spring")
  Summer <- ErrorSeasons %>% filter(season == "Summer")
  Fall <- ErrorSeasons %>% filter(season == "Fall")

  # Return a list of data frames for each season
  return(list(Winter = Winter, Spring = Spring, Summer = Summer, Fall = Fall))
}
