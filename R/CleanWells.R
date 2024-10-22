#' Clean and Process Well Water Level Data
#'
#' This function filters, transforms, and processes well water level data from a combined dataset. It prepares the data for mapping and analysis by calculating changes in well water levels, joining with basin information, and summarizing the data on a monthly basis.
#'
#' @param x A data frame containing well water level data. Defaults to `all_combined_data`.
#' @param y A spatial data frame containing basin information. Defaults to `ActiveDivides`.
#'
#' @return A data frame with monthly averages of the change in well water levels for each basin. The output includes:
#' \describe{
#'   \item{Divide}{The divide ID.}
#'   \item{date}{The first day of the month for which the average is calculated.}
#'   \item{mean_change_mm}{The mean change in well water level in millimeters.}
#' }
#'
#'
#'
#' @export


CleanWells <- function(x, y){


all_combined_data <- ActiveDivides <- site <- Site <- well_water_level <- day_to_day_change <- day_to_day_change_mm <- well_water_level_ft <- Divide <- YearMonth <- mean_change_mm <- NULL


  #Filter out the unnecessary info from the original pull, paring it down to just sites with well water level data.
  WellData <- x %>%
    dplyr::select(date, contains("Well", ignore.case = TRUE)) %>%
    dplyr::select(date, !contains(c("xlsx", "xle", "temperature", "Pressure", "Groundwater"), ignore.case = TRUE))

  #Make the data long
  WellData_long <- WellData %>%
    pivot_longer(cols = starts_with("Dangermond"),
                 names_to = "site",
                 values_to = "well_water_level")

  #Get the names to match between ActiveSites and WellData_Long
  WellData_long_clean <- WellData_long %>%
    separate(site, into = c("Site", "Data"), sep = "_", extra = "merge", fill = "right") %>%
    mutate(Site = ifelse(Site == "DangermondEastramajal", "DangermondEastRamajal", Site)) %>%
    mutate(Site = ifelse(Site == "DangermondQuailcanyon1", "DangermondQuailCanyon1", Site)) %>%
    mutate(Site = ifelse(Site == "DangermondTestwell", "DangermondTestWell", Site)) %>%
    mutate(Site = ifelse(Site == "DangermondLowerjalamavaqueros", "DangermondLowerJalamaVaqueros", Site))


  ##Calculate change in well water level from day to day in mm
  change_in_WWL <- WellData_long_clean %>%
    mutate(date = as.Date(date)) %>%
    na.omit(well_water_level) %>%
    arrange(Site, date) %>%
    group_by(Site) %>%
    mutate(day_to_day_change = well_water_level - lag(well_water_level, default = first(well_water_level))) %>%
    rename(well_water_level_ft = well_water_level,
           day_to_day_change_mm = day_to_day_change) %>%
    ungroup() %>%
    mutate(day_to_day_change_mm = day_to_day_change_mm * 304.8)

  #Join change in WWL and the ActiveDivides sf to prepare for mapping, output is monthly_basin_average

  WellData_Basins <- left_join(change_in_WWL, y)

  WellData_Basins <- WellData_Basins %>%
    drop_na(date, well_water_level_ft, Site)

  #Calculate monthly averages for each basin

  monthly_basin_average <- WellData_Basins %>%
    mutate(YearMonth = format(date, "%Y-%m")) %>%
    group_by(Divide, YearMonth) %>%
    summarize(mean_change_mm = mean(day_to_day_change_mm)) %>%
    mutate(date = as.Date(paste0(YearMonth, "-01"))) %>%
    filter(date <= "2019-09-01") %>%
    ungroup() %>%
    dplyr::select(Divide, date, mean_change_mm)

  return(monthly_basin_average)
}
