#' Read and Combine Terra Climate Parquet Files into a Single Data Frame
#'
#' This function reads all Parquet files from a specified S3 bucket related to Terra Climate data and combines them into a single data frame. It also performs some specific transformations on the `var` column to standardize variable names.
#'
#' @param sub A character string specifying the subfolder within the base folder to look for Parquet files. Default is "terraclim".
#' @param bucket A string representing the S3 bucket name.
#' @param prefix A string representing the prefix (path) within the S3 bucket where the Parquet files are stored.
#'
#'
#' @return A data frame containing the combined data from all Parquet files in the specified subfolder. The `var` column is standardized with values "run" for "q" and "str" for "soil".
#'
#' @details
#' The `TerraClimParquetRead` function performs the following steps:
#' \itemize{
#'   \item Lists all Parquet files in the specified subfolder within the base folder `"/data/water_balance/"`.
#'   \item Reads each Parquet file into a data frame and stores them in a list.
#'   \item Combines all data frames in the list into a single data frame with an additional column `file` indicating the file from which each row was read.
#'   \item Transforms the `var` column to standardize variable names, changing "q" to "run" and "soil" to "str".
#' }


# Function to read all parquet files in a folder and bind them into one data frame
TerraClimParquetRead <- function(sub = "terraclim", bucket = "tnc-dangermond", prefix = "water_balance/v2/") {
  Sys.setenv("AWS_DEFAULT_REGION" = "us-west-2")

  # List all parquet files in the specified folder on S3
  objects <- get_bucket_df(bucket = bucket, prefix = glue("{prefix}{sub}"), pattern = "\\.parquet$")

  # Filter to only parquet files
  parquet_files <- objects$Key[grepl("\\.parquet$", objects$Key)]

  # Create an empty list to store data frames
  all_dfs <- list()

  # Loop through the parquet files and read the data into a data frame
  for (file_key in parquet_files) {
    # Download the parquet file from S3 to a temporary location
    temp_file <- tempfile(fileext = ".parquet")
    save_object(object = file_key, bucket = bucket, file = temp_file)

    # Read the parquet data
    parquet_data <- read_parquet(temp_file)

    # Extract the variable name from the file path
    file_name <- tools::file_path_sans_ext(basename(file_key))

    # Append the data frame to the list
    all_dfs[[file_name]] <- parquet_data
  }

  # Bind all the data frames together
  final_df <- bind_rows(all_dfs, .id = "file")

  final_df <- final_df %>%
    mutate(var = if_else(var == "q", "run", var)) %>%
    mutate(var = if_else(var == "soil", "str", var))

  return(final_df)
}

