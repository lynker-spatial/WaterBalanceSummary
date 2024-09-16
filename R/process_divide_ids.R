#' Process Data for a List of Divide IDs
#'
#' This function processes data for a list of divide IDs by selecting the corresponding basins and reading the associated parquet files from S3. The data is combined into a single data frame.
#'
#' @param divide_ids A character vector containing the divide IDs to process.
#' @param bucket_name A character string representing the name of the S3 bucket.
#' @param prefix A character string representing the prefix path in the S3 bucket.
#' @param region A character string representing the AWS region. Default is `"us-west-2"`.
#' @return A data frame containing the combined data for all the divide IDs.



process_divide_ids <- function(divide_ids, bucket_name, prefix, region) {
  bucket_name <- "tnc-dangermond"
  prefix <- "tnc_datastreams/"
  region <- "us-west-2"  # Specify the correct region

  all_combined_data_list <- list()

  for (divide_id in divide_ids) {
    # Select basin for the given divide ID (no plot)
    site_in_div <- select_basin(divide = divide_id)
    site_names <- site_in_div$SiteName

    # Read parquet files for the site names
    combined_data <- read_parquet_files(site_names, bucket_name, prefix, region)

    # Add to the overall list
    all_combined_data_list <- append(all_combined_data_list, list(combined_data))
  }

  # Combine all the data into one data frame
  all_combined_data <- bind_rows(all_combined_data_list)

  return(all_combined_data)
}
