#' Read Parquet Files from S3
#'
#' This function lists and reads parquet files from an S3 bucket for a given set of site names. The data is processed to group by date and summarize numeric columns.
#'
#' @param site_names A character vector containing site names for which to read parquet files.
#' @param bucket_name A character string representing the name of the S3 bucket.
#' @param prefix A character string representing the prefix path in the S3 bucket.
#' @param region A character string representing the AWS region. Default is `"us-west-2"`.
#' @return A data frame containing the combined data from the parquet files, grouped by date.


read_parquet_files <- function(site_names, bucket_name, prefix, region = "us-west-2") {

  timestamp_utc <- NULL

  all_combined_data_list <- list()

  for (site in site_names) {
    site_prefix <- paste0(prefix, site, "/")
    objects <- get_bucket_df(bucket_name, prefix = site_prefix, region = region)
    parquet_files <- objects$Key[grepl("\\.parquet$", objects$Key)]

    for (file in parquet_files) {
      temp_file <- tempfile(fileext = ".parquet")
      save_object(object = file, bucket = bucket_name, file = temp_file, region = region)
      filename <- tools::file_path_sans_ext(basename(file))
      df <- read_parquet(temp_file, col_select = c("timestamp_utc", starts_with("Danger")))

      if ("timestamp_utc" %in% names(df)) {
        combined_data <- df %>%
          separate(timestamp_utc, into = c("date", "time"), sep = " ") %>%
          mutate(date = as.Date(date)) %>%
          group_by(date) %>%
          summarize(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)))

        all_combined_data_list <- append(all_combined_data_list, list(combined_data))
      }
    }
  }

  all_combined_data <- bind_rows(all_combined_data_list)
  return(all_combined_data)
}
