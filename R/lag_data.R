#' Lag selected date data and save as parquet file
#'
#' This function selects date data based on the provided intervals, transforms the selected data into delayed versions (lags),
#' and saves the lagged data as a gzip compressed parquet file in the specified directory. If a file already exists at the target 
#' filepath and overwrite is TRUE, the existing file is replaced.
#'
#' @author Nathan C. Layman
#'
#' @param data_files Files containing the original data.
#' @param lag_intervals Time intervals for which data should be lagged.
#' @param model_dates_selected The model date for which the data will be lagged.
#' @param lagged_data_directory Directory where the lagged data files will be saved. If it doesn't exist, the directory is created.
#' @param basename_template Template for the naming the output file Default is "lagged_data_{model_dates_selected}.parquet".
#' @param overwrite Boolean flag indicating whether existing processed files should be overwritten. Default is TRUE.
#' @param ... Additional arguments not used by this function but included for generic function compatibility.
#'
#' @return A string containing the filepath to the processed parquet file.
#'
#' @note The file is saved in gzip compressed parquet format for efficiency.
#'
#' @examples
#' lag_data(data_files = list('data1.parquet', 'data2.parquet'),
#'          lag_intervals = c(7, 14, 21, 28),
#'          model_dates_selected = '2021-10-01',
#'          lagged_data_directory = './lagged_data',
#'          basename_template = "lagged_data_{model_dates_selected}.parquet",
#'          overwrite = TRUE)
#'
#' @export
lag_data <- function(data_files,
                     lag_intervals,
                     model_dates_selected,
                     lagged_data_directory,
                     basename_template = "lagged_data_{model_dates_selected}.parquet",
                     overwrite = TRUE,
                     ...) {

  # Check that we're only working on one date at a time
  stopifnot(length(model_dates_selected) == 1)

  # Set filename
  save_filename <- file.path(lagged_data_directory, glue::glue(basename_template))

  # Check if file already exists and can be read
  error_safe_read_parquet <- purrr::possibly(arrow::open_dataset, NULL)

  if (!is.null(error_safe_read_parquet(save_filename)) & !overwrite) {
    message("file already exists and can be loaded, skipping download")
    return(save_filename)
  }

  # The goal of this is to figure out the average of the data column over the interval
  # Find dates at start and end interval back from date
  # Group by x, y, start_interval, end_interval, and take the mean don't forget na.rm = T
  message(glue::glue("calculating lagged data for {dirname(data_files[1])} starting from {model_dates_selected}"))

  lagged_data <- map2_dfr(tail(lag_intervals, -1), head(lag_intervals, -1), function(lag_interval_start, lag_interval_end) {
    start_date <- model_dates_selected + days(lag_interval_start) # start, i.e. 30 days prior.
    end_date <- model_dates_selected + days(lag_interval_end) # end, i.e. 0 days prior.
    message(glue::glue("lag_interval range ({lag_interval_start}, {lag_interval_end}]: ({start_date}, {end_date}]"))

    # Note: lags go back in time so the inequality symbols are reversed. Also
    # date > start_date makes the range _exclusive_ (start_date, end_date] to avoid
    # duplication problems.
    arrow::open_dataset(data_files) |>
      dplyr::filter(date > start_date, date <= end_date) |>
      collect() |>
      group_by(x, y) |>
      summarize(across(contains("anomaly"), ~ mean(.x, na.rm = T)), .groups = "drop") |>
      mutate(lag_interval_start = abs(lag_interval_start)) |>
      select(x, y, lag_interval_start, everything())
  })

  # To ensure consistent schema even with missing data
  full_schema <- expand.grid(x = unique(lagged_data$x), y = unique(lagged_data$y), lag_interval_start = abs(tail(lag_intervals, -1)))

  lagged_data |>
    full_join(full_schema) |>
    pivot_wider(
      names_from = lag_interval_start,
      values_from = -c(x, y, lag_interval_start),
      names_glue = "{.value}_lag_{lag_interval_start}",
      names_expand = TRUE
    )

  lagged_data <- lagged_data |>
    dplyr::mutate(date = model_dates_selected) |>
    dplyr::select(x, y, date, dplyr::everything())

  arrow::write_parquet(lagged_data, save_filename, compression = "gzip", compression_level = 5)

  save_filename
}
