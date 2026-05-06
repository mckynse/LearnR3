#' Read in one nurses' stress data file.
#'
#' @param file_path Path to the data file
#' @param max_rows Maximum number of rows in output
#'
#' @returns Outputs a data frame/tibble
#'
read <- function(file_path, max_rows = 10) {
  data <- file_path |>
    readr::read_csv(
      show_col_types = FALSE,
      name_repair = snakecase::to_snake_case,
      n_max = max_rows
    )
  return(data)
}

#' Summarising by groups
#'
#' @param data
#'
#' @returns summarised data with mean, sd, median per minute
#'
summarise_by_datetime <- function(data) {
  summarised_data <- data |>
    dplyr::mutate(
      collection_datetime = lubridate::round_date(
        collection_datetime,
        unit = "minute"
      )
    ) |>
    dplyr::summarise(
      dplyr::across(
        where(is.numeric),
        list(
          mean = mean,
          sd = sd,
          median = median
        )
      ),
      .by = c(id, collection_datetime)
    )
  return(summarised_data)
}
