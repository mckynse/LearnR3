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

#' Extract participant ID from file_path_id
#'
#' @param data
#'
#' @returns data with ID
#'
get_participant_id <- function(data) {
  data_with_id <- data |>
    dplyr::mutate(
      id = stringr::str_extract(
        file_path_id,
        pattern = "/stress/[:alnum:]{2}/"
      ) |>
        stringr::str_remove("/stress/") |>
        stringr::str_remove("/"),
      .before = file_path_id
    ) |>
    dplyr::select(-file_path_id)

  return(data_with_id)
}

#' Reading all files in a measured parameter (directory) and binding them by file_path_id
#'
#' @param filename
#'
#' @returns all same type parameter data in one data file
#'
read_all <- function(filename) {
  data <- here::here("data-raw/nurses-stress") |>
    fs::dir_ls(regexp = filename, recurse = TRUE) |>
    purrr::map(read) |> # NO BRACKETS!
    purrr::list_rbind(names_to = "file_path_id")
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
