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
