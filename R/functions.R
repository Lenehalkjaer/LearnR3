#' Read in one nurses' stress data file
#'
#' @param a path to a data file
#' @param indskriver max_rows i funktionen
#'
#' @returns outouts a data frame / tibble
#'
read <- function(file_path, max_rows = 100) {
  data <- file_path |>
    readr::read_csv(
      show_col_types = FALSE,
      name_repair = snakecase::to_snake_case,
      n_max = max_rows
    )
  return(data)
}
