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




#' Convert the map with reading code into a function
#'
#' @param The name of files in the sub-folders that we
#'    want to read in.
#'
#' @returns A single data frame/tibble
#'
read_all <- function(filename) {
  # Code that does something

  files <- here::here("data-raw/nurses-stress") |>
    fs::dir_ls(regexp = filename, recurse = TRUE)

  data <- files |>
    purrr::map(read) |> # no brackets!
    purrr::list_rbind(names_to = "file_path_id")

  return(data)
}
