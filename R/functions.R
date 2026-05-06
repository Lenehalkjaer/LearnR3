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



#' Summarise values in a data frame by a rounded datetime
#'
#' @param A data frame with at least a `collection_datetime` column and
#'  some numeric columns to summarise.
#'
#' @returns A summarised data frame.
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
        tidyselect::where(is.numeric),
        list(mean = mean, sd = sd, median = median)
      ),
      .by = c(id, collection_datetime)
    )
  return(summarised_data)
}
