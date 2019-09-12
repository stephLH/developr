#' Look for code inside R or Rmd scripts.
#'
#' @param code R syntax to search.
#' @param path Path where R and Rmd files are located.
#'
#' @return A data frame with three columns : file, line number and code containing researched syntax.
#'
#' @export
code_find <- function(code, path = ".") {

  path <- stringr::str_replace(path, "/$", "") %>%
    tools::file_path_as_absolute()

  files <- list.files(path, recursive = TRUE, pattern = "\\.(R|Rmd)$", full.names = TRUE)

  code_find <- dplyr::tibble(
    file = files,
    code = purrr::map(files, readLines, warn = FALSE)
  ) %>%
    tidyr::unnest() %>%
    dplyr::group_by(.data$file) %>%
    dplyr::mutate(line = dplyr::row_number()) %>%
    dplyr::ungroup() %>%
    dplyr::select(.data$file, .data$line, code) %>%
    tidyr::drop_na(code) %>%
    dplyr::filter(stringr::str_detect(code, !!code))

  return(code_find)
}

#' Look and replace code inside R or Rmd scripts.
#'
#' @param code R syntax to search and replace.
#' @param replacement A character of replacement.
#' @param path Path where R and Rmd files are located.
#'
#' @export
code_replace <- function(code, replacement, path = ".") {

  path <- stringr::str_replace(path, "/$", "") %>%
    tools::file_path_as_absolute()

  files <- developr::code_find(code, path) %>%
    dplyr::pull(.data$file) %>%
    unique()

  import_code <- purrr::map(files, readr::read_lines) %>%
    purrr::map(stringr::str_replace_all, code, replacement)

  purrr::walk2(import_code, files, readr::write_lines)
}
