#' Look for code inside R or Rmd scripts.
#'
#' @param code R syntax to search.
#' @param path Path where R and Rmd files are located.
#'
#' @return A data frame with three columns : file, line number and code containing researched syntax.
#'
#' @export
code_find <- function(code, path = getwd()) {

  fichiers <- list.files(path, recursive = TRUE, pattern = "\\.(R|Rmd)$", full.names = TRUE)

  code_find <- dplyr::tibble(
    fichier = fichiers,
    code = purrr::map(fichiers, readr::read_table, col_names = "code", col_types = "c")
  ) %>%
    tidyr::unnest() %>%
    dplyr::group_by(fichier) %>%
    dplyr::mutate(ligne = dplyr::row_number()) %>%
    dplyr::ungroup() %>%
    dplyr::select(fichier, ligne, code) %>%
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
code_replace <- function(code, replacement, path) {

  fichiers <- developr::code_find(code, path) %>%
    dplyr::pull(fichier) %>%
    unique()

  import_code <- purrr::map(fichiers, readr::read_lines) %>%
    purrr::map(stringr::str_replace_all, code, replacement)

  purrr::walk2(import_code, fichiers, readr::write_lines)
}
