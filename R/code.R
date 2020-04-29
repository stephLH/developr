#' Look for code inside R or Rmd scripts.
#'
#' @param code R syntax to search.
#' @param path Path where R, Rmd and DESCRIPTION files are located.
#'
#' @return A data frame with three columns : file, line number and code containing researched syntax.
#'
#' @export
code_find <- function(code, path = ".") {

  path <- stringr::str_replace(path, "/$", "") %>%
    tools::file_path_as_absolute()

  files <- list.files(path, recursive = TRUE, pattern = "(\\.(R|Rmd)|DESCRIPTION)$", full.names = TRUE)

  code_find <- dplyr::tibble(
    file = files,
    code = purrr::map(files, readLines, encoding = "UTF-8", warn = FALSE)
  ) %>%
    tidyr::unnest_legacy() %>%
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

#' Replace non ASCII character to unicode.
#'
#' @param r_file R file.
#' @param r_file_output Output R file.
#'
#' @export
replace_non_ascii_with_unicode <- function(r_file, r_file_output = NULL) {

  if (is.null(r_file_output)) {

    r_file_output <- r_file

  }

  dplyr::tibble(
    code = readLines(r_file, encoding = "UTF-8", warn = FALSE)
  ) %>%
    dplyr::mutate(line = dplyr::row_number()) %>%
    dplyr::mutate_at("code", strsplit, "") %>%
    tidyr::unnest(code, keep_empty = TRUE) %>%
    dplyr::mutate_at("code", stringr::str_replace_na, "") %>%
    dplyr::group_by(line) %>%
    dplyr::mutate(char = dplyr::row_number()) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      code_ascii = dplyr::if_else(
        !stringi::stri_enc_isascii(code),
        purrr::map_chr(
          code,
          ~ iconv(., from = "UTF-8", toRaw = TRUE) %>%
            unlist() %>%
            as.character() %>%
            paste0(collapse = "") %>%
            { paste0('\\u', .) }
        ),
        code,
        code
      )
    ) %>%
    dplyr::group_by(line) %>%
    dplyr::summarise_at("code_ascii", paste0, collapse = "") %>%
    dplyr::ungroup() %>%
    dplyr::pull(code_ascii) %>%
    readr::write_lines(r_file_output)

}
