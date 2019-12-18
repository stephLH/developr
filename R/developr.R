#' Get package dependancies.
#'
#' Find packages dependances declared into R scripts and Rmd files.
#'
#' @param package_path Package path.
#' @param include_rmd If \code{TRUE} then find dependances into Rmd files as well.
#'
#' @return A dependencies vector.
#'
#' @export
package_deps <- function(package_path = getwd(), include_rmd = TRUE) {

  pattern <- ifelse(include_rmd, "\\.(R|Rmd)$", "\\.R$")

  deps <- list.files(package_path, pattern = pattern, full.names = TRUE, recursive = TRUE) %>%
    lapply(readr::read_lines) %>%
    unlist() %>%
    stringr::str_subset("([[:alnum:]\\.]+)::") %>%
    stringr::str_match_all("([[:alnum:]\\.]+)::") %>%
    purrr::map( ~ dplyr::tibble(.)[[1]][, 2]) %>%
    unlist() %>%
    unique() %>%
    sort() %>%
    .[which(is.na(stringr::str_detect(., stringr::str_match(getwd(), .)[, 1])))]

  return(deps)
}
