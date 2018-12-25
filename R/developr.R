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
    stringr::str_match_all("([[:alnum:]\\.]+)::") %>%
    purrr::map_df(dplyr::as_tibble) %>%
    dplyr::pull(V2) %>% unique() %>% sort() %>%
    .[which(is.na(stringr::str_detect(., stringr::str_match(getwd(), .)[, 1])))]

  return(deps)
}

#' Build a package.
#'
#' @param package_path Package path.
#' @param documentation If \code{TRUE}, documentation is also generated.
#' @param \dots Additional arguments to \code{devtools::install_local()}.
#'
#' @export
package_build <- function(package_path = ".", documentation = TRUE, ...) {

  if (stringr::str_detect(package_path, "/$")) {
    package_path <- substr(package_path, 1, nchar(package_path) - 1)
  }

  if (documentation == TRUE) {
    devtools::document(package_path, roclets = c('rd', 'collate', 'namespace'))
  }

  package_path <- ifelse(package_path == ".", getwd(), package_path)

  devtools::install_local(package_path, ...)

}

#' Copy Microsoft Access tables to an R package data/ folder.
#'
#' @param access_path Microsoft Access database path.
#' @param data_path R packaga data path.
#' @param tables Optional selection of tables to copy.
#' @param tables_rda Optional tables names to save in data/.
#'
#' @export
access_rda <- function(access_path, data_path, tables = NULL, tables_rda = NULL) {

  if (!stringr::str_detect(data_path, "\\/$")) {
    data_path <- paste0(data_path, "/")
  }

  table_names <- impexp::access_tables(access_path)

  if (!is.null(tables)) {
    table_names <- intersect(tables, table_names)
  }

  if (length(table_names) >= 1) {

    tables <- purrr::map(table_names, impexp::access_import, access_path)

    if (!is.null(tables_rda)) {
      table_names <- tables_rda
    }

    names(tables) <- table_names

    attach(tables)

    purrr::walk(table_names, ~ save(list = ., file = paste0(data_path, ., ".rda"), compress = "bzip2"))

    detach(tables)

  }

  return(table_names)
}
