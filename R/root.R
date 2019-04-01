#' Get root directory
#'
#' @param dir_test Directory to test exisence in order to specify the root directory.
#'
#' @return The root directory
#'
#' @export
get_root <- function(dir_test = "C:/OVE/") {

  if (dir.exists(dir_test)) {

    root <- dir_test

  } else {

    root <- "S:/scolarite/ove/OVE/"

  }

  return(root)
}
