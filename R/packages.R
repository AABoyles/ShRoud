#' Load Package
#'
#' @param package
#' @param repos
#'
#' @return
#' @export
#'
#' @examples
load_package <- function(package, repos = "https://cloud.r-project.org/") {
    tryCatch(typeof(package), error = function(e) {
        package = deparse(substitute(package))
    })
    if (!package %in% .packages(all = TRUE)) {
        install.packages(package, repos = repos)
    }
    library(package, character.only = TRUE)
}

#' Load Packages
#'
#' @param packages
#'
#' @return
#' @export
#'
#' @examples
load_packages <- function(packages) {
    map(packages, load_package)
}
