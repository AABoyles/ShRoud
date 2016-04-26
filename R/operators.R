#' Inline Set Operators
#'
#' @param a a set
#' @param b another set
#'
#' @rdname setdiffop
#' @export
`%\\%` <- setdiff

#' @rdname setdiffop
#' @export
`%U%` <- union

#' @rdname setdiffop
#' @export
`%n%` <- intersect


#' String concatenator
#'
#' Because \code{paste} is godawful.
#'
#' @param a, b character strings to be concatenated together
#'
#' @rdname stringconcat
#' @export
`%+%` <- function(a, b){
  paste0(a, b)
}
