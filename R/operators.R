#' Set Difference Operator
#'
#' Take the difference between sets using an infix operator.
#' @rdname setdiffop
#' @export
`%\\%` <- setdiff

#' String concatenator
#'
#' Because \code{paste} is godawful.
#' @rdname stringconcat
#' @export
`%+%` <- function(a, b){
  paste0(a, b)
}
