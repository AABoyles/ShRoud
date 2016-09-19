#' Calculate root mean square error
#'
#' @param x a single value or vector of observed (reference) values
#' @param y a single value of vector of the same length as \code{x} of predicted values
#' @return root meas square error
#' @examples
#' rmse(runif(10),runif(10))
#' @export

rmse <- function(x,y) {
  sqrt(mean((x - y)^2, na.rm = TRUE))
}
