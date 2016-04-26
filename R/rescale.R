#' Rescaling functions
#'
#' A Family of functions for transform the values of a vector
#' such that it falls into a specified range, without modifying
#' the ratios between values.
#'
#' @param x numeric vector
#' @param minx The desired minimum for the output vector
#' @param maxx The desired maximum for the output vector
#' @name rescale

#' @export
#' @rdname rescale
rescale <- function(x, minx, maxx){
  rng <- range(x, na.rm = TRUE)
  if(missing(minx)) minx <- rng[1]
  if(missing(maxx)) maxx <- rng[2]
  (x - rng[1]) / (rng[2] - rng[1]) * (maxx - minx) + minx
}

#' @export
#' @rdname rescale
rescale01 <- function(x) {
  rescale(x, 0, 1)
}

#' @export
#' @rdname rescale
rescale11 <- function(x) {
  rescale(x, -1, 1)
}

#' @export
#' @rdname rescale
rescale100 <- function(x){
  rescale(x, 0, 100)
}

#' Recentering Functions
#'
#' A family of functions to "recenter" a vector around a new value.
#' This changes the ratios between values of x, while preserving
#' the differences between the values.
#'
#' @param x a numeric vector
#' @param diff The number should be removed from each value of x
#'
#' @rdname recentering

#' @export
#' @rdname recentering
recenter <- function(x, diff){
  if(missing(diff)) diff <- mean(x, na.rm = TRUE)
  x - diff
}

#' @export
#' @rdname recentering
max0 <- function(x) {
  recenter(x, max(x, na.rm = TRUE))
}

#' @export
#' @rdname recentering
max100 <- function(x){
  recenter(x, max(x, na.rm = TRUE)-100)
}

#' @export
#' @rdname recentering
mean0 <- recenter

#' @export
#' @rdname recentering
median0 <- function(x){
  recenter(x, median(x, na.rm = TRUE))
}

#' @export
#' @rdname recentering
min0 <- function(x) {
  recenter(x, min(x, na.rm = TRUE))
}
