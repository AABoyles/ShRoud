#' Partial Apply
#'
#' @description Borrowed from the Functional Package, which had misnamed it 'Curry'
#'
#' @param FUN
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
partial_apply <- function(FUN, ...) {
    .orig = list(...)
    function(...) do.call(FUN, c(.orig, list(...)))
}
