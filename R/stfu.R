#' Shut the File Up
#'
#' Given an expression, stfu evaluates and returns the value of the expression, but supressesses output to the console.
#'
#' @param expr
#'
#' @return whatever expr should return
#' @export
#'
#' @examples
#' stfu(print('Foo'))
#' stfu(cat('Foo'))
stfu <- function(expr) {
    fileloc <- tempfile()
    sink(fileloc)
    foo <- eval(expr)
    sink(NULL)
    unlink(fileloc)
    return(foo)
}
