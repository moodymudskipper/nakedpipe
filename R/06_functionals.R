

#' nakedpipe functionals
#'
#' These combine functionals and `%.%`
#'
#' @param X list or vector of element to apply the sequence on
#' @param expr call or sequence of calls wrapped inside `{}`
#' @export
#' @rdname nakedpipe_functionals
`%lapply.%` <- function(X, expr) {
  eval.parent(substitute(lapply(X, function(x) x %.% expr)))
}

#' @export
#' @rdname nakedpipe_functionals
`%sapply.%` <- function(X, expr) {
  eval.parent(substitute(sapply(X, function(x) x %.% expr)))
}
