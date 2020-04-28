#' nakedpipe functionals and functional sequences
#'
#' These naked pipes are substitutes or helpers for functionals.
#'
#' `%lapply.%` and `%sapply.%` are infix `lapply` and `sapply` variants used to run
#' a sequence of calls on each element of a vector, list or data frame.
#'
#' `%F.%`
#' can only take a dot on the lhs and returns a function, it is directly inspired
#' from *magrittr*'s functional sequences.
#'
#' `%lapply..%` and `%sapply..%` and `%F..%` are the bare counterparts, they
#' require explicit dots and have restricted features, but are faster.
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

#' @export
#' @rdname nakedpipe_functionals
`%sapply..%` <- function(X, expr) {
  eval.parent(substitute(sapply(X, function(x) x %..% expr)))
}

#' @export
#' @rdname nakedpipe_functionals
`%lapply..%` <- function(X, expr) {
  eval.parent(substitute(lapply(X, function(x) x %..% expr)))
}

#' @export
#' @rdname nakedpipe_functionals
`%F.%` <- function(dot, expr) {
  if(!identical(substitute(dot), quote(.))) stop("The lhs of `%F.%` must be `.`")
  as.function(c(
    alist(.=),
    do.call(substitute, list(sys.call(), list(`%F.%` = quote(`%.%`))))))
}

#' @export
#' @rdname nakedpipe_functionals
`%F..%` <- function(dot, expr) {
  if(!identical(substitute(dot), quote(.))) stop("The lhs of `%F..%` must be `.`")
  as.function(c(
    alist(.=),
    do.call(substitute, list(sys.call(), list(`%F..%` = quote(`%..%`))))))
}
