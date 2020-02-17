insert_dot <- function(expr) {
  if (is.symbol(expr) || expr[[1]] == quote(`(`)) {
    # if a symbol or an expression inside parentheses, make it a call with dot arg
    expr <- as.call(c(expr, quote(`.`)))
  } else if (length(expr) == 1) {
    # if a call without arg, give it a dot arg
    expr <- as.call(c(expr[[1]], quote(`.`)))
  } else if (expr[[1]] != quote(`{`) &&
             all(sapply(expr[-1], `!=`, quote(`.`)))) {
    # if a call with args but no dot in arg, insert one first
    expr <- as.call(c(expr[[1]], quote(`.`), as.list(expr[-1])))
  }
  expr
}

#' Naked pipe
#'
#' Pipe into a sequence of calls, without repeating the pipe symbol
#'
#' This is inspired by Stefan Bache and Hadley Wickham's *magrittr* pipe and
#' behaves consistently, though it doesn't use its code.
#'
#' If you want to be even faster, use `%..%` and use
#' explicit dots (though keep in mind these are micro seconds!)
#'
#' @param x object to pipe in
#' @param expr sequence of calls wrapped inside `{}`
#'
#' @export
#'
#' @examples
#' # Pipe into a sequence of calls using `%.%`
#' cars %.% {
#'   head(2)
#'   transform(time = dist/speed)
#' }
#'
#' # `%..%` is faster at the cost of using explicit dots
#' cars %..% {
#'   head(.,2)
#'   transform(.,time = dist/speed)
#' }
#'
#' # the naked pipe plays well with left to right assignment
#' cars %.% {
#'   head(2)
#'   transform(time = dist/speed)
#' } -> res
#' res
#' @rdname nakedpipe
`%.%` <- function(x, expr) {
  pf <- parent.frame()
  expr <- substitute(expr)
  if (!is.call(expr)) return(eval(insert_dot(expr), envir = list(. = x), enclos = pf))
  if (identical(expr[[1]], quote(`{`))) expr <- as.list(substitute(expr)[-1])
  args <- c(list(x), expr)
  Reduce(function(x,y) eval(insert_dot(y), envir = list(. = x), enclos = pf), args)
}

#' @export
#' @rdname nakedpipe
`%..%` <- function(x, expr) {
  pf <- parent.frame()
  expr <- substitute(expr)
  if (!is.call(expr)) return(eval(expr, envir = list(. = x), enclos = pf))
  if (identical(expr[[1]], quote(`{`))) expr <- as.list(substitute(expr)[-1])
  args <- c(list(x), expr)
  Reduce(function(x,y) eval(y, envir = list(. = x), enclos = pf), args)
}
