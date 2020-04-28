#' Naked pipe
#'
#' Pipe into a sequence of calls, without repeating the pipe symbol.
#'
#' This is inspired by Stefan Bache and Hadley Wickham's *magrittr* pipe and
#' behaves consistently, though it doesn't use its code.
#'
#' - `%.%` is the main pipe, called **naked pipe**, by contrast to the traditional
#'   more verbose *magrittr* pipe.
#' - `%<.%` is the **assignment pipe**, it works like `%.%` but
#'   assigns the result back to the input variable.
#' - `%L.%` is the **logging pipe**, it works like `%.%` but logs to the console the
#'   calls and their execution times.
#' - `%P.%` is the **printing pipe**, it works like `%.%` but prints the output
#'   of each step to the console.
#' - `%V.%` is the **viewing pipe**, it works like `%.%` but calls `View()` on
#'   the output of each step.
#'   calls and their execution times.
#' - `%<L.%`, `%<P.%` and `%<V.%` are assigning counterparts of the pipes described above.
#' - `%D.%` is the **debugging pipe**, is used to go through the pipe step by
#'   step, it gives the  same output as the standard `%.%` pipe. See also
#'   `?side_effect`.
#'
#' The prefix `~~` can be used with any of the above for side effects.
#'
#' - `%..%` requires explicit dots and is faster and more adapted to programming,
#' it doesn't support `~~`. It's the bare version of `%.%`, which makes it the
#' **bare-naked pipe**.
#' - `%<..%` is the **bare-naked assignment pipe**, it works like `%..%` but
#' assigns the result back to the input variable.
#'
#' @param x object to pipe in
#' @param expr call or sequence of calls wrapped inside `{}`
#'
#'
#' @examples
#'
#' # Pipe into a sequence of calls using `%.%`
#' cars %.% {
#'   head(2)
#'   transform(time = dist/speed)
#' }
#'
#' # it plays well with left to right assignment
#' cars %.% {
#'   head(2)
#'   transform(time = dist/speed)
#' } -> res
#' res
#'
#' # Use `~~` for side effects
#' cars %.% {
#'   head(2)
#'   ~~ message("nrow:", nrow(.))
#'   transform(time = dist/speed)
#' }
#'
#' # This include assignments
#' cars %.% {
#'   head(2)
#'   ~~ cars_h <- .
#'   transform(time = dist/speed)
#' }
#' cars_h
#'
#' # Assign in place using `%<.%`
#' cars_copy <- cars
#' cars_copy %<.% {
#'   head(2)
#'   ~~ message("nrow:", nrow(.))
#'   transform(time = dist/speed)
#' }
#' cars_copy
#'
#' # Clock each step using `%L.%`
#' cars %L.% {
#'   head(2)
#'   ~~ Sys.sleep(1)
#'   transform(time = dist/speed)
#' }
#'
#' \dontrun{
#' # Debug the pipe using `%D.%`
#' cars %D.% {
#'   head(2)
#'   ~~ message("nrow:", nrow(.))
#'   transform(time = dist/speed)
#' }
#' }
#'
#' # `%..%` is faster at the cost of using explicit dots
#' cars %..% {
#'   head(.,2)
#'   transform(.,time = dist/speed)
#' }
#'
#' # It is better suited for programming and doesn't support side effect
#' # notation, but you can do :
#' cars %..% {
#'   head(.,2)
#'   {message("nrow:", nrow(.)); .}
#'   transform(.,time = dist/speed)
#' }
#'
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

#' @export
#' @rdname nakedpipe
`%.%` <- function(x, expr) {
  pf <- parent.frame()
  expr <- substitute(expr)
  if (!is.call(expr)) return(eval(insert_dot(expr), envir = list(. = x), enclos = pf))
  if (identical(expr[[1]], quote(`{`))) expr <- as.list(substitute(expr)[-1])
  args <- c(list(x), expr)
  Reduce(function(x,y) eval_step(x,y,pf), args)
}

#' @export
#' @rdname nakedpipe
`%<.%` <- function(x, expr) {
  val <- eval.parent(do.call(substitute, list(match.call(), list(`%<.%` = quote(`%.%`)))))
  assign(as.character(substitute(x)), val, envir = parent.frame())
  invisible(val)
}

#' @export
#' @rdname nakedpipe
`%<..%` <- function(x, expr) {
  val <- eval.parent(do.call(substitute, list(match.call(), list(`%<..%` = quote(`%..%`)))))
  assign(as.character(substitute(x)), val, envir = parent.frame())
  invisible(val)
}

