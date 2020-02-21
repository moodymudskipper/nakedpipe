#' @export
#' @rdname nakedpipe
`%L.%` <- function(x, expr) {
  header <- deparse(substitute(x))
  header[length(header)] <- paste(header[length(header)],  "%L.% {")
  lapply(header, message)
  pf <- parent.frame()
  expr <- substitute(expr)
  if (!is.call(expr)) expr <- list(expr)
  if (identical(expr[[1]], quote(`{`))) expr <- as.list(substitute(expr)[-1])
  args <- c(list(x), expr)
  res <- Reduce(function(x,y) {
    sapply(paste0("  ",deparse(y)), message)
    time <- system.time(res <- eval_step(x,y,pf))
    print(time)
    res}
    , args)
  message("}")
  res
}


#' @export
#' @rdname nakedpipe
`%<L.%` <- function(x, expr) {
  header <- deparse(substitute(x))
  header[length(header)] <- paste(header[length(header)],  "%<L.% {")
  lapply(header, message)
  pf <- parent.frame()
  expr <- substitute(expr)
  if (!is.call(expr))  expr <- list(expr)
  if (identical(expr[[1]], quote(`{`))) expr <- as.list(substitute(expr)[-1])
  args <- c(list(x), expr)
  res <- Reduce(function(x,y) {
    sapply(paste0("  ",deparse(y)), message)
    time <- system.time(res <- eval_step(x,y,pf))
    print(time)
    res}
    , args)
  message("}")
  assign(as.character(substitute(x)), res, envir = parent.frame())
  invisible(res)
}
