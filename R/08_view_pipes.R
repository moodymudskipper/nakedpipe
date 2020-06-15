#' @export
#' @rdname nakedpipe
`%V.%` <- function(x, expr) {
  # nocov start
  View <- NULL # to avoid a cmd check note
  header <- deparse(substitute(x))
  header[length(header)] <- paste(header[length(header)],  "%V.% {")
  lapply(header, message)
  pf <- parent.frame()
  buffer_env <- new.env(parent = pf)
  expr <- substitute(expr)
  if (!is.call(expr)) expr <- list(expr)
  if (identical(expr[[1]], quote(`{`))) expr <- as.list(substitute(expr)[-1])
  args <- c(list(x), expr)
  res <- Reduce(function(x,y) {
    sapply(paste0("  ", deparse(y)), message)
    res <- eval_step(x,y,pf, buffer_env)
    if(!is.call(y) || !identical(y[[1]], quote(`<-`)) && !identical(y[[1]], quote(`~`)))
      View(res, title = deparse(insert_dot(y))[[1]])
    res}
    , args)
  message("}")
  res
  # nocov end
}


#' @export
#' @rdname nakedpipe
`%<V.%` <- function(x, expr) {
  # nocov start
  View <- NULL # to avoid a cmd check note
  header <- deparse(substitute(x))
  header[length(header)] <- paste(header[length(header)],  "%<V.% {")
  lapply(header, message)
  pf <- parent.frame()
  buffer_env <- new.env(parent = pf)
  expr <- substitute(expr)
  if (!is.call(expr))  expr <- list(expr)
  if (identical(expr[[1]], quote(`{`))) expr <- as.list(substitute(expr)[-1])
  args <- c(list(x), expr)
  res <- Reduce(function(x,y) {
    sapply(paste0("  ", deparse(y)), message)
    res <- eval_step(x,y,pf, buffer_env)
    if(!is.call(y) || !identical(y[[1]], quote(`<-`)) && !identical(y[[1]], quote(`~`)))
      View(res, title = deparse(insert_dot(y))[[1]])
    res}
    , args)
  message("}")
  assign(as.character(substitute(x)), res, envir = parent.frame())
  invisible(res)
  # nocov end
}
