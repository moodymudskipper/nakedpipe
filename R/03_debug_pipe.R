#' @export
#' @rdname nakedpipe
`%D.%` <- function(x, expr){
  pf <- parent.frame()
  #dot_backup <- if (exists(".", pf)) get(".", pf) else substitute()
  expr <- substitute(expr)
  if (!is.call(expr)) expr <- list(expr) else expr <- as.list(expr)[-1]
  body <- lapply(
    expr,
    function(expr) {
      if (has_assignment_op(expr)) {
        if (!has_dbl_tilde(expr[[2]]))
          stop("Wrong syntax! If you mean to assign as a side effect you should",
               "use a `~~` prefix")
        expr[[2]] <- expr[[c(2,2,2)]]
        return(bquote(side_effect(.(expr))))
      }
      if (has_dbl_tilde(expr)) {
        expr <- expr[[c(2,2)]]
        return(bquote(side_effect(.(expr))))
      }
      expr <- insert_dot(expr)
      as.call(c(quote(`<-`), quote(.), expr))
    })
  body <- c(
    as.call(c(quote(`<-`), quote(.), substitute(x))),
    body
  )

  nakedpipe_debugger <- as.function(alist(NULL))
  body(nakedpipe_debugger) <- as.call(c(quote(`{`),body))
  environment(nakedpipe_debugger) <- pf
  debugonce(nakedpipe_debugger)
  res <- nakedpipe_debugger()
  return(res)
}


#' Produce a side effect from the nakedpipe debugger
#'
#' This function should only be called inside `nakedpipe_debugger()`, which is
#' created and run when calling `%D.%`. You will
#' find it there to transcribe the side effects prefixed by `~~` in the pipe.
#' It acts as if it evaluates its argument in the calling environment of the
#' pipe, but making the current value of the dot available. If an assignment is
#' used inside `side_effect()` a variable will be created or modified in the
#' calling environment of the pipe.
#'
#' @param expr expr
#' @export
side_effect <- function(expr){
  if (!identical(
    eval.parent(quote(match.call())),
    quote(nakedpipe_debugger())))
    stop("`side_effect()` should only be called inside `nakedpipe_debugger()`!")
  pf <- parent.frame(3)
  # evaluate
  env <- new.env()
  env$. <- eval.parent(quote(.))
  eval(substitute(expr), envir = env, enclos = pf)
  list2env(x = as.list(env), envir = pf)
  invisible()
}
