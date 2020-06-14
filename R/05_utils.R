#' @importFrom utils file.edit
NULL

# insert dot at the relevant place following magrittr conventions
insert_dot <- function(expr) {
  # # if it's the dot, leave untouched
  # if (identical(expr, quote(.)))
  #   return(expr)

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

# evaluate a single step given an output and expression, dealing with side effects
# and assignments
eval_step <-   fun <- function(input, expr, pf, buffer_env) {
  # we could go a bit faster, we restest each time if expr is a call
  # we can test once first, and modify all our functions so they don't test anymore

  if (has_assignment_op(expr)) {
    # assignment can only be used with `~~`
    # wether `~~` is used or not, assignment is on top of the tree due to precedence
    # so some juggling is required, indeed
    # `~~ a <- b` is `"<-"(~~ a, b)` and
    # `~~ a -> b` is `"<-"(b, ~~ a)`

    # remove double tilde from call before evaluation
    if (has_dbl_tilde(expr[[2]])) {
      expr[[2]] <- expr[[c(2,2,2)]]
    } else if (has_dbl_tilde(expr[[3]])) {
      expr[[3]] <- expr[[c(3,2,2)]]
    } else {
      stop("Wrong syntax! If you mean to assign as a side effect you should",
           "use a `~~` prefix")
    }

    # we create a new environment in which to evaluate this side effect
    # this way we can safely put "." there
    env <- new.env(parent = buffer_env)
    env$. <- input
    eval(expr, envir = env)
    # as.list by default drops "." and dotted variables
    list2env(x = as.list(env), envir = pf)
    list2env(x = as.list(env, all.names = TRUE)[
      ls(env, all.names = TRUE, pattern = "^\\..+")],
      envir = buffer_env)
    return(input)
  }

  if (has_dbl_tilde(expr)) {
    env <- new.env(parent = buffer_env)
    env$. <- input
    # evaluate expression past the tilde
    eval(expr[[c(2,2)]], envir = env)
    list2env(x = as.list(env), envir = pf)
    return(input)
  }

  if (has_if(expr)) {
    cond <- eval(expr[[2]], envir = list(. = input), enclos = buffer_env)
    if(cond) {
      res <- eval(insert_dot(expr[[3]]), envir = list(. = input), enclos = buffer_env)
      return(res)
    } else {
      if(length(expr) == 4) {
        res <- eval(insert_dot(expr[[4]]), envir = list(. = input), enclos = buffer_env)
      } else {
        res <- input
      }
    }
    return(res)
  }

  if (has_scalar_logic(expr)){
    stop("You've used '&&' or '||' in a naked pipe step, did you mean '&' or '||' ?")
  }

  if(has_logic_or_comparison(expr)){
    expr <- call("subset", expr)
  }

  if(has_tilde(expr)){
    class_ <- class(input)
    nm <- deparse(expr[[2]])
    agg_call <- expr[[2]]
    grp_vars <- attr(terms(eval(expr[-2])), "term.labels")
    splitter <- interaction(input[grp_vars], drop = TRUE)
    split_data <- split(input, splitter)
    res_agg <- sapply(split_data, function(x)
      eval(bquote(with(.(x), .(agg_call))), envir = list(. = input), enclos = buffer_env),
      USE.NAMES = FALSE)
    res <- do.call("rbind", lapply(split_data, `[`, 1,grp_vars, drop = FALSE))
    res[nm] <- res_agg
    row.names(res) <- NULL
    class(res) <- class_
    return(res)
  }

  if(has_colon_equal(expr)){
    new_nm <- as.character(expr[[2]])
    old_nm <- as.character(expr[[3]])
    names(input)[names(input) == old_nm] <- new_nm
    return(input)
  }

  if(has_dt(expr)) {
    if (!is.data.frame(input))
      stop("lhs must be a data frame")
    class_ <- class(input)
    if(!requireNamespace("data.table"))
      stop("You must have the package 'data.table' installed to use the feature `.dt[...]` in a nakedpipe call")
    assign(".dt", data.table::as.data.table(input), envir = buffer_env)
    res <- eval(expr, envir = list(. = input), enclos = buffer_env)
    rm(.dt, envir = buffer_env)
    class(res) <- class_
    return(res)
  }

  if(has_tb(expr)) {
    if (!is.data.frame(input))
      stop("lhs must be a data frame")
    class_ <- class(input)
    if(!requireNamespace("tb"))
      stop("You must have the package 'tb' installed to use the feature `.dt[...]` in a nakedpipe call")
    assign(".tb", tb::as_tb(input), envir = buffer_env)
    res <- eval(expr, envir = list(. = input), enclos = buffer_env)
    rm(.tb, envir = buffer_env)
    class(res) <- class_
    return(res)
  }

  if(has_equal(expr)){
    if(has_tilde(expr[[3]])){
      class_ <- class(input)
      nm <- as.character(expr[[2]])
      agg_call <- expr[[3]][[2]]
      grp_vars <- attr(terms(eval(expr[[3]][-2])), "term.labels")
      splitter <- interaction(input[grp_vars], drop = TRUE)
      split_data <- split(input, splitter)
      res_agg <- sapply(split_data, function(x)
        eval(bquote(with(.(x), .(agg_call))), envir = list(. = input), enclos = buffer_env),
        USE.NAMES = FALSE)
      res <- do.call("rbind", lapply(split_data, `[`, 1,grp_vars, drop = FALSE))
      res[nm] <- res_agg
      row.names(res) <- NULL
      class(res) <- class_
      return(res)
    } else {
      expr <- as.call(c(quote(transform), setNames(list(expr[[3]]), as.character(expr[[2]]))))
    }
  }
  eval(insert_dot(expr), envir = list(. = input), enclos = buffer_env)
}


has_dt <- function(expr) {
  is.call(expr) && identical(expr[[1]], quote(`[`)) &&
    (identical(expr[[2]], quote(.dt)) || has_dt(expr[[2]]))
}

has_tb <- function(expr) {
  is.call(expr) && identical(expr[[1]], quote(`[`)) &&
    (identical(expr[[2]], quote(.tb)) || has_tb(expr[[2]]))
}


has_if <- function(expr) {
  is.call(expr) && identical(expr[[1]], quote(`if`))
}


has_equal <- function(expr) {
  is.call(expr) && identical(expr[[1]], quote(`=`))
}

has_colon_equal <- function(expr) {
  is.call(expr) && identical(expr[[1]], quote(`:=`))
}

has_assignment_op <- function(expr) {
  is.call(expr) && (
    identical(expr[[1]], quote(`<-`)) ||
      identical(expr[[1]], quote(`<<-`)))
}

has_tilde <- function(expr) {
  is.call(expr) && identical(expr[[1]], quote(`~`))
}

has_dbl_tilde <- function(expr) {
  has_tilde(expr) && has_tilde(expr[[2]]) && length(expr[[2]] == 2)
}

has_logic_or_comparison <- function(expr) {
  is.call(expr) && (
    identical(expr[[1]], quote(`<`)) ||
      identical(expr[[1]], quote(`>`)) ||
      identical(expr[[1]], quote(`<=`)) ||
      identical(expr[[1]], quote(`>=`)) ||
      identical(expr[[1]], quote(`==`)) ||
      identical(expr[[1]], quote(`!=`)) ||
      identical(expr[[1]], quote(`&`)) ||
      identical(expr[[1]], quote(`|`)) ||
      identical(expr[[1]], quote(`%in%`)))
}


has_scalar_logic <- function(expr) {
  is.call(expr) && (
      identical(expr[[1]], quote(`&&`)) ||
      identical(expr[[1]], quote(`||`)))
}

writeToClipboard  <- function(x) {
  ## from pacman package

  OS <- Sys.info()["sysname"]

  if (!(OS %in% c("Darwin", "Windows", "Linux"))) {
    stop("Copying to clipboard not supported on your OS")
  }

  if (OS != "Windows") {
    writeClipboard <- NULL
  }

  switch(
    OS,
    "Darwin" = {j <- pipe("pbcopy", "w")
    writeLines(x, con = j)
    close(j)
    },
    "Windows" = writeClipboard(x, format = 1),
    "Linux" = {
      if (Sys.which("xclip") == "") {
        warning("Clipboard on Linux requires 'xclip'. Try using:\nsudo apt-get install xclip")
      }
      con <- pipe("xclip -i", "w")
      writeLines(x, con = con)
      close(con)
    }
  )
}



