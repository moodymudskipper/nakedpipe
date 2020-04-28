#' @importFrom utils file.edit
NULL

# insert dot at the relevant place following magrittr conventions
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

# evaluate a single step given an output and expression, dealing with side effects
# and assignments
eval_step <-   fun <- function(input, expr, pf) {

  if (has_assignment_op(expr)) {
    # assignment can only be used with `~~`
    # wether `~~` is used or not, assignment is on top of the tree due to precedence
    # so some juggling is required, indeed
    # `~~ a <- b` is `"<-"(~~ a, b)` and
    # `~~ a -> b` is `"<-"(b, ~~ a)`

    env <- new.env()
    env$. <- input
    # remove double tilde from call before evaluation
    if (has_dbl_tilde(expr[[2]])) {
      expr[[2]] <- expr[[c(2,2,2)]]
    } else if (has_dbl_tilde(expr[[3]])) {
      expr[[3]] <- expr[[c(3,2,2)]]
    } else {
      stop("Wrong syntax! If you mean to assign as a side effect you should",
           "use a `~~` prefix")
    }
    eval(expr, envir = env, enclos = pf)
    list2env(x = as.list(env), envir = pf)
    return(input)
  }
  if (has_dbl_tilde(expr)) {
    env <- new.env()
    env$. <- input
    # evaluate expression past the tilde
    eval(expr[[c(2,2)]], envir = env, enclos = pf)
    list2env(x = as.list(env), envir = pf)
    return(input)
  }

  if (has_if(expr)) {
    cond <- eval(expr[[2]], envir = list(. = input), enclos = pf)
    if(cond) {
      res <- eval(insert_dot(expr[[3]]), envir = list(. = input), enclos = pf)
      return(res)
    } else {
      if(length(expr) == 4) {
        res <- eval(insert_dot(expr[[4]]), envir = list(. = input), enclos = pf)
      } else {
        res <- input
      }
    }
    return(res)
  }

  eval(insert_dot(expr), envir = list(. = input), enclos = pf)
}

has_if <- function(expr) {
  is.call(expr) && identical(expr[[1]], quote(`if`))
}


has_assignment_op <- function(expr) {
  is.call(expr) && (
    identical(expr[[1]], quote(`<-`)) ||
      identical(expr[[1]], quote(`<<-`)) ||
      identical(expr[[1]], quote(`=`)))
}

has_dbl_tilde <- function(y) {
  is.call(y) && identical(y[[1]], quote(`~`)) &&
    is.call(y[[2]]) && identical(y[[c(2,1)]], quote(`~`)) &&
    length(y[[2]] == 2)
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



