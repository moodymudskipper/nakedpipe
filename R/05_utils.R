#' @importFrom utils file.edit writeClipboard
NULL

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

eval_step <-   fun <- function(x,y, pf) {
  if (has_assignment_op(y)) {
    if (!has_dbl_tilde(y[[2]]))
      stop("Wrong syntax! If you mean to assign as a side effect you should",
           "use a `~~` prefix")
    env <- new.env()
    env$. <- x
    y[[2]] <- y[[c(2,2,2)]]
    eval(y, envir = env, enclos = pf)
    list2env(x = as.list(env), envir = pf)
    return(x)
  }
  if (has_dbl_tilde(y)) {
    env <- new.env()
    env$. <- x
    eval(y[[c(2,2)]], envir = env, enclos = pf)
    list2env(x = as.list(env), envir = pf)
    return(x)
  }
  eval(insert_dot(y), envir = list(. = x), enclos = pf)
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



