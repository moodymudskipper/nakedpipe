#' @importFrom stats setNames terms
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

as_tb <- getFromNamespace("as_tb", "tb")

# writeToClipboard  <- function(x) {
#   ## from pacman package
#
#   OS <- Sys.info()["sysname"]
#
#   if (!(OS %in% c("Darwin", "Windows", "Linux"))) {
#     stop("Copying to clipboard not supported on your OS")
#   }
#
#   if (OS != "Windows") {
#     writeClipboard <- NULL
#   }
#
#   switch(
#     OS,
#     "Darwin" = {j <- pipe("pbcopy", "w")
#     writeLines(x, con = j)
#     close(j)
#     },
#     "Windows" = writeClipboard(x, format = 1),
#     "Linux" = {
#       if (Sys.which("xclip") == "") {
#         warning("Clipboard on Linux requires 'xclip'. Try using:\nsudo apt-get install xclip")
#       }
#       con <- pipe("xclip -i", "w")
#       writeLines(x, con = con)
#       close(con)
#     }
#   )
# }
