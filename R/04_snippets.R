#' setup nakedpipe snippets
#'
#' Places snippet definitions in the pastebin and opens in the editor the
#' snippet file to modify.
#'
#' @export
setup_nakedpipe_snippets <- function(){
  snips <- "
snippet ..
\t%..% {
\t  ${1}
\t}

snippet .
\t%.% {
\t  ${1}
\t}"

  if (.Platform$OS.type == "windows") {
  writeClipboard(snips)
  } else {
    cat(snips, file = pipe("pbcopy"))
  }
  file.edit("~/.R/snippets/r.snippets")
  message("snippets have been copied in your clipboard and the 'r.snippets' ",
          "file was opened in your editor. Paste the snippets wherever you ",
          "prefer (at the top is fine), and save, then you can use the snippet ",
          "by typing `.` or `..` followed by 2 quick presses on the <tab> key.")

}
