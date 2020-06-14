run_incomplete_call <- function() {

  context <- rstudioapi::getSourceEditorContext()
  selection <- rstudioapi::primary_selection(context)[["text"]]
  test <- try(str2lang(selection), silent = TRUE)
  if(inherits(test, "try-error"))
    selection <- paste0(selection, "\n}")

  rstudioapi::sendToConsole(selection,execute = T)
}
