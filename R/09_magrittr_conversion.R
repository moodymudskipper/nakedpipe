nakedpipe_to_magrittr <- function(){
  selection <- rstudioapi::primary_selection(
    rstudioapi::getSourceEditorContext())[["text"]]
  rstudioapi::sendToConsole("",execute = F)
  selection
  selection_lng <- str2lang(selection)

  # support case where we selected assignment
  if(is.call(selection_lng) & deparse(selection_lng[[1]]) %in% c("<-", "<<-", "=", ":=")){
    is_assignment <- TRUE
    assign_op     <- selection_lng[[1]]
    assign_target <- selection_lng[[2]]
    selection_lng <- selection_lng[[3]]
  } else {
    is_assignment <- FALSE
  }

  pipe <- selection_lng[[1]]
  pipe_chr <- deparse(pipe)
  standard_pipes <- c("%..%","%.%", "%L.%", "%D.", "%V.%", "%P.", "%F.", "%F..")
  assignment_pipes <- c("%<..%","%<.%", "%<L.%", "%<V.%", "%<P.")
  if(!pipe_chr %in% c(standard_pipes, assignment_pipes)){
    stop("select a full proper nakedpipe call to use this functionality", call. = FALSE)
  }

  input <- selection_lng[[2]]
  if(is.call(selection_lng[[3]]) && identical(selection_lng[[c(3,1)]], quote(`{`))) {
    args <- as.list(selection_lng[[3]][-1])

    # inspect args for side effects
    lapply(args, function(x) {
      if (is.call(x) && deparse(x[[1]]) %in% c("~", "<-", "<<-", "="))
        stop("a nakedpipe containing side effects (`~~ expr`) cannot be converted", call. = FALSE)
    })

    if(pipe_chr %in% standard_pipes)
      args <- c(input, args)
    else
      args[[1]] <- call("%<>%", input, args[[1]])

    piped_expr <- Reduce(function(x,y) {
      if(is.call(y) && identical(y[[1]], quote(with))) {
        y <- y[[length(y)]] # so it works with or without explicit dot
        call("%$%", x, y)
      } else if(is.call(y) && identical(y[[1]], quote(`+`))) {
        y <- y[[length(y)]] # so it works with or without explicit dot
        # we call "+" 2 times so we can detect where to go to the next line
        call("+",x, call("+", y))
      } else {
        call("%>%", x, y)
      }
    }, args)

    if(is_assignment)
      piped_expr <- call(deparse(assign_op), assign_target, piped_expr)

    txt <- deparse(piped_expr)
    txt <- gsub("\\%>\\% ", "%>%\n  ", txt)
    txt <- gsub("\\%<>\\% ", "%<>%\n  ", txt)
    txt <- gsub("\\%\\$\\% ", "%$%\n  ", txt)
    txt <- gsub("\\+ \\+", "+\n  ", txt)
  } else {
    arg <- selection_lng[[3]]
    if(pipe_chr %in% standard_pipes)
      piped_expr <- call("%>%", input, arg)
    else
      piped_expr <- call("%<>%", input, arg)

    if(is_assignment)
      piped_expr <- call(deparse(assign_op), assign_target, piped_expr)

    txt <- deparse(piped_expr)
  }

  context <- rstudioapi::getSourceEditorContext()
  rstudioapi::modifyRange(context$selection[[c(1,1)]], txt, context$id)
}



magrittr_to_nakedpipe <- function(){
  selection <- rstudioapi::primary_selection(
    rstudioapi::getSourceEditorContext())[["text"]]
  rstudioapi::sendToConsole("",execute = F)
  selection
  selection_lng <- str2lang(selection)

  # support case where we selected assignment
  if(is.call(selection_lng) & deparse(selection_lng[[1]]) %in% c("<-", "<<-", "=", ":=")){
    is_assignment <- TRUE
    assign_op     <- selection_lng[[1]]
    assign_target <- selection_lng[[2]]
    selection_lng <- selection_lng[[3]]
  } else {
    is_assignment <- FALSE
  }
  pipe_chr <- deparse(selection_lng[[1]])
  # this is not the first pipe but the last!
  if(!pipe_chr %in% c("%>%", "%$%", "%T>%", "+")){
    stop("select a full proper magrittr pipe chain call to use this functionality",
         call. = FALSE)
  }

  args <- list()
  while(is.call(selection_lng) &&
        (pipe_chr <- deparse(selection_lng[[1]])) %in% c("%>%", "%$%", "%T>%", "+")){
    if(pipe_chr == "%T>%")
      stop("a magrittr pipe chain containing side effects (`%T>%`) cannot be converted",
           call. = FALSE)
    if(pipe_chr == "%$%") {
      args <- c(call("with",selection_lng[[3]]), args)
      selection_lng <- selection_lng[[2]]
    } else if(pipe_chr == "+") {
      args <- c(call("+",selection_lng[[3]]), args)
      selection_lng <- selection_lng[[2]]
    } else {
    args <- c(selection_lng[[3]], args)
    selection_lng <- selection_lng[[2]]
    }
  }
  if(is.call(selection_lng) && identical(selection_lng[[1]], quote(`%<>%`))){
    args <- c(selection_lng[[3]], args)
    input <- selection_lng[[2]]
    np_expr <- call("%<.%", input, as.call(c(quote(`{`), args)))
  } else {
    input <- selection_lng
    np_expr <- call("%.%", input, as.call(c(quote(`{`), args)))
  }

  if(is_assignment)
    np_expr <- call(deparse(assign_op), assign_target, np_expr)
  txt <- deparse(np_expr)
  txt <- gsub("^    ","  ", txt)
  txt <- paste(txt, collapse="\n")

  context <- rstudioapi::getSourceEditorContext()
  rstudioapi::modifyRange(context$selection[[c(1,1)]], txt, context$id)
}
