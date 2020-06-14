connect_np_steps_with_magrittr <- function(x,y) {
  if(!is.call(y)) return(call("%>%", x, y))

  y_fun_chr <- paste(deparse(y[[1]]), collapse=";")

  if(y_fun_chr == "if") {
    # handle `if` call
    else_is_missing <- length(y) == 3
    if(else_is_missing)
      y[[4]] <- quote(.) # add an else condition
    else
      y[[4]] <- insert_dot(y[[4]])
    y[[3]] <- insert_dot(y[[3]])
    # wrap into `{` so it can be piped
    y <- as.call(c(quote(`{`), y))
    return(call("%>%", x, y))
  }

  if(y_fun_chr == "with") {
    # handle `with` call
    y <- y[[length(y)]] # so it works with or without explicit dot
    return(call("%$%", x, y))
  }

  if(y_fun_chr == c("+")) {
    y <- y[[length(y)]] # so it works with or without explicit dot
    # we call "+" 2 times so we can detect where to go to the next line
    return(call("+",x, call("+", y)))
  }

  if(y_fun_chr %in% c("<-", "<<-", "=")) {
    stop("Can't convert, magrittr doesn't support temporary variables or ",
         "assignments to the current environment.", call. = FALSE)
  }

  if(y_fun_chr == "~") {
    warning(paste("converting side effects between magrittr and nakedpipe syntax is",
            "not guaranteed to always work as they deal differently with environments."))
    y <- y[[c(2,2)]]
    return(call("%T>%", x, y))
  }

  call("%>%", x, y)
}


np_standard_pipes   <- c("%..%","%.%", "%L.%", "%D.", "%V.%", "%P.", "%F.", "%F..")

np_assignment_pipes <- c("%<..%","%<.%", "%<L.%", "%<V.%", "%<P.")

# "+" is included as it ca be used for ggplot calls
mg_pipes <- c("%>%", "%$%", "%T>%", "+")

toggle <- function() {

  context <- rstudioapi::getSourceEditorContext()
  selection <- rstudioapi::primary_selection(context)[["text"]]
  selection_lng <- str2lang(selection)

  # support case where we selected assignment
  assignment_syms <- c("<-", "<<-", "=", ":=")
  is_assignment <-
    is.call(selection_lng) && deparse(selection_lng[[1]]) %in% assignment_syms
  assign_op <- NULL
  assign_target <- NULL
  if(is_assignment){
    assign_op     <- selection_lng[[1]]
    assign_target <- selection_lng[[2]]
    selection_lng <- selection_lng[[3]]
  }

  # maybe better check with regex to check start of the pipe not end
  pipe_chr <- deparse(selection_lng[[1]])
  if (pipe_chr %in% c(np_standard_pipes, np_assignment_pipes)) {
    txt <- nakedpipe_to_magrittr(
      selection_lng, is_assignment, assign_op, assign_target)
  } else if (pipe_chr %in% mg_pipes) {
    txt <- magrittr_to_nakedpipe(
      selection_lng, is_assignment, assign_op, assign_target)
  } else {
    stop("select proper nakedpipe or magrittr call to use this functionality", call. = FALSE)
  }
  rstudioapi::modifyRange(context$selection[[c(1,1)]], txt, context$id)
}

nakedpipe_to_magrittr <- function(selection_lng, is_assignment, assign_op, assign_target){

  pipe_chr <- deparse(selection_lng[[1]])

  input <- selection_lng[[2]]
  np_uses_curly <-
    is.call(selection_lng[[3]]) && identical(selection_lng[[c(3,1)]], quote(`{`))
  if(np_uses_curly) {
    steps <- as.list(selection_lng[[3]][-1])

    # integrate input in steps
    if(pipe_chr %in% np_standard_pipes)
      steps <- c(input, steps)
    else
      steps[[1]] <- call("%<>%", input, steps[[1]])

    # connect all steps
    piped_expr <- Reduce(connect_np_steps_with_magrittr, steps)

    # rebuild original call by prefixing assignment `foo <- ...`
    if(is_assignment)
      piped_expr <- call(deparse(assign_op), assign_target, piped_expr)

    # format output
    txt <- piped_expr %..% {
      deparse(.)
      gsub("\\%>\\% ", "%>%\n  ", .)
      gsub("\\%T>\\% ", "%T>%\n  ", .)
      gsub("\\%<>\\% ", "%<>%\n  ", .)
      gsub("\\%\\$\\% ", "%$%\n  ", .)
      gsub("\\+ \\+", "+\n  ", .)
      paste(., collapse = "\n")
    }

  } else {
    arg <- selection_lng[[3]]
    if(pipe_chr %in% np_standard_pipes)
      piped_expr <- call("%>%", input, arg)
    else
      piped_expr <- call("%<>%", input, arg)

    if(is_assignment)
      piped_expr <- call(deparse(assign_op), assign_target, piped_expr)

    txt <- deparse(piped_expr)
  }
  txt
}

magrittr_to_nakedpipe <- function(selection_lng, is_assignment, assign_op, assign_target){

  pipe_chr <- deparse(selection_lng[[1]])
  steps <- list()

  repeat {
    # break loop if not a pipe or "+"
    if (is.call(selection_lng)) pipe_chr <- deparse(selection_lng[[1]])
    is_pipe_or_plus <- is.call(selection_lng) && pipe_chr %in% c("%>%", "%$%", "%T>%", "+")
    if(!is_pipe_or_plus) break

    if(pipe_chr == "%T>%") {
      warning(paste("converting side effects between magrittr and nakedpipe syntax is",
                    "not guaranteed to always work as they deal differently with environments."))
      step <- insert_dot(selection_lng[[3]])
      step <- call("~",call("~",step))
      steps <- c(step, steps)
      selection_lng <- selection_lng[[2]]
      next
    }
      # stop("a magrittr pipe chain containing side effects (`%T>%`) cannot be converted",
      #      call. = FALSE)

    if(pipe_chr == "%$%") {
      steps <- c(call("with",selection_lng[[3]]), steps)
      selection_lng <- selection_lng[[2]]
      next
    }

    if(pipe_chr == "+") {
      steps <- c(call("+",selection_lng[[3]]), steps)
      selection_lng <- selection_lng[[2]]
      next
    }

    curr_step <- selection_lng[[3]]

    # disentangle conditional step
    is_if_step <-
      length(curr_step) == 2 &&
      identical(curr_step[[1]], quote(`{`)) &&
      is.call(curr_step[[2]]) &&
      identical(curr_step[[c(2,1)]], quote(`if`))

    if(is_if_step) {
      curr_step <- curr_step[[2]]
      else_is_missing <- length(curr_step) == 3
      if (else_is_missing) curr_step[4] <- list(NULL)
      else_is_dot <-
        length(curr_step) == 4 && identical(curr_step[[4]], quote(.))
      if (else_is_dot) curr_step[[4]] <- NULL
    }
    steps <- c(curr_step, steps)
    selection_lng <- selection_lng[[2]]
  }

  has_assignment_pipe <-
    is.call(selection_lng) && identical(selection_lng[[1]], quote(`%<>%`))
  if(has_assignment_pipe){
    steps <- c(selection_lng[[3]], steps)
    input <- selection_lng[[2]]
    np_expr <- call("%<.%", input, as.call(c(quote(`{`), steps)))
  } else {
    input <- selection_lng
    np_expr <- call("%.%", input, as.call(c(quote(`{`), steps)))
  }

  if(is_assignment)
    np_expr <- call(deparse(assign_op), assign_target, np_expr)

  txt <- np_expr %..% {
    deparse(.)
    gsub("^    ","  ", .)
    paste(., collapse="\n")
  }
  txt
}