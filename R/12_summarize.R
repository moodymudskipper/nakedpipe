# summarize ungrouped data

np_summarise <- function(.data, ..., env) {
  exprs <- eval(substitute(alist(...)))
  # expand, using `?`

  names(exprs) <- allNames(exprs)
  # set names for `=` expressions
  for (i in seq_along(exprs)) {
    if(has_equal(exprs[[i]])) {
      names(exprs)[[i]] = as.character(exprs[[i]][[2]])
      exprs[[i]] <- exprs[[i]][[3]]
    }
  }

  expand <- function(expr, nm) {
    # if no question mark don't change
    if(! "?" %in% all.names(expr)) {
      return(expr)
    }
    # pull the question mark calls, and extract the rhs of the informative one
    ind <- call_pull(expr)
    ind <- setdiff(ind, list(quote(.)))
    if(length(ind) > 1)
      stop("If you use several `?` in a call, only one should not be `?.`")
    ind <- ind[[1]]

    # evaluate it and infer the indices if it evaluates to a function
    ind <- eval(ind, env)
    if(is.function(ind)) {
      ind <- sapply(.data, ind)
      if(!is.logical(ind))
        stop("`?` should only be used on functions that return a boolean")
      ind <- names(.data)[ind]
    }

    # use call_sub to build a list of substituted calls
    res <- call_sub(expr, target = quote(`?`), replacement = sapply(ind, as.name))
    if(nm != "") {
      if(!requireNamespace("glue")) {
        stop("You must install the package {glue} to use this feature.")
      }
      names(res)[] <- eval(
      substitute(glue::glue(nm), list(nm = nm)),
      list(col = names(res), fn = deparse(expr[[1]])), enclos = env)
    }
    res
  }

  # deal with name concatenation that happens with unlist
  exprs <- Map(expand, exprs, names(exprs))
  for (i in seq_along(exprs)) {
    if (is.list(exprs[[i]])) names(exprs)[i] <- ""
  }
  exprs <- unlist(exprs)

  res <- sapply(
    exprs,
    function(expr) {
      eval(expr, envir = .data, enclos = env)
    }, simplify = FALSE)

  if(length(unique(c(1,lengths(res)))) > 2) {
    stop("The different outputs must have the same length or a length of 1.")
  }
  nms <- allNames(res)
  res <- as.data.frame(res)
  names(res)[nms == ""] <- sapply(exprs[nms == ""], deparse)
  res
}


#' compute by group
#'
#' `compute_by_group()` is meant to be used through the `{expr} ~ by` syntax
#' in a call to the nakedpipe, and might be encountered when using the debugging pipe.
#'
#' @param data data
#' @param expr expression
#' @param by variables to group by
#'
#' @export
compute_by_group <- function(data, expr, by) {
  env <- parent.frame()
  by_expr <- substitute(by)
  #~~~~~~~~~~~~~~~~~~~~
  # split data "by"
  data_vars <- names(data)
  by_vars0 <- all.names(by_expr)
  if("?" %in% by_vars0) {
    stop("`?` in `by` is not supported yet!")
  } else if (length(setdiff(by_vars0, c(data_vars, "+", "-", "*", ":")))) {
    stop("The only allowed operators in the `by` clause are ",
         "`+`, `-`, `*`, and  `:`")
  } else {
    if(any(c("-", "*", ":") %in% by_vars0))
      stop(" `-`, `*`, and  `:` in `by` are not supported yet")
    by_vars <- setdiff(by_vars0, c("+", "-", "*", ":"))
    if(! all(by_vars %in% data_vars))
      stop("some variables are not in data")
  }
  split_data <- split(
    `[<-`(data, by_vars, value = NULL),
    lapply(data[by_vars], factor, exclude = NULL),
    drop = TRUE)

  groups <- unique(data[by_vars])

  split_groups <- split(groups, seq(nrow(groups)))

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # preprocess expr
  expr <- substitute(expr)
  expr <- insert_dot(expr)

  if(identical(expr[[1]], quote(`{`))) {
    expr <- as.call(c(quote(np_summarise), quote(.), env = env, as.list(expr[-1])))
  }

  fun <- as.function(c(alist(.=, .groups =), expr))

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # apply aggregation
  split_data_modified <- Map(fun, split_data, split_groups)

  fun <- function(., .groups) {
    res <- cbind(., .groups) # in that order for row.names issues
    res[c(by_vars, setdiff(names(res), by_vars))]
  }

  split_data_modified <- Map(fun, split_data_modified, split_groups)
  res     <- do.call("rbind", split_data_modified)
  row_nms <- unlist(lapply(split_data_modified, row.names))
  if (all(row_nms %in% row.names(data)) && !anyDuplicated(row_nms))
    row.names(res) <- row_nms
  else
    row.names(res) <- NULL
  res
}


# compute_by_group(starwars, np_summarise(mean(?is.numeric, na.rm = TRUE), head(?is.character, 1)), by = gender + sex)
# compute_by_group(starwars, {
#   mean(?is.numeric, na.rm = TRUE)
#   head(?is.character, 1) }, by = gender + sex)
