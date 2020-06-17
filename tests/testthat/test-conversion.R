
test_that("nakedpipe_to_magrittr works", {
  expect_identical(
    nakedpipe_to_magrittr(quote(
      iris %<.% head), FALSE),
    "iris %<>% head")

  expect_identical(
    nakedpipe_to_magrittr(quote(
      iris %<.% head), TRUE, assign_op = quote(`<-`), assign_target = quote(`x`)),
    "x <- iris %<>% head")

  expect_identical(
    nakedpipe_to_magrittr(quote(
      iris %.% head), TRUE, assign_op = quote(`<-`), assign_target = quote(`x`)),
    "x <- iris %>% head")

  expect_identical(
    nakedpipe_to_magrittr(quote(
      iris %<.% {head}), FALSE),
    "iris %<>%\n  head")

  expect_identical(
    nakedpipe_to_magrittr(quote(
      iris %<.% {head}), TRUE, assign_op = quote(`<-`), assign_target = quote(`x`)),
    "x <- iris %<>%\n  head")

  expect_identical(
    nakedpipe_to_magrittr(quote(
      iris %.% {head}), TRUE, assign_op = quote(`<-`), assign_target = quote(`x`)),
    "x <- iris %>%\n  head")
})

test_that("connect_np_steps_with_magrittr  works", {
  expect_identical(
    nakedpipe_to_magrittr(quote(
      iris %.% head), FALSE),
    "iris %>% head")

  expect_identical(
    nakedpipe_to_magrittr(quote(
      iris %.% {head}), FALSE),
    "iris %>%\n  head")

  expect_identical(
    nakedpipe_to_magrittr(quote(
      iris %.% {if(TRUE) {.}}), FALSE),
    "iris %>%\n  {\n    if (TRUE) {\n        .\n    }\n    else .\n}")

  expect_identical(
    nakedpipe_to_magrittr(quote(
      iris %.% {if(FALSE) {.} else {.}}), FALSE),
    "iris %>%\n  {\n    if (FALSE) {\n        .\n    }\n    else {\n        .\n    }\n}")

  expect_identical(
    nakedpipe_to_magrittr(quote(
      iris %.% {with(Species)}), FALSE),
    "iris %$%\n  Species")

  expect_identical(
    nakedpipe_to_magrittr(quote(
      iris %.% {ggplot();+ geom_point(...)}), FALSE),
    "iris %>%\n  ggplot() +\n  geom_point(...)")

  expect_error(
    nakedpipe_to_magrittr(quote(
      iris %.% {~~ x <- .}), FALSE),
    "assignments"
  )

  expect_identical(
    nakedpipe_to_magrittr(quote(
      iris %.% {a = 1}), FALSE),
    "iris %>%\n  transform(a = 1)")

  expect_identical(
    nakedpipe_to_magrittr(quote(
      iris %.% {Petal.Length < 30}), FALSE),
    "iris %>%\n  subset(Petal.Length < 30)")

  expect_identical(
    suppressWarnings(nakedpipe_to_magrittr(quote(
      iris %.% {.dt[, a:=1]}), FALSE)),
    "iris %>%\n  data.table::as.data.table() %>%\n  .[, `:=`(a, 1)] %>%\n  as.data.frame()")

  expect_identical(
    suppressWarnings(nakedpipe_to_magrittr(quote(
      iris %.% {.dt[, a:=1][, b:=2]}), FALSE)),
    "iris %>%\n  data.table::as.data.table() %>%\n  {\n    .[, `:=`(a, 1)][, `:=`(b, 2)]\n} %>%\n  as.data.frame()")

  expect_identical(
    suppressWarnings(nakedpipe_to_magrittr(quote(
      iris %.% {~~ message("foo")}), FALSE)),
    "iris %T>%\n  {\n    message(\"foo\")\n}")

  expect_identical(
    suppressWarnings(nakedpipe_to_magrittr(quote(
      iris %.% {~~ print(.)}), FALSE)),
    "iris %T>%\n  print(.)")
})

library(magrittr)
iris %.% {
  ~~{
    message("foo")
  }
}



test_that("magrittr_to_nakedpipe works", {
  expect_identical(
    magrittr_to_nakedpipe(quote(
      iris %<>% head), FALSE),
    "iris %<.% {\n  head\n}")

  expect_identical(
    magrittr_to_nakedpipe(quote(
      iris %<>% head), TRUE, assign_op = quote(`<-`), assign_target = quote(`x`)),
    "x <- iris %<.% {\n  head\n}")

  expect_identical(
    magrittr_to_nakedpipe(quote(
      iris %>% head), TRUE, assign_op = quote(`<-`), assign_target = quote(`x`)),
    "x <- iris %.% {\n  head\n}")

  expect_identical(
    suppressWarnings(magrittr_to_nakedpipe(quote(
      iris %T>% print), FALSE)),
    "iris %.% {\n  ~~print(.)\n}")

  expect_identical(
    magrittr_to_nakedpipe(quote(
      iris %$% Species), FALSE),
    "iris %.% {\n  with(Species)\n}")

  expect_identical(
    magrittr_to_nakedpipe(quote(
      iris %>% ggplot() + geom_point(...)), FALSE),
    "iris %.% {\n  ggplot()\n  +geom_point(...)\n}")

  expect_identical(
    magrittr_to_nakedpipe(quote(
      iris %>% {if(TRUE) {a} else {b}}), FALSE),
    "iris %.% {\n  if (TRUE) {\n      a\n  }\n  else {\n      b\n  }\n}")

  expect_identical(
    magrittr_to_nakedpipe(quote(
      iris %>% {if(TRUE) a else .}), FALSE),
    "iris %.% {\n  if (TRUE) \n      a\n}")


  expect_identical(
    magrittr_to_nakedpipe(quote(
      iris %>% {if(FALSE) {.}}), FALSE),
    "iris %.% {\n  if (FALSE) {\n      .\n  }\n  else NULL\n}")
})
