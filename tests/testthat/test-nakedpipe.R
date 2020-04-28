test_that("standard use works", {
  expect_identical(
    transform(head(cars,2), time = dist/speed),
    cars %.% {
      head(2)
      transform(time = dist/speed)
    })
  expect_identical(
    transform(head(cars,2), time = dist/speed),
    cars %..% {
      head(.,2)
      transform(.,time = dist/speed)
    })
})


test_that("naked pipe works on calls", {
  expect_identical(
    head(cars,2),
    cars %.% head(2))
  expect_identical(
    head(cars,2),
    cars %..% head(., 2))
  expect_identical(
    head(cars),
    cars %.% head())
})

test_that("naked pipe works on symbols", {
  expect_identical(
    head(cars),
    cars %.% head)
  expect_identical(
    head,
    cars %..% head)
})

suppressWarnings(library(dplyr, warn.conflicts = FALSE,quietly = TRUE, verbose = FALSE))
test_that("naked pipe works in functions with lazy evaluation and quasiquotation", {
  max_by1 <- function(data, var, by) {
    data %>%
      group_by({{ by }}) %>%
      summarise(maximum = max({{ var }}, na.rm = TRUE))
  }
  max_by2 <- function(data, var, by) {
    data %.% {
      dplyr::group_by({{ by }})
      dplyr::summarise(maximum = max({{ var }}, na.rm = TRUE))
    }
  }

  expect_identical(
      starwars %.% max_by2(height),
      starwars %>% max_by1(height))

  expect_identical(
    starwars %.% max_by2(height, by = gender),
    starwars %>% max_by1(height, by = gender))
})

test_that("assignment pipes work", {
  cars2 <- cars
  cars2 %<.% {
    head(2)
    transform(time = dist/speed)
  }
  expect_identical(
    transform(head(cars,2), time = dist/speed),
    cars2)

  cars2 <- cars
  cars2 %<..% {
    head(.,2)
    transform(.,time = dist/speed)
  }
  expect_identical(
    transform(head(cars,2), time = dist/speed),
    cars2)
})

test_that("side effects work", {
  expect_message(  cars %.% {
    head(2)
    ~~ message("hello")
    transform(time = dist/speed)
  },
  "hello")

  cars %.% {
    head(2)
    ~~ x <- .
    transform(time = dist/speed)
  }
  expect_identical(x, head(cars, 2))

  cars %.% {
    head(2)
    ~~ . -> x
    transform(time = dist/speed)
  }
  expect_identical(x, head(cars, 2))

  expect_error(  cars %.% {
    head(2)
    ~ x <- 3
    transform(time = dist/speed)
  },
  "Wrong")

})

test_that("log pipes work", {
  expect_identical(cars %L.% {
    head(2)
    transform(time = dist/speed)
  },
  transform(head(cars,2), time = dist/speed))

  x <- cars
  expect_identical(x %<L.% {
    head(2)
    transform(time = dist/speed)
  },
  transform(head(cars,2), time = dist/speed))

  expect_identical(cars %L.% head, head(cars))
  x <- cars
  expect_identical(x %<L.% head, head(cars))

})

test_that("debug pipe works", {
  expect_identical(cars %D.% {
    head(2)
    transform(time = dist/speed)
  },
  transform(head(cars,2), time = dist/speed))
  expect_identical(cars %D.% head, head(cars))

  expect_message(  cars %D.% {
    head(2)
    ~~ message("hello")
    transform(time = dist/speed)
  },
  "hello")

  expect_error(  cars %D.% {
    head(2)
    ~ x <- 3
    transform(time = dist/speed)
  },
  "Wrong")

  cars %D.% {
    head(2)
    ~~ x <- .
    transform(time = dist/speed)
  }
  expect_identical(x, head(cars, 2))

})

