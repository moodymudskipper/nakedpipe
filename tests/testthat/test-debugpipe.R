test_that("standard debug pipe works", {
  expect_identical(cars %D.% {
    head(2)
    transform(time = dist/speed)
  },
  transform(head(cars,2), time = dist/speed))
})


test_that("debug pipe works with data manipulation shorthands", {
  expect_identical(cars %D.% {
    head(2)
    time = dist/speed
  },
  transform(head(cars,2), time = dist/speed))

  expect_identical(cars %D.% {
    speed > 15
  },
  subset(cars, speed > 15))
})

test_that("debug pipe works with dt syntax", {

  expect_identical(cars %D.% {
    .dt[1:2,]
  },
  head(cars,2))
})


test_that("debug pipe works with if syntax", {
  expect_identical(cars %D.% {
    if (TRUE) head(.)
  },
  head(cars))

  expect_identical(cars %D.% {
    if (FALSE) head(.)
  },
  cars)

  expect_identical(cars %D.% {
    if (FALSE) head(.) else tail(.)
  },
  tail(cars))
})



test_that("debug pipe works with side effects", {
  # standard side effects
  expect_message(  cars %D.% {
    head(2)
    ~~ message("hello")
    transform(time = dist/speed)
  },
  "hello")

  # assignments

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

  cars %D.% {
    ~~ .x <- .
  }
  expect_false(exists(".x"))

  cars %D.% {
    head(2)
    ~~ . -> x
    transform(time = dist/speed)
  }
  expect_identical(x, head(cars, 2))

  cars %D.% {
    ~~ . -> .x
  }
  expect_false(exists(".x"))


})
