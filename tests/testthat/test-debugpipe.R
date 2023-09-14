test_that("standard debug pipe works", {
  debugonce_called <- 0
  testthat::local_mocked_bindings(.package = "base", debugonce = function(...) {
    debugonce_called <<- debugonce_called + 1
  })

  expect_identical(cars %D.% {
    head(2)
    transform(time = dist/speed)
  },
  transform(head(cars,2), time = dist/speed))
  expect_equal(debugonce_called, 1)
  expect_identical(cars %D.% head(2), head(cars, 2))
  expect_equal(debugonce_called, 2)
})


test_that("debug pipe works with data manipulation shorthands", {
  debugonce_called <- 0
  testthat::local_mocked_bindings(.package = "base", debugonce = function(...) {
    debugonce_called <<- debugonce_called + 1
  })

  expect_identical(cars %D.% {
    head(2)
    time = dist/speed
  },
  transform(head(cars,2), time = dist/speed))
  expect_equal(debugonce_called, 1)

  expect_identical(cars %D.% {
    speed > 15
  },
  subset(cars, speed > 15))
  expect_equal(debugonce_called, 2)
})

test_that("debug pipe works with dt syntax", {
  debugonce_called <- 0
  testthat::local_mocked_bindings(.package = "base", debugonce = function(...) {
    debugonce_called <<- debugonce_called + 1
  })

  expect_identical(cars %D.% {
    .dt[1:2,]
  },
  head(cars,2))
  expect_equal(debugonce_called, 1)
})


test_that("debug pipe works with if syntax", {
  debugonce_called <- 0
  testthat::local_mocked_bindings(.package = "base", debugonce = function(...) {
    debugonce_called <<- debugonce_called + 1
  })

  expect_identical(cars %D.% {
    if (TRUE) head(.)
  },
  head(cars))
  expect_equal(debugonce_called, 1)

  expect_identical(cars %D.% {
    if (FALSE) head(.)
  },
  cars)
  expect_equal(debugonce_called, 2)

  expect_identical(cars %D.% {
    if (FALSE) head(.) else tail(.)
  },
  tail(cars))
  expect_equal(debugonce_called, 3)
})



test_that("debug pipe works with side effects", {
  debugonce_called <- 0
  testthat::local_mocked_bindings(.package = "base", debugonce = function(...) {
    debugonce_called <<- debugonce_called + 1
  })

  # standard side effects
  expect_message(  cars %D.% {
    head(2)
    ~~ message("hello")
    transform(time = dist/speed)
  },
  "hello")
  expect_equal(debugonce_called, 1)

  # assignments

  expect_error(  cars %D.% {
    head(2)
    ~ x <- 3
    transform(time = dist/speed)
  },
  "Wrong")
  expect_equal(debugonce_called, 1)

  cars %D.% {
    head(2)
    ~~ x <- .
    transform(time = dist/speed)
  }
  expect_identical(x, head(cars, 2))
  expect_equal(debugonce_called, 2)

  cars %D.% {
    ~~ .x <- .
  }
  expect_false(exists(".x"))
  expect_equal(debugonce_called, 3)

  cars %D.% {
    head(2)
    ~~ . -> x
    transform(time = dist/speed)
  }
  expect_identical(x, head(cars, 2))
  expect_equal(debugonce_called, 4)

  cars %D.% {
    ~~ . -> .x
  }
  expect_false(exists(".x"))
  expect_equal(debugonce_called, 5)


})
