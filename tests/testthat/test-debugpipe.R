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


iris %D.% {
  head(2)
  ~~ a <- .
}
