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
