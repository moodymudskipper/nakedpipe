
<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![Travis build
status](https://travis-ci.org/moodymudskipper/nakedpipe.svg?branch=master)](https://travis-ci.org/moodymudskipper/nakedpipe)
[![Codecov test
coverage](https://codecov.io/gh/moodymudskipper/nakedpipe/branch/master/graph/badge.svg)](https://codecov.io/gh/moodymudskipper/nakedpipe?branch=master)
<!-- badges: end -->

# nakedpipe <img src='man/figures/logo.png' align="right" height="139" />

Pipe into a sequence of calls without repeating the pipe symbol.

This is inspired by Stefan Bache and Hadley Wickham’s *magrittr* pipe
and behaves consistently, though it doesn’t use its code.

The package is very light (25 lines of code + doc).

Install with :

``` r
remotes::install_github("moodymudskipper/nakedpipe")
```

## Example

``` r
library(nakedpipe)
cars %.% {
  head(2)
  transform(time = dist/speed)
} -> res
res
#>   speed dist time
#> 1     4    2  0.5
#> 2     4   10  2.5
```

## Benchmark

We’re a bit faster than *magrittr*, if you want to be even faster use
`%..%` with explicit dots (though keep in mind these are micro
seconds\!):

``` r
library(magrittr)
bench::mark(iterations = 10000,
  `%>%` = cars %>% 
    identity %>%
    identity() %>%
    identity(.) %>%
    {identity(.)},
  `%.%` = cars %.% {
    identity
    identity()
    identity(.)
    {identity(.)}
  },
  `%..%` = cars %..% {
    identity(.)
    identity(.)
    identity(.)
    {identity(.)}
   }
)
#> # A tibble: 3 x 6
#>   expression      min   median `itr/sec` mem_alloc `gc/sec`
#>   <bch:expr> <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl>
#> 1 %>%          98.3us  125.2us     6988.   119.5KB     14.7
#> 2 %.%          37.3us   43.7us    21483.        0B     17.2
#> 3 %..%         16.7us   20.2us    46960.    24.1KB     18.8
```

The package is written around the good parts of my *fastpipe*
experiment. I could have included it there but I’d rather not override
`%>%`.
