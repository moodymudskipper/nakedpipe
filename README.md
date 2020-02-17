
<!-- README.md is generated from README.Rmd. Please edit that file -->

# nakedpipe

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
#> 1 %>%         131.5us  140.5us     6714.   119.5KB     14.1
#> 2 %.%          49.6us   54.8us    15396.        0B     12.3
#> 3 %..%         23.2us   25.4us    32770.    18.3KB     13.1
```

The package is written around the good parts of my *fastpipe*
experiment. I could have included it there but I’d rather not override
`%>%`.
