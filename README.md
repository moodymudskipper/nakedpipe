
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

The approach of *nakedpipe* spares typing and visual space, the package
also proposes some convenient logging and debugging features and is a
bit more performant than its dressed counterpart.

Install with :

``` r
remotes::install_github("moodymudskipper/nakedpipe")
```

## Examples

``` r
library(nakedpipe)
```

Pipe into a sequence of calls using `%.%`:

``` r
cars %.% {
  subset(speed < 6)
  transform(time = dist/speed)
}
#>   speed dist time
#> 1     4    2  0.5
#> 2     4   10  2.5
```

It plays well with left to right assignment:

``` r
cars %.% {
  subset(speed < 6)
  transform(time = dist/speed)
} -> res
```

Use `~~` for side effects:

``` r
cars %.% {
  subset(speed < 6)
  ~~ message("nrow:", nrow(.))
  transform(time = dist/speed)
}
#> nrow:2
#>   speed dist time
#> 1     4    2  0.5
#> 2     4   10  2.5
```

This include assignments:

``` r
cars %.% {
  subset(speed < 6)
  ~~ cars_h <- . # or ~~ . -> cars_h
  transform(time = dist/speed)
}
#>   speed dist time
#> 1     4    2  0.5
#> 2     4   10  2.5
cars_h
#>   speed dist
#> 1     4    2
#> 2     4   10
```

To assign to a temp variable, use a dotted name:

``` r
cars %.% {
  ~~ .n <- 6
  subset(speed < .n)
  transform(time = dist/speed)
}
#>   speed dist time
#> 1     4    2  0.5
#> 2     4   10  2.5
exists(".n")
#> [1] FALSE
```

Use `if` for conditional step. if the condition is not TRUE and there is
no `else` clause the data is unchanged:

``` r
cars %.% {
  subset(speed < 6)
  if(ncol(.) < 5) transform(time = dist/speed)
}
#>   speed dist time
#> 1     4    2  0.5
#> 2     4   10  2.5

cars %.% {
  subset(speed < 6)
  if(ncol(.) > 5) transform(time = dist/speed)
}
#>   speed dist
#> 1     4    2
#> 2     4   10
```

for the very common `subset()` and `transform()` operations, shorthands
are available, so that for our first exaple we could simply write:

``` r
cars %.% {
  speed < 6 # any call to < > <= >= == != %in% & | is interpreted as a subset call
  time = dist/speed # any call to = is interpreted as a transform call
}
#>   speed dist time
#> 1     4    2  0.5
#> 2     4   10  2.5
```

We can use *data.table* syntax for one step by using `.dt[...]`, the
output will be of the same class of the input (the temporary conversion
to *data.table* is invisible):

``` r
cars %.% {
  speed < 8
  time = dist/speed
  .dt[, .(mmean_time = mean(time)), by = speed]
}
#>   speed mmean_time
#> 1     4   1.500000
#> 2     7   1.857143
```

We can chain *data.table* brackets too:

``` r
cars %.% {
  .dt[speed < 8][, time := dist/speed][,.(mmean_time = mean(time)), by = speed]
}
#>   speed mmean_time
#> 1     4   1.500000
#> 2     7   1.857143
```

## Additional pipes

Assign in place using `%<.%`

``` r
cars_copy <- cars
cars_copy %<.% {
  head(2)
  ~~ message("nrow:", nrow(.))
  transform(time = dist/speed)
}
#> nrow:2
cars_copy
#>   speed dist time
#> 1     4    2  0.5
#> 2     4   10  2.5
```

Clock each step using `%L.%`

``` r
cars %L.% {
  head(2)
  ~~ Sys.sleep(1)
  transform(time = dist/speed)
}
#> cars %L.% {
#>   head(2)
#>    user  system elapsed 
#>       0       0       0
#>   ~~Sys.sleep(1)
#>    user  system elapsed 
#>       0       0       1
#>   transform(time = dist/speed)
#>    user  system elapsed 
#>       0       0       0
#> }
#>   speed dist time
#> 1     4    2  0.5
#> 2     4   10  2.5
```

`print()` the output of each step using `%P.%`

``` r
cars %P.% {
  head(2)
  transform(time = dist/speed)
}
#> cars %P.% {
#>   head(2)
#>   speed dist
#> 1     4    2
#> 2     4   10
#>   transform(time = dist/speed)
#>   speed dist time
#> 1     4    2  0.5
#> 2     4   10  2.5
#> }
#>   speed dist time
#> 1     4    2  0.5
#> 2     4   10  2.5
```

`View()` the output of each step using `%V.%`

``` r
cars %V.% {
  head(2)
  transform(time = dist/speed)
}
```

`%..%` is faster at the cost of using explicit dots

``` r
cars %..% {
  head(.,2)
  transform(.,time = dist/speed)
}
#>   speed dist time
#> 1     4    2  0.5
#> 2     4   10  2.5
```

It is better suited for programming and doesn’t support side effect
notation but you can do :

``` r
cars %..% {
  head(.,2)
  {message("nrow:", nrow(.)); .}
  transform(.,time = dist/speed)
}
#> nrow:2
#>   speed dist time
#> 1     4    2  0.5
#> 2     4   10  2.5
```

Create a function using `%F.%` on `.`

``` r
fun <- . %F.% {
  head(.,2)
  transform(.,time = dist/speed)
}
fun(cars)
#>   speed dist time
#> 1     4    2  0.5
#> 2     4   10  2.5
```

Apply a sequence of calls on all elements using `%lapply.%`

``` r
replicate(2, cars, simplify = FALSE) %lapply.% {
  head(.,2)
  transform(.,time = dist/speed)
}
#> [[1]]
#>   speed dist time
#> 1     4    2  0.5
#> 2     4   10  2.5
#> 
#> [[2]]
#>   speed dist time
#> 1     4    2  0.5
#> 2     4   10  2.5
```

See `?"%.%"` and `?"%lapply.%"` to see all available pipes (including
variants of the above).

## Debugging

The `%D.%` pipe allows you to step through the calls one by one.

``` r
# Debug the pipe using `%D.%`
cars %D.% {
  head(2)
  transform(time = dist/speed)
}
```

You could also inster a browser() call as a side effect at a chosen
step.

``` r
# Debug the pipe using `%D.%`
cars %D.% {
 head(2)
 ~~ browser()
 transform(time = dist/speed)
}
```

## *ggplot2*

It’s a little known trick that you can use *magrittr*’s pipe with
*ggplot2* if you pipe to the `+` symbol. It is convenient if you want to
use the ggplot object as the input of another function without
intermediate variables of bracket overload :

``` r
library(ggplot2)
path <- tempfile()
cars %>%
  head() %>% 
  ggplot(aes(speed, dist)) %>%
  + geom_point() %>%
  + ggtitle("head(cars)") %>%
  saveRDS(path)

# rather than 
plt <- cars %>%
  head() %>% 
  ggplot(aes(speed, dist)) + 
  geom_point() +
  ggtitle("head(cars)")
saveRDS(plt, path)
```

The former case above shows operators on both sides, which looks a bit
complicated, the latter requires a temporary variable and we must look
at the end of the previous line to know what kind of piping was done.

In both cases additionally if I chose to comment out the
`ggtitle("head(cars)")` line, I should also comment the last operator at
the end of the previous line.

With *nakedpipe* we can write :

``` r
cars %.% {
  head()
  ggplot(aes(speed, dist))
  + geom_point()
  + ggtitle("head(cars)")
  saveRDS(path)
}
```

`+` signs are neatly alligned, it’s obvious where the *ggplot* chain
starts and ends, and trivial to pipe it to another instruction or to
comment a line.

## Conversion to magrittr syntax and back

We provide an addin to ease the conversion.

![Alt
Text](https://user-images.githubusercontent.com/18351714/84393270-add85b80-abfb-11ea-8a7d-3ec4c8c59d55.gif)

## Benchmark

We’re a bit faster than *magrittr*, if you want to be even faster use
`%..%` with explicit dots, though keep in mind these are micro seconds
and that the fastest solution is always not to use pipes at all.

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
   },
  `base` = {
    . <- cars
    . <- identity(.)
    . <- identity(.)
    . <- identity(.)
    . <- identity(.)
   }
)
#> # A tibble: 4 x 6
#>   expression      min   median `itr/sec` mem_alloc `gc/sec`
#>   <bch:expr> <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl>
#> 1 %>%         101.2us  111.4us     8405.     118KB     19.4
#> 2 %.%          85.4us   94.9us     9286.      280B     24.2
#> 3 %..%         18.7us   20.2us    45959.        0B     23.0
#> 4 base          1.8us      2us   459924.        0B      0
```

## Snippets

Runing `setup_nakedpipe_snippets()` will open RStudio’s snippet file so
you can add our suggested snippets there. Follow the instructions and
you’ll be able to type :

``` r
cars . # + 2 time the <tab> key
```

and display :

``` r
cars %.% {
  # with the cursor conveniently placed here
}
```

(or type `..` to get the `%..%` equivalent)

## Similar efforts

*nakedpipe* is heavily inspired by *magrittr* and follows the same dot
insertion rules.

Alternative pipes are available on *CRAN*, at the time of writing and to
my knowlege, in packages *wrapr* and *pipeR*. The latter includes a
function `pipeline()` that allows piping a sequence of calls in a
similar fashion as *nakedpipe*
