Assignment 4
================
Edy Kim
10/24/2020

``` r
library(parallel)
library(tidyverse)
library(microbenchmark)
```

## Problem 1: Make Sure Your Code is Nice

``` r
# Total row sums
fun1 <- function(mat) {
  n <- nrow(mat)
  ans <- double(n)
  for (i in 1:n) {
    ans[i] <- sum(mat[i, ])
  }
  ans
}

fun1alt <- function(mat) {
  rowSums(mat)
}

# Cumulative sum by row
fun2 <- function(mat) {
  n <- nrow(mat)
  k <- ncol(mat)
  ans <- mat
  for (i in 1:n) {
    for (j in 2:k) {
      ans[i,j] <- mat[i, j] + ans[i, j - 1]
    }
  }
  ans
}


fun2alt <- function(mat) {
  t(apply(dat,1,cumsum))
}

# Use the data with this code
set.seed(2315)
dat <- matrix(rnorm(200 * 100), nrow = 200)

# Test for the first
microbenchmark::microbenchmark(
  fun1(dat),
  fun1alt(dat), unit = "relative", check = "equivalent")
```

    ## Unit: relative
    ##          expr     min       lq     mean   median       uq       max neval cld
    ##     fun1(dat) 5.00885 7.585313 5.809832 7.851891 8.304829 0.5123742   100   b
    ##  fun1alt(dat) 1.00000 1.000000 1.000000 1.000000 1.000000 1.0000000   100  a

``` r
# Test for the second
microbenchmark::microbenchmark(
  fun2(dat),
  fun2alt(dat), unit = "relative", check = "equivalent"
)
```

    ## Unit: relative
    ##          expr      min       lq     mean   median       uq       max neval cld
    ##     fun2(dat) 3.995717 3.833478 2.786929 3.480228 2.839371 0.7626413   100   b
    ##  fun2alt(dat) 1.000000 1.000000 1.000000 1.000000 1.000000 1.0000000   100  a

After fun1 was altered to be more efficient, it was determined that the
alternate fun1alt was overall faster in processing than the original
function. On average, the fun1alt function was around 5.6941 times
faster than the original fun1 function.

After fun2 was altered to be more efficient, it was determined that the
alternate fun2alt was overall faster in processing than the origina
function. On average, the fun2alt function was around 3.296 times faster
than the original fun2 function.

``` r
sim_pi <- function(n = 1000, i = NULL) {
  p <- matrix(runif(n*2), ncol = 2)
  mean(rowSums(p^2) < 1) * 4
}

# Here is an example of the run
set.seed(156)
sim_pi(1000) # 3.132
```

    ## [1] 3.132

``` r
# This runs the simulation a 4,000 times, each with 10,000 points
set.seed(1231)
system.time({
  ans <- unlist(lapply(1:4000, sim_pi, n = 10000))
  print(mean(ans))
})
```

    ## [1] 3.14124

    ##    user  system elapsed 
    ##    3.41    0.00    3.41
