Assignment 4
================
Edy Kim
10/24/2020

``` r
library(parallel)
library(tidyverse)
library(microbenchmark)
```

# HPC

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
    ##          expr      min       lq     mean   median      uq       max neval cld
    ##     fun1(dat) 5.210412 7.512873 5.575314 7.624748 7.91334 0.5689932   100   b
    ##  fun1alt(dat) 1.000000 1.000000 1.000000 1.000000 1.00000 1.0000000   100  a

``` r
# Test for the second
microbenchmark::microbenchmark(
  fun2(dat),
  fun2alt(dat), unit = "relative", check = "equivalent"
)
```

    ## Unit: relative
    ##          expr      min       lq     mean   median       uq       max neval cld
    ##     fun2(dat) 3.868006 3.746394 2.877186 3.656203 2.942313 0.8950274   100   b
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
    ##    3.28    0.00    3.29

# SQL

``` r
# install.packages(c("RSQLite", "DBI"))

library(RSQLite)
library(DBI)

# Initialize a temporary in memory database
con <- dbConnect(SQLite(), ":memory:")

# Download tables
film <- read.csv("https://raw.githubusercontent.com/ivanceras/sakila/master/csv-sakila-db/film.csv")
film_category <- read.csv("https://raw.githubusercontent.com/ivanceras/sakila/master/csv-sakila-db/film_category.csv")
category <- read.csv("https://raw.githubusercontent.com/ivanceras/sakila/master/csv-sakila-db/category.csv")

# Copy data.frames to database
dbWriteTable(con, "film", film)
dbWriteTable(con, "film_category", film_category)
dbWriteTable(con, "category", category)
```

``` sql
PRAGMA table_info(film)
```

<div class="knitsql-table">

| cid | name                   | type    | notnull | dflt\_value | pk |
| :-- | :--------------------- | :------ | ------: | :---------- | -: |
| 0   | film\_id               | INTEGER |       0 | NA          |  0 |
| 1   | title                  | TEXT    |       0 | NA          |  0 |
| 2   | description            | TEXT    |       0 | NA          |  0 |
| 3   | release\_year          | INTEGER |       0 | NA          |  0 |
| 4   | language\_id           | INTEGER |       0 | NA          |  0 |
| 5   | original\_language\_id | INTEGER |       0 | NA          |  0 |
| 6   | rental\_duration       | INTEGER |       0 | NA          |  0 |
| 7   | rental\_rate           | REAL    |       0 | NA          |  0 |
| 8   | length                 | INTEGER |       0 | NA          |  0 |
| 9   | replacement\_cost      | REAL    |       0 | NA          |  0 |

Displaying records 1 - 10

</div>

``` sql
PRAGMA table_info(film_category)
```

<div class="knitsql-table">

| cid | name         | type    | notnull | dflt\_value | pk |
| :-- | :----------- | :------ | ------: | :---------- | -: |
| 0   | film\_id     | INTEGER |       0 | NA          |  0 |
| 1   | category\_id | INTEGER |       0 | NA          |  0 |
| 2   | last\_update | TEXT    |       0 | NA          |  0 |

3 records

</div>

``` sql
PRAGMA table_info(category)
```

<div class="knitsql-table">

| cid | name         | type    | notnull | dflt\_value | pk |
| :-- | :----------- | :------ | ------: | :---------- | -: |
| 0   | category\_id | INTEGER |       0 | NA          |  0 |
| 1   | name         | TEXT    |       0 | NA          |  0 |
| 2   | last\_update | TEXT    |       0 | NA          |  0 |

3 records

</div>

## Question 1: How many movies is there available in each rating category?

``` sql
SELECT rating,
COUNT(*) AS count
FROM film
GROUP BY rating
```

<div class="knitsql-table">

| rating | count |
| :----- | ----: |
| G      |   180 |
| NC-17  |   210 |
| PG     |   194 |
| PG-13  |   223 |
| R      |   195 |

5 records

</div>

## Question 2: What is the average replacement cost and rental rate for each rating category?

``` sql
SELECT rating,
  AVG(replacement_cost) AS avg_replacement_cost,
  AVG(rental_rate) AS avg_rental_rate
FROM film 
GROUP BY rating
```

<div class="knitsql-table">

| rating | avg\_replacement\_cost | avg\_rental\_rate |
| :----- | ---------------------: | ----------------: |
| G      |               20.12333 |          2.912222 |
| NC-17  |               20.13762 |          2.970952 |
| PG     |               18.95907 |          3.051856 |
| PG-13  |               20.40256 |          3.034843 |
| R      |               20.23103 |          2.938718 |

5 records

</div>

## Question 3: Use table film\_category together with film to find how many films there are with each category ID

## Question 4: Incorporate table category into the answer to the previous question to find the name of the most popular category
