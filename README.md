Assignment 4
================
Edy Kim
10/24/2020

## Problem 1

# Total row sums

fun1 \<- function(mat) { n \<- nrow(mat) ans \<- double(n) for (i in
1:n) { ans\[i\] \<- sum(mat\[i, \]) } ans }

fun1alt \<- function(mat) { \# YOUR CODE HERE }

# Cumulative product by row

fun2 \<- function(mat) { n \<- nrow(mat) K \<- ncol(mat) ans \<- mat for
(i in 1:n) { for (j in 2:k) ans\[i,j\] \<- mat\[i, k\] + ans\[i, k - 1\]
} } ans }

fun2alt \<- function(mat) { \# YOUR CODE HERE }

# Use the data with this code

set.seed(2315) dat \<- matrix(rnorm(200 \* 100), nrow = 200)

# Test for the first

microbenchmark::microbenchmark( fun1(dat), fun1alt(dat), unit =
“relative”, check = “equivalent” )

# Test for the second

microbenchmark::microbenchmark( fun2(dat), fun2alt(dat), unit =
“relative”, check = “equivalent” )
