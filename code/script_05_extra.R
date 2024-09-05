
# setup -------------------------------------------------------------------

rm(list = ls())

## load library
library(tidyverse)

iris <- as_tibble(iris)


# function ----------------------------------------------------------------

## function() is to create a new function
x <- rnorm(100, mean = 10, sd = 100)
y <- rnorm(100, mean = 10, sd = 100)
z <- rnorm(100, mean = 10, sd = 100)

## without function
sig <- sd(x)
mu <- mean(x)
cv <- sig / mu
print(cv)

## with function
fun_cv <- function(x) {
  cv <- sd(x) / mean(x)
  return(cv)
}

fun_cv(x)
fun_cv(y)
fun_cv(z)

## standardize
x0 <- x - mean(x) # centering
z <- x0 / sd(x0) # scaling

scl <- function(phi) {
  z <- (phi - mean(phi)) / sd(phi)
  return(z)
}

z <- scl(phi = x)
mean(z)
sd(z)

## random function with two arguments

f0 <- function(phi, zeta) {
  cout <- 2 * phi + rnorm(1) * zeta
  return(cout)
}

f0(phi = 2, zeta = 3)

# apply family ------------------------------------------------------------

m <- matrix(rnorm(25), nrow = 5, ncol = 5)

## apply() - for matrix mainly
## MARGIN = 1; row-wise operation
## MARGIN = 2; column-wise operation
apply(m, MARGIN = 1, FUN = mean)
apply(m, MARGIN = 2, FUN = mean)

## FUN can be a function you defined
apply(m, MARGIN = 1, FUN = fun_cv)

## for dataframe
apply(iris %>% select(1:4),
      MARGIN = 2, FUN = mean)

## sapply() - for list, vector output
x <- rnorm(10)
y <- rnorm(100)
z <- rnorm(5)

## ugly code
mean(x)
mean(y)
mean(z)

## nicer code
l_xyz <- list(x, y, z)
sapply(l_xyz, FUN = mean)

## lapply() - for list, but output is list too
x <- rpois(10, lambda = 5)
y <- rpois(100, lambda = 5)
z <- rep(letters[1:3], 10)

## create list of xyz
l_xyz <- list(x, y, z)

## remove diplicates, or get unique elements
unique(x)
unique(y)
unique(z)

lapply(l_xyz, FUN = unique)

## try to get only the first element from each vector
## - 1st example
lapply(l_xyz,
       FUN = function(x) {
         x[1]
       })

## - 2nd example
first <- function(x) {
  return(x[1])
}

lapply(l_xyz,
       FUN = first)

# for loop ----------------------------------------------------------------

## for loop is to repeat work inside {}
x <- seq(0, 10, by = 0.25)
y <- NULL

## multiply 2 for each element one by one
for (i in 1:10) {
  y[i] <- 2 * x[i]
}

y <- NULL
for (i in 1:10) {
  y <- 2 * x[i]
}