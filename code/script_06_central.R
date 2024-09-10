
# setup -------------------------------------------------------------------

rm(list = ls())

## load library
library(tidyverse)


# central tendency --------------------------------------------------------

# construct vectors x and y
x <- c(15.9, 15.1, 21.9, 13.3, 24.4)
y <- c(15.9, 15.1, 21.9, 53.3, 24.4)

# calculate arithmetic means for x and y
## for x
mean(x)

## for y
sum(y) / length(y)
mean(y)

# calculate geometric mean
## prod() multiply all elements in a vector
prod(x)^(1 / length(x))

## or mean in a log scale then transform back to an ordinary scale
log_y <- log(y)
exp(mean(log_y))

prod(y) ^ (1 / length(y))

# calculate median for x and y
## use median() for x
median(x)

## for y
index <- (length(y) + 1) / 2
sort(y)[index]

median(y)


# variation ---------------------------------------------------------------

## calculate variance and sd for x and y
## manual calculation
## use sum(), length(), ^, sqrt()

## variance
sig2_x <- sum((x - mean(x))^2) / length(x)

## standard dev
sqrt(sig2_x)

## variance
sig2_y <- sum((y - mean(y))^2) / length(y)

## standard dev
sqrt(sig2_y)


## quantile range for x and y
## quantile()
x25 <- quantile(x, 0.25)
x75 <- quantile(x, 0.75)
(iqr_x <- abs(x25 - x75))

## use bracket, 
yq <- quantile(y, c(0.25, 0.75))
(iqr_y <- abs(yq[1] - yq[2]))


# relative variance -------------------------------------------------------

## coefficient of variaiton
## cv for x and y
## sd over mean
(cv_x <- sqrt(sig2_x) / mean(x))
(cv_y <- sqrt(sig2_y) / mean(y))

## IQR / median
abs(diff(quantile(x, c(0.25, 0.75))) / median(x))
abs(diff(quantile(y, c(0.25, 0.75))) / median(y))

