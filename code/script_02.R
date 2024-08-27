
# setup -------------------------------------------------------------------

rm(list = ls())

# exercise 1 - vector -----------------------------------------------------

## numeric
nv1 <- c(1, 3, 9)
nv2 <- seq(1, 100, length = 6)
nv3 <- rep(1, 20)

## character
cv1 <- c("a", "b", "c")
cv2 <- rep("a", 6)
cv3 <- letters[1:20]

## random
set.seed(1)
x <- rnorm(100)

## element id
which(x > 2)

## element values
x[x > 2]
x[which(x > 2)]

# exercise 2 - matrix -----------------------------------------------------

## numeric
nm1 <- cbind(rep(1, 4),
             rep(2, 4),
             rep(3, 4),
             rep(4, 4))

# you can do: nm2 <- rbind(...) for row binding but
nm2 <- matrix(rep(1:4, each = 4),
              nrow = 4,
              ncol = 4,
              byrow = TRUE)

## character
cm1 <- cbind(rep("a", 4),
             rep("b", 4),
             rep("c", 4),
             rep("d", 4))

# you can do: nm2 <- rbind(...) for row binding but
cm2 <- matrix(rep(letters[1:4], each = 4),
              nrow = 4,
              ncol = 4,
              byrow = TRUE)

set.seed(1)
x <- matrix(rnorm(100), nrow = 10, ncol = 10)

## element id
which(x > 2, arr.ind = TRUE)

## mean
mean(x[x > 2])

# exercise 3 - data frame -------------------------------------------------

set.seed(1)
x <- rnorm(100, mean = 10, sd = 3)
y <- rpois(100, lambda = 10)
z <- rep(c("VA", "NC"), 50)
df0 <- data.frame(temperature = x, abundance = y, state = z)

v_va <- df0$temperature[df0$state == "VA"]
v_nc <- df0$temperature[df0$state == "NC"]

mu_va <- mean(v_va)
mu_nc <- mean(v_nc)

## extra exercise; alternative way
v_va <- with(df0, temperature[state == "VA"])
v_nc <- with(df0, temperature[state == "NC"])

## extra exercise; alternative way tapply()
v_mu <- tapply(df0$temperature,
               INDEX = df0$state,
               FUN = mean)

v_mu <- with(df0,
             tapply(temperature,
                    INDEX = state,
                    FUN = mean))
