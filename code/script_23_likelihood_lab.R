
# setup -------------------------------------------------------------------

rm(list = ls())
source(here::here("code/set_library.R"))


# binomial ----------------------------------------------------------------

y <- c(2, 2, 0, 0, 3, 1, 3, 3, 4, 3)
prob <- seq(0, 1, by = 0.01)

## likelihood
## prod(dbinom(...))

## for loop version
lh <- NULL
for (i in 1:length(prob)) {
  lh[i] <- prod(dbinom(x = y,
                       size = 10,
                       prob = prob[i]))
}

## sapply() version
lh <- sapply(prob, function(x) prod(dbinom(x = y,
                                           size = 10,
                                           prob = x)))

## find the best prob
df_p <- tibble(prob = prob,
               lh = lh) %>%
  arrange(desc(lh))

## sample mean
mean(y/10)
