# setup -------------------------------------------------------------------

rm(list = ls())
source(here::here("code/set_library.R"))


# exercise 1 --------------------------------------------------------------

# 1. Generate a variable with 50 observations.

mu <- 10
sigma <- 0.2
x1 <- rnorm(n = 50, mean = mu, sd = sigma)

# 2. Create a figure similar to Figure 3.3

x1_min <- floor(min(x1))
x1_max <- ceiling(max(x1))
bin <- seq(x1_min, x1_max, by = 0.1)

p <- sapply(1:(length(bin) - 1),
            FUN = function(i) {
              pnorm(bin[i + 1], mean = mu, sd = sigma) - 
                pnorm(bin[i], mean = mu, sd = sigma)
            })

df_prob <- tibble(x = bin[-length(bin)] + 0.05,
                  p = p) %>% 
  mutate(freq = p * length(x1))

tibble(x1 = x1) %>% 
  ggplot(aes(x = x1)) +
  geom_histogram(binwidth = 0.1,
                 center = 0.05) +
  geom_point(data = df_prob,
             aes(y = freq,
                 x = x),
             color = "tomato") +
  geom_line(data = df_prob,
            aes(y = freq,
                x = x),
            color = "tomato")

# exercise 2 --------------------------------------------------------------
# 1. Generate a variable with 1000 observations.

lambda <- 2
x2 <- rpois(n = 1000, lambda = lambda)

# 2. Create a figure similar to Figure 3.7

x2_min <- min(x2)
x2_max <- max(x2)

bin <- seq(x2_min, x2_max, by = 1)

df_prob <- tibble(x = bin) %>% 
  mutate(p = dpois(bin, lambda = lambda),
         freq = p * length(x2))

tibble(x2 = x2) %>% 
  ggplot(aes(x = x2)) +
  geom_histogram(binwidth = 0.5,
                 center = 0) +
  geom_point(data = df_prob,
             aes(y = freq,
                 x = x),
             color = "tomato") +
  geom_line(data = df_prob,
            aes(y = freq,
                x = x),
            color = "tomato")

