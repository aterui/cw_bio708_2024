
# setup -------------------------------------------------------------------

rm(list = ls())

## load library
library(tidyverse)


# Comparing Central Tendency Measures -------------------------------------

## - create a vector `z`
z <- exp(rnorm(n = 1000,
               mean = 0,
               sd = 1))

## - arithmetic, geometric, median
mu_a <- mean(z)
mu_g <- prod(z)^(1 / length(z))
mu_m <- median(z)

## create a tibble with vector `z`
df_z <- tibble(z = z)

## draw histogram
g_hist <- df_z %>% 
  ggplot(aes(x = z)) +
  geom_histogram()

## draw a vertical line
g_hist2 <- g_hist +
  geom_vline(xintercept = mu_a,
             color = "salmon") +
  geom_vline(xintercept = mu_g,
             color = "green") +
  geom_vline(xintercept = mu_m,
             color = "steelblue") +
  theme_bw()
  

## Comparing Central Tendency Measures
## - create a vector `z`
z <- exp(rnorm(n = 1000,
               mean = 0,
               sd = 1))

z_rev <- -z + max(z) + 0.1

## - arithmetic, geometric, median
mu_a <- mean(z_rev)
mu_g <- exp(mean(log(z_rev)))
mu_m <- median(z_rev)

## create a tibble with vector `z_rev`
df_z_rev <- tibble(z_rev = z_rev)

## draw histogram
g_hist <- df_z_rev %>% 
  ggplot(aes(x = z_rev)) +
  geom_histogram()

## draw a vertical line
df_mu <- tibble(mu = c(mu_a, mu_g, mu_m),
                type = c("Arithmetic", "Geometric", "Median"))

g_hist +
  geom_vline(data = df_mu,
             aes(xintercept = mu,
                 color = type)) +
  theme_bw()


# Comparing Variation Measures --------------------------------------------

w <- rnorm(100, mean = 10, sd = 1)
head(w) # show first 10 elements in w

# w in [g] -> m in [mg]
m <- w * 1000
(sigma_w <- sqrt(sum((w - mean(w))^2) / length(w)))
(sigma_m <- sqrt(sum((m - mean(m))^2) / length(m)))

mad_w <- median(abs(w - median(w)))
mad_m <- median(abs(m - median(m)))

## cv
sigma_w / mean(w)
sigma_m / mean(m)

## MAD / median
mad_w / median(w)
mad_m / median(m)
