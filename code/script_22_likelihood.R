
# setup -------------------------------------------------------------------

rm(list = ls())
source(here::here("code/set_library.R"))


# poisson example, likelihood ---------------------------------------------

# dpois()
# the first argument is "k"
# the second argument is "lambda"
# Pr(y = 3) = 3.5^3 exp(-3.5) / 3!
dpois(x = 3, lambda = 3.5)

# write an equation
# probability of observing 3
# if the data follows a Poisson dist. with mean 3.5
3.5^3 * exp(-3.5) / factorial(3)

# change lambda values, calculate probabilities
lambda <- seq(0, 10, by = 0.1)
pr <- dpois(x = 3, lambda = lambda)
df_pr <- tibble(y = 3,
                lambda = lambda,
                pr = pr)

print(df_pr)

# visualize
df_pr %>% 
  ggplot(aes(x = lambda,
             y = pr)) +
  geom_point() +
  theme_bw() +
  labs(y = "Pr(y = 3)",
       x = "lambda (mean of Poisson dist)")

# arrange by probability
df_pr %>% 
  arrange(desc(pr))

# data y = {3, 2, 5}
pr <- dpois(x = c(3, 2, 5), lambda = 3)

# prod(pr) means pr[1] * pr[2] * pr[3]
prod(pr)

# likelihood for y = 3, 2, 5
# lambda = 0 - 10 by 0.01
y <- c(3, 2, 5)
lambda <- seq(0, 10, by = 0.01)

# the following code automate...
# dpois(y, lambda = lambda[1])
# dpois(y, lambda = lambda[2])
# dpois(y, lambda = lambda[3])...
pr <- sapply(X = lambda,
             FUN = function(z) prod(dpois(y, lambda = z)))

df_pois <- tibble(lambda = lambda,
                  pr = pr)

df_pois %>% 
  ggplot(aes(x = lambda,
             y = pr)) +
  geom_line() +
  theme_bw()

df_pois %>% 
  arrange(desc(pr))


# back to glm -------------------------------------------------------------

df_count <- read_csv(here::here("data_raw/data_garden_count.csv"))

m_pois <- glm(count ~ nitrate,
              data = df_count,
              family = "poisson")

logLik(m_pois)
