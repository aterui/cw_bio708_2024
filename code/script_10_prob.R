# setup -------------------------------------------------------------------

rm(list = ls())
source(here::here("code/set_library.R"))

# load csv data on R
df_h0 <- read_csv(here::here("data_raw/data_plant_height.csv"))


# class work --------------------------------------------------------------

## histogram
df_h0 %>% 
  ggplot(aes(x = height)) + 
  geom_histogram() +
  geom_vline(aes(xintercept = mean(height)))

## x vector
x <- seq(min(df_h0$height),
         max(df_h0$height),
         length = 100)

mu <- mean(df_h0$height)
sigma <- sd(df_h0$height)
pd <- dnorm(x, mean = mu, sd = sigma)

tibble(y = pd, x = x) %>% 
  ggplot(aes(x = x,
             y = y)) +
  geom_line() +
  labs(y = "Probability density")

## convert probability density to probability
p10 <- pnorm(10, mean = mu, sd = sigma)
p5 <- pnorm(5, mean = mu, sd = sigma)

## create histogram with estimates
x_min <- floor(min(df_h0$height))
x_max <- ceiling(max(df_h0$height))
bin <- seq(x_min, x_max, by = 1)

p <- NULL
for (i in 1:(length(bin) - 1)) {
  p[i] <- pnorm(bin[i + 1], mean = mu, sd = sigma) -
    pnorm(bin[i], mean = mu, sd = sigma)
}

df_prob <- tibble(p = p,
                  bin = bin[-length(bin)] + 0.5) %>% 
  mutate(freq = p * nrow(df_h0))

df_h0 %>% 
  ggplot(aes(x = height)) +
  geom_histogram(binwidth = 1,
                 center = 0.5) +
  geom_point(data = df_prob,
             aes(y = freq,
                 x = bin),
             color = "salmon") +
  geom_line(data = df_prob,
            aes(y = freq,
                x = bin),
            color = "salmon")

# count data --------------------------------------------------------------

df_count <- read_csv(here::here("data_raw/data_garden_count.csv"))

df_count %>% 
  ggplot(aes(x = count)) +
  geom_histogram(binwidth = 0.5,
                 center = 0)

# vector of x values
# create a vector of 0 to 10 with an interval one
# must be integer of > 0
x <- seq(0, 10, by = 1)
lambda_hat <- mean(df_count$count)
pm <- dpois(x, lambda = lambda_hat)

tibble(y = pm, 
       x = x) %>% 
  ggplot(aes(x = x,
             y = y)) +
  geom_line(linetype = "dashed") +
  geom_point()

df_prob <- tibble(y = pm, 
                  x = x) %>% 
  mutate(freq = pm * nrow(df_count))

df_count %>% 
  ggplot(aes(x = count)) +
  geom_histogram(binwidth = 0.5,
                 center = 0) +
  geom_line(data = df_prob,
            aes(x = x,
                y = freq),
            linetype = "dashed") +
  geom_point(data = df_prob,
             aes(x = x,
                 y = freq)) +
  scale_x_continuous(breaks = x)


