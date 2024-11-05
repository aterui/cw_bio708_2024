
# setup -------------------------------------------------------------------

rm(list = ls())
source(here::here("code/set_library.R"))


# count data --------------------------------------------------------------

df_count <- read_csv(here::here("data_raw/data_garden_count.csv"))
print(df_count)

## fit a normal model to count data
m_normal <- lm(count ~ nitrate,
               data = df_count)

summary(m_normal)

## visualize fit (normal model)
b <- coef(m_normal)

df_pred <- df_count %>% 
  reframe(nitrate = seq(min(nitrate),
                        max(nitrate),
                        length = 100),
          y = b[1] + b[2] * nitrate)

g_normal <- df_count %>% 
  ggplot(aes(x = nitrate,
             y = count)) +
  geom_point() +
  geom_line(data = df_pred,
            aes(y = y),
            linetype = "dashed")

## fit a poisson model to count data
m_pois <- glm(count ~ nitrate,
              data = df_count,
              family = "poisson")

summary(m_pois)

# parameter estimates and their SEs
theta <- coef(m_pois)
se <- sqrt(diag(vcov(m_pois)))
z_value <- theta / se

print(z_value)

## fit a poisson model
df_pred_pois <- df_count %>% 
  reframe(nitrate = seq(min(nitrate),
                        max(nitrate),
                        length = 100),
          y_pois = exp(theta[1] + theta[2] * nitrate)) # you need a tweak here

g_normal + # figure object from a Normal model
  geom_line(data = df_pred_pois,
            aes(y = y_pois),
            color = "salmon")


# offset term -------------------------------------------------------------
## this account for un-equal area of sampling

df_count_ue <- df_count %>% 
  mutate(area = rpois(nrow(.), 10),
         count_ue = count * area)

## plot for area vs. count
df_count_ue %>% 
  ggplot(aes(x = area,
             y = count_ue)) +
  geom_point()

## glm() with offset term
## - offset term is should be provided in log scale if the link function is log
## - log-link function is the default setup in Poisson
## the following code reads...
## y ~ Pois(lambda)
## log(lambda) = alpha + beta * x  + log(area)
m_pois_ue <- glm(count ~ nitrate + offset(log(area)),
                 data = df_count_ue,
                 family = "poisson")

summary(m_pois_ue)


# proportional data -------------------------------------------------------

## visualization of logit function
# x: produce 100 numbers from -100 to 100 (assume logit scale)
# y: convert with inverse-logit transformation (ordinary scale)
df_test <- tibble(logit_x = seq(-10, 10, length = 100),
                  x = exp(logit_x) / (1 + exp(logit_x)))

df_test %>% 
  ggplot(aes(x = logit_x,
             y = x)) +
  geom_point() +
  geom_line() +
  labs(y = "x",
       x = "logit(x)")

## mussel egg data
df_mussel <- read_csv(here::here("data_raw/data_mussel.csv"))
print(df_mussel)

df_mussel <- df_mussel %>% 
  mutate(p = n_fertilized / n_examined)

## plot
df_mussel %>% 
  ggplot(aes(x = density,
             y = p)) +
  geom_point() +
  labs(x = "Mussel density",
       y = "Prop. of eggs fertilized")

## glm with binomial error distribution
m_binom <- glm(cbind(n_fertilized, n_examined - n_fertilized) ~ density,
               data = df_mussel,
               family = "binomial")

# # make prediction
# df_pred <- tibble(density = seq(min(df_mussel$density),
#                                 max(df_mussel$density),
#                                 length = 100))

## make a data frame for prediction
df_pred <- df_mussel %>% 
  reframe(density = seq(min(density),
                        max(density),
                        length = 100))

## prediction
y <- predict(m_binom,
             newdata = df_pred) %>% 
  boot::inv.logit()

df_pred <- df_pred %>% 
  mutate(y_pred = y)

# ## manual conversion
# my.inv.logit <- function(x) exp(x) / (1 + exp(x))
# my_y <- predict(m_binom,
#              newdata = df_pred) %>% 
#   my.inv.logit()

## draw prediction
df_mussel %>% 
  ggplot(aes(x = density,
             y = p)) +
  geom_point() +
  labs(x = "Mussel density",
       y = "Prop. of eggs fertilized") +
  geom_line(data = df_pred,
            aes(y = y_pred))

# binomial distribution with variable number of trials --------------------

## create fake data with variable number of trials
df_mussel <- df_mussel %>% 
  mutate(n_examined = rpois(nrow(.), lambda = 40))

## this model code natually accounts for variation in n_examined
m_binom <- glm(cbind(n_fertilized, n_examined - n_fertilized) ~ density,
               data = df_mussel,
               family = "binomial")



