
# setup -------------------------------------------------------------------

rm(list = ls())
source(here::here("code/set_library.R"))


# question 1: glm modeling with fish data ---------------------------------

df_fish <- read_csv(here::here("data_raw/data_vpart.csv"))

## `n_sp` is a discrete variable with no upper limit
## possible choice: Poisson or Negative-Binomial

## if variance ~ mean, Poisson
## if variance >> mean, negative binomial
## check mean-variance relationship
with(df_fish, mean(n_sp)) # mean = 5.33
with(df_fish, var(n_sp)) # variance = 2.86

m_pois <- glm(n_sp ~ distance + cat_area + hull_area,
              data = df_fish,
              family = "poisson")

summary(m_pois)

## prediction plot
## - coefficients from the model
b <- coef(m_pois)

## - prediction for distance
## - other predictors are fixed at their means
df_pred <- df_fish %>% 
  reframe(distance = seq(min(distance),
                         max(distance),
                         length = 100),
          cat_area = mean(cat_area),
          hull_area = mean(hull_area)) %>% 
  mutate(log_y_pred = predict(m_pois,
                              newdata = .),
         y_pred = exp(log_y_pred))

## plot
(g_fish <- df_fish %>% 
  ggplot(aes(x = distance,
             y = n_sp)) +
  geom_point() +
  geom_line(data = df_pred,
            aes(y = y_pred)) +
  labs(y = "Fish species richness",
       x = "Distance to the sea") +
  theme_bw())


# effect size -------------------------------------------------------------

m_std <- glm(n_sp ~ scale(distance) + scale(cat_area) + scale(hull_area),
             data = df_fish,
             family = "poisson")
