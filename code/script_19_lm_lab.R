
# setup -------------------------------------------------------------------

rm(list = ls())
source(here::here("code/set_library.R"))

# question 1 --------------------------------------------------------------
## question 1: shapiro test

# develop iris model
m_iris <- lm(Petal.Length ~ Petal.Width + Species,
             data = iris)

summary(m_iris)

## Normality assumption should be tested with model residuals,
## not the response variable (Petal.Length in this case)!
eps <- resid(m_iris)
shapiro.test(eps)

# question 2 --------------------------------------------------------------
## extract intercept values for each species

## intercept and coefficient
b <- coef(m_iris)
a <- NULL

## intercept for setosa (the baseline species)
a[1] <- b[1]

## intercept for versicolor
a[2] <- b[1] + b[3]

## intercept for virginica
a[3] <- b[1] + b[4]

## check with plot
n_rep <- 100
df_pred <- tibble(Petal.Width = rep(seq(min(iris$Petal.Width),
                                        max(iris$Petal.Width),
                                        length = n_rep),
                                    n_distinct(iris$Species)),
                  Species = rep(unique(iris$Species),
                                each = n_rep))

y_pred <- predict(m_iris,
                  newdata = df_pred)

df_pred <- df_pred %>% 
  mutate(y_pred = y_pred)

(g_org <- iris %>% 
  ggplot(aes(x = Petal.Width,
             y = Petal.Length,
             color = Species)) +
  geom_point(alpha = 0.5) +
  geom_line(data = df_pred,
            aes(y = y_pred)) +
  geom_point(data = tibble(Petal.Width = 0,
                           Petal.Length = a,
                           Species = c("setosa",
                                       "versicolor",
                                       "virginica")),
             color = "black"))

# question 3 --------------------------------------------------------------
## alternative model

m_iris0 <- lm(Petal.Length ~ Petal.Width,
              data = iris)

df_new <- tibble(Petal.Width = with(iris,
                                    seq(min(Petal.Width),
                                        max(Petal.Width),
                                        length = 100)))

# - predict() requires predictor name(s) used in the model in `newdata`
# - otherwise, it will spit errors
y_pred <- predict(m_iris0,
                  newdata = df_new)

df_new <- df_new %>%
  mutate(y_pred = y_pred)

# ## alternative codes
# b <- coef(m_iris0)
# df_new <- df_new %>%
#   mutate(y_pred = b[1] + b[2] * Petal.Width)

g_org + 
  geom_line(data = df_new,
            aes(y = y_pred),
            color = grey(0, 0.5))