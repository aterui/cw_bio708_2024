
# setup -------------------------------------------------------------------

rm(list = ls())
source(here::here("code/set_library.R"))


# t-test equivalence with lm ----------------------------------------------

df_fl <- read_csv("data_raw/data_fish_length.csv")
print(df_fl)

v_mu <- df_fl %>% 
  group_by(lake) %>% 
  summarize(mu = mean(length)) %>% 
  pull(mu)
  
# average difference
v_mu[2] - v_mu[1]

m <- lm(length ~ lake,
        data = df_fl)

summary(m)

## look into details of t-test
lake_a <- df_fl %>% 
  filter(lake == "a") %>% 
  pull(length)

lake_b <- df_fl %>% 
  filter(lake == "b") %>% 
  pull(length)

t.test(x = lake_b, y = lake_a, var.equal = TRUE)

# anova equivalence with lm -----------------------------------------------

df_anova <- read_csv("data_raw/data_fish_length_anova.csv")
print(df_anova)

# group means
v_mu <- df_anova %>% 
  group_by(lake) %>% 
  summarize(mu = mean(length)) %>% 
  pull(mu)

print(c(v_mu[1], # mu_a: should be identical to intercept
        v_mu[2] - v_mu[1], # mu_b - mu_a: should be identical to the slope for lakeb
        v_mu[3] - v_mu[1])) # mu_c - mu_a: should be identical to the slope for lakec

lm(length ~ lake,
   data = df_anova) %>% 
  summary()

aov(length ~ lake,
    data = df_anova) %>% 
  summary()

# multiple types of predictors --------------------------------------------

# convert the data format to tibble
iris <- as_tibble(iris)
print(iris)

distinct(iris, Species)

m_iris <- lm(Petal.Length ~ Petal.Width + Species,
             data = iris)

summary(m_iris)

# create a data frame for prediction
# variable names must be identical to the original dataframe for analysis
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

iris %>% 
  ggplot(aes(x = Petal.Width,
             y = Petal.Length,
             color = Species)) +
  geom_point(alpha = 0.5) +
  geom_line(data = df_pred,
            aes(y = y_pred)) # redefine y values for lines; x and color are inherited from ggplot()

df_pred0 <- iris %>%
  group_by(Species) %>%
  reframe(Petal.Width = seq(min(Petal.Width),
                            max(Petal.Width),
                            length = 100))

y_pred0 <- predict(m_iris, 
                   newdata = df_pred0)

df_pred0 <- df_pred0 %>% 
  mutate(y_pred0 = y_pred0)

iris %>% 
  ggplot(aes(x = Petal.Width,
             y = Petal.Length,
             color = Species)) +
  geom_point(alpha = 0.5) +
  geom_line(data = df_pred0,
            aes(y = y_pred0)) # redefine y values for lines; x and color are inherited from ggplot()
