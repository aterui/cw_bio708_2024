
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


