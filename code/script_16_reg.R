
# setup -------------------------------------------------------------------

rm(list = ls())
source(here::here("code/set_library.R"))


# lm trial ----------------------------------------------------------------

df_algae <- read_csv(here::here("data_raw/data_algae.csv"))

# comment out `install.packages()` once installed; don't do more than once
# install.packages("skimr")
skimr::skim(df_algae)

# scatter plot
df_algae %>% 
  ggplot(aes(x = conductivity,
             y = biomass)) +
  geom_point() +
  theme_bw()

# lm() takes a formula as the first argument
# don't forget to supply your data
m <- lm(biomass ~ conductivity,
        data = df_algae)

summary(m)

# get estimates
alpha <- coef(m)[1]
beta <- coef(m)[2]

df_algae %>% 
  ggplot(aes(x = conductivity,
             y = biomass)) +
  geom_point() +
  geom_abline(intercept = alpha,
              slope = beta)

# get t-values
theta <- coef(m)
se <- sqrt(diag(vcov(m)))
t_value <- theta / se

# for intercept
# (1 - pt(t_value[1], df = 48)) calculates pr(t > t_value[1])
# pt(-t_value[1], df = 48) calculates pr(t < -t_value[1])
p_alpha <- (1 - pt(t_value[1], df = 48)) + 
  pt(-t_value[1], df = 48)

# for slope
p_beta <- (1 - pt(t_value[2], df = 48)) + 
  pt(-t_value[2], df = 48)

print(p_alpha)

# visualize errors
# get residuals
eps <- round(df_algae$biomass - (alpha + beta * df_algae$conductivity), 4)
eps0 <- round(resid(m), 4)
mean(eps == eps0)

ss <- sum(eps^2)

# add error column
df_algae <- df_algae %>% 
  mutate(eps = eps)

df_algae %>% 
  ggplot(aes(x = conductivity,
             y = biomass)) +
  geom_point() +
  geom_abline(intercept = alpha,
              slope = beta) +
  geom_segment(aes(x = conductivity,
                   xend = conductivity,
                   y = biomass,
                   yend = biomass - eps),
               linetype = "dashed")

# coefficient of determination
summary(m)

# residual variance
ss <- sum(resid(m)^2)

# null variance
v_y <- df_algae$biomass
ss_0 <- sum((v_y - mean(v_y))^2)

# coefficient of determination
r2 <- 1 - ss / ss_0
