
# setup -------------------------------------------------------------------

rm(list = ls())
source(here::here("code/set_library.R"))


# aov trial ---------------------------------------------------------------

# read data
df_anova <- read_csv(here::here("data_raw/data_fish_length_anova.csv"))
distinct(df_anova, lake)

# first argument is formula
# second argument is data frame for reference
# do not forget specify data = XXX! aov() refer to columns in the data frame
m <- aov(formula = length ~ lake,
         data = df_anova)

summary(m)


# partition variability ---------------------------------------------------

# visual
# geom_violin() - function for violin plots
# geom_jitter() - jittered points
df_anova %>% 
  ggplot(aes(x = lake,
             y = length)) +
  geom_violin(draw_quantiles = 0.5, # draw median horizontal line
              alpha = 0.2) + # transparency
  geom_jitter(alpha = 0.2) # transparency

## between group-variability
# estimate overall mean
mu <- mean(df_anova$length)

# estimate group means and sample size each
df_g <- df_anova %>% 
  group_by(lake) %>% 
  summarize(mu_g = mean(length), # mean for each group
            dev_g = (mu_g - mu)^2, # squared deviation for each group
            n = n()) # sample size for each group

df_g <- df_g %>% 
  mutate(ss = dev_g * n)

s_b <- sum(df_g$ss)

## within-group variability
df_i <- df_anova %>% 
  group_by(lake) %>% 
  mutate(mu_g = mean(length)) %>% # use mutate() to retain individual rows
  ungroup() %>% 
  mutate(dev_i = (length - mu_g)^2) # deviation from group mean for each fish

df_i_ss <- df_i %>% 
  group_by(lake) %>% 
  summarize(ss = sum(dev_i))

s_w <- sum(df_i_ss$ss)


# calculate between within variances --------------------------------------

n_g <- n_distinct(df_anova$lake)
var_b <- s_b / (n_g - 1)
var_w <- s_w / (nrow(df_anova) - n_distinct(df_anova$lake))

f_value <- var_b / var_w

## f-distribution
x <- seq(0, 10, by = 0.1)
y <- df(x = x, df1 = n_g - 1, df2 = nrow(df_anova) - n_g)

tibble(x = x, y = y) %>% 
  ggplot(aes(x = x,
             y = y)) + 
  geom_line() + # F distribution
  geom_vline(xintercept = f_value,
             color = "salmon") # observed F-statistic

# pf() estimate the probability of less than q
# Pr(F0 > F) is 1 - Pr(F0 < F)
p_value <- 1 - pf(q = f_value, df1 = n_g - 1, df2 = nrow(df_anova) - n_g)
print(p_value)

