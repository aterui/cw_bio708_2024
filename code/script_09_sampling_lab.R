
# setup -------------------------------------------------------------------

rm(list = ls())
source(here::here("code/set_library.R"))

df_h0 <- read_csv(here::here("data_raw/data_plant_height.csv"))

# exercise 1 --------------------------------------------------------------
## Obtain 100 sub-datasets with 50 and 100 measures each, and draw histograms
## of sample means and unbiased variances

## fifty samples
mu <- sigma <- NULL
for (i in 1:100) {
  df_i <- df_h0 %>% 
    sample_n(50)
  
  mu[i] <- mean(df_i$height)
  sigma[i] <- var(df_i$height)
}

df50 <- tibble(mu = mu,
               sigma = sigma,
               n = 50)

## 100 samples
mu <- sigma <- NULL
for (i in 1:100) {
  df_i <- df_h0 %>% 
    sample_n(100)
  
  mu[i] <- mean(df_i$height)
  sigma[i] <- var(df_i$height)
}

df100 <- tibble(mu = mu,
                sigma = sigma,
                n = 100)

df_m <- bind_rows(df50, df100)

## visual

## first approach: patchwork
g_mu50 <- df50 %>%
  ggplot(aes(x = mu)) +
  geom_histogram()

g_mu100 <- df100 %>%
  ggplot(aes(x = mu)) +
  geom_histogram()

## visual
g_var50 <- df50 %>%
  ggplot(aes(x = sigma)) +
  geom_histogram()

g_var100 <- df100 %>%
  ggplot(aes(x = sigma)) +
  geom_histogram()

g_mu50 / g_mu100 / g_var50 / g_var100

## 2nd approach - combine two dataframes into one
## - plot for mu and sigma, separately
df_m %>% 
  ggplot(aes(x = mu,
             color = factor(n))) +
  geom_density()

df_m %>% 
  ggplot(aes(x = sigma,
             color = factor(n))) +
  geom_density()

## - plot mu and sigma together using facet_wrap
g_r <- df_m %>% 
  pivot_longer(cols = c(mu, sigma),
               names_to = "parm", # parm stands for parameter 
               values_to = "value") %>% 
  ggplot(aes(x = value,
             color = factor(n))) +
  geom_density() + 
  facet_wrap(facets =~ parm,
             scales = "free")

## - challenging example
## - combination of lapply, function, for loop
df_m <- lapply(X = c(50, 100),
               function(z) {
                 
                 mu <- sigma <- NULL
                 
                 for(i in 1:100) {
                   df_i <- df_h0 %>%
                     sample_n(z)
                   
                   mu[i] <- mean(df_i$height)
                   sigma[i] <- var(df_i$height)
                 }
                 
                 cout <- tibble(n = z,
                                mu = mu,
                                sigma = sigma)
                 
                 return(cout)
               }) %>% 
  bind_rows()


# exercise 2 --------------------------------------------------------------
## Suppose the investigator was unable to find plants less than 10 cm in height
## – the following code excludes those less than 10 cm in height:
## Repeat step 1 with df_h10 instead of df_h0 and compare the results.
## only 50 samples

## non-random sampling
df_m_nr <- lapply(X = c(50, 100),
               function(z) {
                 
                 mu <- sigma <- NULL
                 
                 for(i in 1:100) {
                   df_i <- df_h0 %>%
                     filter(height >= 10) %>% 
                     sample_n(z)
                   
                   mu[i] <- mean(df_i$height)
                   sigma[i] <- var(df_i$height)
                 }
                 
                 cout <- tibble(n = z,
                                mu = mu,
                                sigma = sigma)
                 
                 return(cout)
               }) %>% 
  bind_rows()

## plot
g_nr <- df_m_nr %>% 
  pivot_longer(cols = c(mu, sigma),
               names_to = "parm", # parm stands for parameter 
               values_to = "value") %>% 
  ggplot(aes(x = value,
             color = factor(n))) +
  geom_density() + 
  facet_wrap(facets =~ parm,
             scales = "free",
             labeller = label_parsed) + # label_parsed `parse` input text
  labs(x = "Plant height (cm)",
       y = "Density",
       color = "Sample size") +
  theme_bw() +
  theme(strip.background = element_blank())

## compare random vs non-random
g_r / g_nr
