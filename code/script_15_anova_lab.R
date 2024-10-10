
# setup -------------------------------------------------------------------

rm(list = ls())
source(here::here("code/set_library.R"))


# exercise 1 --------------------------------------------------------------
## draw violin plot as in Figure 5.1
df_plant <- PlantGrowth %>% 
  as_tibble()

df_plant %>% 
  ggplot(aes(x = group,
             y = weight)) +
  geom_jitter(alpha = 0.5,
              width = 0.1) +
  geom_violin(fill = NA,
              draw_quantiles = c(0.25, 0.5, 0.75))

## perform anova (`aov()` function) with PlantGrowth data
fit <- aov(formula = weight ~ group,
           data = df_plant)

summary(fit)

# exercise 2 --------------------------------------------------------------
## calculate F value manually using the PlantGrowth data

mu <- mean(df_plant$weight)

## between group
s_b <- df_plant %>% 
  group_by(group) %>% # group by treatment
  summarize(mu_g = mean(weight), # mean weight by treatment
            dev_g = (mu_g - mu)^2, # deviation of group means from the global mean
            n = n(), # number of samples per group
            ss = dev_g * n) %>% # dev_g times number of samples within each group
  pull(ss) %>% # pull ss from the data frame
  sum() # take sum

var_b <- s_b / (n_distinct(df_plant$group) - 1)

## within group
s_w <- df_plant %>% 
  group_by(group) %>% 
  mutate(mu_g = mean(weight)) %>%
  ungroup() %>% 
  mutate(dev_i = (weight - mu_g)^2) %>% 
  group_by(group) %>% 
  summarize(ss = sum(dev_i)) %>% 
  pull(ss) %>% 
  sum()

var_w <- s_w / (nrow(df_plant) - n_distinct(df_plant$group))

f_value <- var_b / var_w

## create a function to calculate F value
## at least, something that works for the PlantGrowth data frame

### simple example of function
timestwo <- function(x) {
  2 * x
}

timestwo(x = 2) # return 4

fvalue <- function(data) {
  s_b <- data %>% 
    group_by(group) %>% # group by treatment
    summarize(mu_g = mean(weight), # mean weight by treatment
              dev_g = (mu_g - mu)^2, # deviation of group means from the global mean
              n = n(), # number of samples per group
              ss = dev_g * n) %>% # dev_g times number of samples within each group
    pull(ss) %>% # pull ss from the data frame
    sum() # take sum
  
  ### add lines here for var_b s_w var_w, and f_value...
  var_b <- s_b / (n_distinct(data$group) - 1)
  
  ## within group
  s_w <- data %>% 
    group_by(group) %>% 
    mutate(mu_g = mean(weight)) %>%
    ungroup() %>% 
    mutate(dev_i = (weight - mu_g)^2) %>% 
    group_by(group) %>% 
    summarize(ss = sum(dev_i)) %>% 
    pull(ss) %>% 
    sum()
  
  var_w <- s_w / (nrow(data) - n_distinct(data$group))
  
  f_value <- var_b / var_w
  
  return(f_value)
}

fvalue(data = df_plant)
