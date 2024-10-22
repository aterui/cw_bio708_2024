
# setup -------------------------------------------------------------------

rm(list = ls())
source(here::here("code/set_library.R"))

# 6.4.1 Develop regression models
# R provides a built-in data set called iris.
# The iris data contain data points from three different species (Species column).
# Split this data set by species (create three separate data frames) and 
# perform regression for each species separately to analyze the relationship 
# between Sepal.Width (response variable) and Petal.Width (explanatory variable).

## model without explanatory variable
m_set <- lm(Sepal.Width ~ Petal.Width,
            data = iris %>% 
              filter(Species == "setosa"))

m_ver <- lm(Sepal.Width ~ Petal.Width,
            data = iris %>% 
              filter(Species == "versicolor"))

m_vir <- lm(Sepal.Width ~ Petal.Width,
            data = iris %>% 
              filter(Species == "virginica"))

## Avoid redundant codes
sp <- unique(iris$Species) # get unique vector of species names
n_sp <- n_distinct(iris$Species) # get the number of unique elements

## - for loop option
## -- you may want to create an empty list as a placeholder
list_m <- NULL
for (i in 1:n_sp) {
  ## for (i in 1:n_sp) loops i from 1 to n_sp
  list_m[[i]] <- lm(Sepal.Width ~ Petal.Width, 
                    data = iris %>% 
                      filter(Species == sp[i]))
}

## - lapply option
## -- in this case, you won't need to reate a placeholder
list_m <- lapply(1:n_sp, function(i) {
  ## function(i) loops i from 1 to n_sp
  lm(Sepal.Width ~ Petal.Width,
     data = iris %>% 
       filter(Species == sp[i]))
})


# 6.4.2 Multiple explanatory variables
# Regression analysis can involve multiple explanatory variables.
# To explore this, consider utilizing Petal.Length as an additional
# explanatory variable for each species.
# Then, investigate:
# (1) the variations in estimates of regression coefficients
# (2) the differences in the coefficients of determination compared to the model with only a single explanatory variable.

m1_set <- lm(Sepal.Width ~ Petal.Width + Petal.Length,
             data = iris %>% 
               filter(Species == "setosa"))

m1_ver <- lm(Sepal.Width ~ Petal.Width + Petal.Length,
             data = iris %>% 
               filter(Species == "versicolor"))

m1_vir <- lm(Sepal.Width ~ Petal.Width + Petal.Length,
             data = iris %>% 
               filter(Species == "virginica"))

## interaction term example
## one way to write a model with interaction x1 + x2 + x1:x2 
m_int0 <- lm(Sepal.Width ~ Petal.Width + Petal.Length + Petal.Width:Petal.Length,
             data = iris %>% 
               filter(Species == "virginica"))

## another way to write a model with interaction x1 * x2 
## x1 * x2 is a short version of x1 + x2 + x1:x2
m_int1 <- lm(Sepal.Width ~ Petal.Width * Petal.Length,
             data = iris %>% 
               filter(Species == "virginica"))

# Extra: Calculate the coefficient of variation for models developed in 6.4.2
# Perform the calculation manually, then confirm your calculation match the reported value in lm()
df_set <- iris %>% 
  filter(Species == "setosa")

ss_null <- with(df_set, (Sepal.Width - mean(Sepal.Width)))^2 %>% 
  sum()

ss <- resid(lm(Sepal.Width ~ Petal.Width, df_set))^2 %>% 
  sum()

(r_sq <- 1 - (ss / ss_null))
