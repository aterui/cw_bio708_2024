
# setup -------------------------------------------------------------------

rm(list = ls())

## install tidyverse, if not done yet
## install.packages("tidyverse")

## load library
library(tidyverse)

iris <- as_tibble(iris)

# row manipulation --------------------------------------------------------

# single match "=="
df_ex <- filter(iris, Species == "virginica")

# multiple match "%in%"
df_ex <- filter(iris, Species %in% c("virginica", "versicolor"))

# except "!="
df_ex <- filter(iris, Species != "virginica")

# except multiple "!(x %in% c("a", "b"))
df_ex <- filter(iris, !(Species %in% c("virginica", "versicolor")))

# greater than ">"
df_ex <- filter(iris, Sepal.Length > 5)

# equal & greater than ">="
df_ex <- filter(iris, Sepal.Length >= 5)

# less than "<"
df_ex <- filter(iris, Sepal.Length < 5)

# equal & less than "<="
df_ex <- filter(iris, Sepal.Length <= 5)

# arrange by Sepal.Length: increasing order
df_ex <- arrange(iris, Sepal.Length)

# arrange by Sepal.Length: decreasing order
df_ex <- arrange(iris, desc(Sepal.Length))


# column manipulation -----------------------------------------------------

# select one column
df_ex <- select(iris, Sepal.Length)

# select multiple columns
df_ex <- select(iris, c(Sepal.Length, Sepal.Width))

# remove one column
df_ex <- select(iris, -Sepal.Length)

# remove multiple columns
df_ex <- select(iris, -c(Sepal.Length, Sepal.Width))

# select/remove multiple columns with a start rule
# starts_with("x")
df_ex <- select(iris, starts_with("Sepal"))
df_ex <- select(iris, -starts_with("Sepal"))

# select/remove multiple columns with an end rule
# ends_with("x")
df_ex <- select(iris, ends_with("Width"))
df_ex <- select(iris, -ends_with("Width"))

## add new columns
# add a new column
x <- 1:150
mutate(iris, id = x)


# piping ------------------------------------------------------------------

df_vir <- filter(iris, Species == "virginica")
df_vir_length <- select(df_vir, ends_with("Length"))

df_master <- iris %>% 
  filter(Species == "virginica") %>% 
  select(ends_with("Length"))


# reshape -----------------------------------------------------------------

df_wide <- iris %>% 
  mutate(id = rep(1:50, times = 3)) %>% 
  select(id, Sepal.Length, Species) %>% 
  pivot_wider(id_cols = id,
              values_from = "Sepal.Length",
              names_from = "Species")

print(df_wide)

df_long <- df_wide %>% 
  pivot_longer(cols = c("setosa",
                        "versicolor",
                        "virginica"),
               names_to = "Species",
               values_to = "Sepal.Length")

print(df_long)

# group operation ---------------------------------------------------------

df_g <- iris %>% 
  group_by(Species) %>% 
  summarize(mu_sl = mean(Sepal.Length),
            sigma_sl = sd(Sepal.Length))

print(df_g)

df_i <- iris %>% 
  group_by(Species) %>% 
  mutate(mu_sl = mean(Sepal.Length),
         sigma_sl = sd(Sepal.Length)) %>% 
  ungroup()

print(df_i)