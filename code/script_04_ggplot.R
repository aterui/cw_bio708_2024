
# setup -------------------------------------------------------------------

rm(list = ls())

## load library
library(tidyverse)

iris <- as_tibble(iris)

# scatter plot ------------------------------------------------------------

iris %>% 
  ggplot(aes(x = Sepal.Length,
             y = Sepal.Width)) +
  geom_point()

## change color by `Species`
iris %>% 
  ggplot(aes(x = Sepal.Length,
             y = Sepal.Width,
             color = Species)) +
  geom_point()

## change color uniformly
iris %>% 
  ggplot(aes(x = Sepal.Length,
             y = Sepal.Width)) +
  geom_point(color = "red")


# line --------------------------------------------------------------------

df0 <- tibble(x = rep(1:50, 3),
              y = x * 2)

df0 %>% 
  ggplot(aes(x = x,
             y = y)) +
  geom_line()


# histogram ---------------------------------------------------------------

iris %>% 
  ggplot(aes(x = Sepal.Length)) +
  geom_histogram()

## change bin width
iris %>% 
  ggplot(aes(x = Sepal.Length)) +
  geom_histogram(binwidth = 0.5)

## change how many bins
iris %>% 
  ggplot(aes(x = Sepal.Length)) +
  geom_histogram(bins = 50)

## change the edge color
iris %>% 
  ggplot(aes(x = Sepal.Length)) +
  geom_histogram(color = "red")

## change the bar color
iris %>% 
  ggplot(aes(x = Sepal.Length)) +
  geom_histogram(fill = "red",
                 color = "white")

# boxplot -----------------------------------------------------------------

iris %>% 
  ggplot(aes(x = Species,
             y = Sepal.Length)) +
  geom_boxplot()

## change the color of boxes by species
iris %>% 
  ggplot(aes(x = Species,
             y = Sepal.Length,
             color = Species)) +
  geom_boxplot()

## change the `fill` of boxes by species
iris %>% 
  ggplot(aes(x = Species,
             y = Sepal.Length,
             fill = Species)) +
  geom_boxplot()

## change the color transparency of boxes by species
iris %>% 
  ggplot(aes(x = Species,
             y = Sepal.Length,
             fill = Species)) +
  geom_boxplot(alpha = 0.2)


# exercise ----------------------------------------------------------------

## density plot
library(hrbrthemes)
library(viridis)

# Load dataset from github
data <- read.table("https://raw.githubusercontent.com/holtzy/data_to_viz/master/Example_dataset/1_OneNum.csv",
                   header=TRUE)

# Make the histogram
data %>%
  filter(price < 300) %>%
  ggplot(aes(x = price)) +
  geom_density(fill = "#69b3a2",
               color = "#e9ecef",
               alpha = 0.8) +
  ggtitle("Night price distribution of Airbnb appartements") +
  theme_ipsum()


## violin plot
# Load dataset from github
data <- read.table("https://raw.githubusercontent.com/zonination/perceptions/master/probly.csv",
                   header = TRUE,
                   sep = ",") %>% 
  as_tibble()

data <- data %>%
  gather(key = "text",
         value = "value") %>%
  mutate(text = gsub("\\.", " ", text)) %>%
  mutate(value = round(as.numeric(value), 0)) %>%
  filter(text %in% c("Almost Certainly",
                     "Very Good Chance",
                     "We Believe",
                     "Likely",
                     "About Even",
                     "Little Chance",
                     "Chances Are Slight",
                     "Almost No Chance"))

# Plot
data %>%
  mutate(text = fct_reorder(text, value)) %>%
  ggplot(aes(x = text,
             y = value,
             fill = text,
             color = text)) +
  geom_violin(width = 2.1,
              size = 0.2) +
  scale_fill_viridis(discrete = TRUE) +
  scale_color_viridis(discrete = TRUE) +
  theme_ipsum() +
  theme(legend.position="none") +
  coord_flip() +
  xlab("") +
  ylab("Assigned Probability (%)")
