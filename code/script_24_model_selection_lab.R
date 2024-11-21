
# setup -------------------------------------------------------------------

rm(list = ls())
source(here::here("code/set_library.R"))


# data cleaning -----------------------------------------------------------

library(palmerpenguins)

## basic approach
df_pen0 <- as_tibble(penguins_raw)
cnm <- colnames(df_pen0)
cnm_clean <- str_to_lower(cnm) %>% 
  str_replace_all("\\s", "_") %>% 
  str_replace_all("_\\(mm\\)", "") %>% # alternative: str_remove_all("text") 
  str_replace_all("_\\(g\\)", "") %>% 
  str_replace_all("_\\(o/oo\\)", "") 

colnames(df_pen0) <- cnm_clean

df_pen0 <- df_pen0 %>% 
  mutate(success = ifelse(clutch_completion == "Yes",
                          yes = 1,
                          no = 0),
         species = case_when(species == "Adelie Penguin (Pygoscelis adeliae)" ~ "adelie",
                             species == "Gentoo penguin (Pygoscelis papua)" ~ "gentoo",
                             species == "Chinstrap penguin (Pygoscelis antarctica)" ~ "chinstrap")) %>% 
  drop_na(culmen_length,
          culmen_depth,
          flipper_length,
          body_mass)

## advanced approach
## - use rename_with() function
## - in regular expression (REGEX), "." means ANY TEXT
## - {1,2} means text string with length 1 to 2
## - combined, ".{1,}" means any text more than 1 test string
## - "\\(.{1,}\\)" catches "(mm)", "(g)", "(o/oo)" in this case
df_pen <- as_tibble(penguins_raw) %>%
  rename_with(.fn = function(x) {
    x %>%
      str_replace_all(pattern = "\\s",
                      replacement = "_") %>%
      str_remove("_\\(.{1,}\\)") %>%
      str_to_lower()
  }) %>%
  mutate(success = ifelse(clutch_completion == "Yes",
                          yes = 1,
                          no = 0),
         species = case_when(species == "Adelie Penguin (Pygoscelis adeliae)" ~ "adelie",
                             species == "Gentoo penguin (Pygoscelis papua)" ~ "gentoo",
                             species == "Chinstrap penguin (Pygoscelis antarctica)" ~ "chinstrap"))


# analysis ----------------------------------------------------------------

# what probability distribution for 0/1 binary data?
m <- glm(success ~ species + 
           culmen_length + 
           culmen_depth + 
           flipper_length +
           body_mass,
         family = "binomial",
         data = df_pen0)

library(MuMIn)
options(na.action = "na.fail")
m_set <- dredge(m, rank = "AIC")
subset(m_set, delta < 4)






