# Greenbuildings

library(tidyverse)
library(ggthemes)
library(tictoc)
library(kableExtra)
library(modelsummary)
library(skimr)
library(estimatr)
library(janitor)
library(tidymodels)
library(patchwork)
library(ggmap)
library(maps)
library(mapdata)

# funcs
read_data <- function(df) {
  #' read data from git url
  #' INPUT: data set name
  #' OUTPUT: dataframe
  full_path <- paste("https://raw.githubusercontent.com/jgscott/ECO395M/master/data/", 
                     df, sep = "")
  df <- read_csv(full_path)
  return(df)
}

green_build <- 
  read_data("greenbuildings.csv") %>%
  # to snake case
  janitor::clean_names() %>%
  # Revenue per sq ft
  mutate(rev_sq_ft = rent * leasing_rate)

skim(green_build)

# distro rev_sq_ft
green_build %>%
  ggplot() + 
  geom_histogram(aes(x = rev_sq_ft), 
                 bins = 50,
                 fill = "dodgerblue", color = "black") +
  geom_vline(aes(xintercept = mean(rev_sq_ft)), 
             color = "tomato", linetype = 2) +
  # few outliers
  xlim(0, 12000) +
  labs(x = "Revenue by square foot (USD)", y = "Count") + 
  theme_clean() +
  facet_wrap(. ~ green_rating,
             labeller = labeller(green_rating = c("0" = "Not rated", "1" = "Green Rated")))

# tally by group (green rating)
green_build %>%
  mutate(green_rating = if_else(green_rating == 0, "No rating", "Green rating")) %>%
  group_by(green_rating) %>%
  tally() %>%
  rename(`Green Rating` = green_rating,
         Count = n) %>% 
  kable("pipe")

# tally by green rating
# green_build %>%
#   #filter(green_rating == 1) %>%
#   select(energystar, leed, class_a, class_b) %>% 
#   pivot_longer(everything(), names_to = "Rating", values_to = "is_rated") %>% 
#   mutate(Rating = factor(Rating)) %>%
#   group_by(Rating) %>% 
#   tally() %>%
#   rename(Count = n) %>% 
#   kable("pipe")

green_build %>%
 # mutate(green_rating = if_else(green_rating == 0, "No rating", "Green rating")) %>%
  ggplot() + 
  geom_histogram(aes(x = rev_sq_ft, fill = factor(class_a)), 
                 bins = 50,
                 color = "black") +
  geom_vline(aes(xintercept = mean(rev_sq_ft)), 
             color = "tomato", linetype = 2) +
  # few outliers
  xlim(0, 12000) +
  labs(x = "Revenue by square foot (USD)", y = "Count") + 
  theme_clean() 






