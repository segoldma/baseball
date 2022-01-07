## Draft help

library(readr)
library(dplyr)

# Read in rankings
rankings <- read_csv("FantasyPros_Fantasy_Baseball_Rankings_ALL.csv") 

# Read in draft progress
draft_progress <- read_csv("Draft 44075 2019-03-08.csv",col_names = c("a","b","cee","d","e","f","g")) %>% 
  mutate("Player" = paste(cee,d))
  

# Show remaining guys
rankings %>% 
  anti_join(draft_progress, by = "Player") %>% 
  View()
