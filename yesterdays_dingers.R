# Get Dingers

library(mlbgameday)
library(dplyr)
library(lubridate)
library(readr)

# Get payload from yesterday's games
df <- get_payload(start = today()-1, end = today()-1)

# Suset homerun data
dingers <- df$atbat %>% 
  filter(event == "Home Run") %>% 
  select(batter_name, pitcher_name, b, s, o, inning, date, event)

write_csv(dingers, paste0((today-1)))
          
          