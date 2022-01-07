## Nats Statcast
library(dplyr)
library(readr)
library(baseballr)
library(stringr)

player_ids <- read_csv("mlb_player_ids.csv") %>% 
  select(starts_with("mlb")) %>% 
  filter(mlb_team == "WSH",
         !mlb_pos == "P") 

# Write a function to get season-to-date player data from the 2019 MLB season
get_nats_data <- function(x){
x <- as.character(x)

df <- scrape_statcast_savant(start_date = as.Date("2019-04-01"), 
                         end_date = Sys.Date() - 1, 
                         player_type = "batter",
                         playerid = x)
  


 assign(x = stringr::str_c("payload_",x), value = df, envir = .GlobalEnv)
 
 
}


purrr::map(player_ids$mlb_id, get_nats_data)

# Identify payload dataframes into a list
payloads <- mget(ls(pattern = "^payload_.*"))

# Stack data
nats_batters <- bind_rows(mutate_all(payloads,as.character))

stringr::str_detect(ls(),"payload")


nats %>% 
  ggplot(aes(x=release_speed,y=hit_distance_sc,color = pitch_name))+
  geom_point()+
  geom_abline()
