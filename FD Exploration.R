library(dplyr)
library(readr)

# Read in SaberSim data
ss_batters <- read_csv("FanGraphs Leaderboard (6).csv") %>%  
  filter(!is.na(Game))

# Read in sample FanDuel player list
fd <- read_csv("FanDuel-MLB-2018-04-28-25410-players-list.csv")

# Teams with the most projected points (full roster)
ss_batters %>% 
  group_by(Team) %>% 
  summarise("xpts" = sum(FanDuel)) %>% 
  arrange(desc(xpts)) 

# Stackability ranking (top 4 by team)
stack_ranking <- ss_batters %>% 
  arrange(desc(FanDuel)) %>% 
  group_by(Team) %>% 
  slice(1:4) %>% 
  group_by(Team) %>% 
  summarise("xspts" = sum(FanDuel)) %>% 
  arrange(desc(xspts))%>% 
  mutate("stack_rank" = 1:n())
  
# View Top four batters by team
  ss_batters %>% 
    arrange(desc(FanDuel)) %>% 
    group_by(Team) %>% 
    slice(1:4) %>% 
    left_join(stack_ranking, by = "Team") %>% 
    arrange(stack_rank) %>% View("top four batters by team")
  

# Points per dollar
ss_pitchers %>% 
  left_join(fd,  by = c("Name" = "Nickname")) %>% 
  mutate("points per su" = Salary/FanDuel) %>% 
  select(Name, FanDuel, `points per su`) %>% 
  arrange(desc(`points per su`))
