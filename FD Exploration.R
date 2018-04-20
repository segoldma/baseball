library(dplyr)
library(readr)

# Read in SaberSim data
ss_batters <- read_csv("ss batters.csv")
ss_pitchers <- read.csv("ss pitchers.csv", fileEncoding = "UTF-8-BOM")

# Read in sample FanDuel player list
fd <- read_csv("FanDuel-MLB-2018-04-20-25194-players-list.csv")

# Try to match on names
fd_sample <- fd %>% 
  select(Nickname, FPPG)

ss_batters %>% 
  select(Name, Team) %>% 
  left_join(fd_sample, by = c("Name" = "Nickname")) %>% 
  group_by(is.na(FPPG)) %>% 
  tally()

ss_pitchers %>% 
  select(Name, Team) %>% 
  left_join(fd_sample, by = c("Name" = "Nickname")) %>% 
  group_by(is.na(FPPG)) %>% 
  tally()

# Points per dollar
ss_pitchers %>% 
  left_join(fd,  by = c("Name" = "Nickname")) %>% 
  mutate("points per su" = Salary/FanDuel) %>% 
  select(Name, FanDuel, `points per su`) %>% 
  arrange(desc(`points per su`))
