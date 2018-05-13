### mlbgameday: Tools to Gather Data from Major League Baseball Advanced Media (MLBAM)


#documentation: https://cran.r-project.org/web/packages/mlbgameday/mlbgameday.pdf

# Install and load package from CRAN
install.packages("mlbgameday")
library(mlbgameday)
library(dplyr)
library(lubridate)
library(skimr)
library(tidyr)

# View vignette
?mlbgameday


# payload <- get_payload(start = "2018-03-29", end = today()-1)

# Skim the payload
skim(df$atbat)
skim(df$action)
skim(df$pitch)
skim(df$runner)
skim(df$po)


atbat <- payload$atbat
action <- payload$action
pitch <- payload$pitch
runner <- payload$runner
pickoff <- payload$po


# Convert date to date
atbat$date <- as.Date(atbat$date, "%Y-%m-%d")

# Get player ids
player_ids <- read_csv("http://crunchtimebaseball.com/master.csv") %>% 
  select(mlb_id,mlb_name,mlb_team,mlb_pos)


# Create player-game hits table
player_game_hits <- atbat %>% 
  filter(event %in% c("Single", "Double", "Triple", "Home Run", "Walk", "Intent Walk", "Hit By Pitch")) %>% 
  left_join(player_ids, by = c("batter" = "mlb_id")) %>% 
  group_by(batter, mlb_name, gameday_link, event) %>% 
  summarise("count" = n()) %>% 
  spread(event,count) 

# Create player-game RBI table
player_game_rbi <- atbat %>% 
  filter(score == "T") %>% 
  left_join(player_ids, by = c("batter" = "mlb_id")) %>%
  mutate("RBI" = ifelse(event == "Home Run", 1 + str_count(atbat_des, "scores"), 
    str_count(atbat_des, "scores"))) %>%  
  group_by(batter, mlb_name, gameday_link) %>% 
  summarise("RBI" = sum(RBI)) %>% 
  ungroup()

# Calculate player-game Runs table
player_game_runs <- runner %>% 
  filter(score == "T") %>% 
  left_join(player_ids, by = c("id" = "mlb_id")) %>% 
  group_by(id, mlb_name,gameday_link, score) %>% 
  summarise("Runs" = n()) %>% 
  select(-score) %>% 
  ungroup()

# Calculate player-game Steals table
player_game_steals <- runner %>% 
  left_join(player_ids, by = c("id" = "mlb_id")) %>% 
  mutate("sb"=str_count(event, "Stolen Base")) %>% 
  group_by(id, mlb_name, gameday_link) %>% 
  summarise("SB" = sum(sb)) %>% 
  filter(SB>0) %>% arrange(desc(SB))
  
# Compile player-game fanduel stats
player_game <- atbat %>% 
  select(batter, date, gameday_link) %>% 
  unique() %>% 
  left_join(player_game_hits, by = c("batter","gameday_link")) %>% 
  left_join(player_game_rbi, by = c("batter", "mlb_name", "gameday_link")) %>% 
  left_join(player_game_runs, by = c("batter" = "id", "mlb_name", "gameday_link")) %>% 
  left_join(player_game_steals, by = c("batter" = "id", "mlb_name", "gameday_link")) %>% 
  mutate("Single" = ifelse(is.na(Single), 0, Single),
         "Double" = ifelse(is.na(Double), 0, Double),
         "Triple" = ifelse(is.na(Triple), 0, Triple),
         "Home Run" = ifelse(is.na(`Home Run`), 0, `Home Run`),
         "Walk" = ifelse(is.na(Walk), 0, Walk),
         "Intent Walk" = ifelse(is.na(`Intent Walk`), 0, `Intent Walk`),
         "Hit By Pitch" = ifelse(is.na(`Hit By Pitch`), 0, `Hit By Pitch`),
         "RBI" = ifelse(is.na(RBI), 0, RBI),
         "Runs" = ifelse(is.na(Runs), 0, Runs),
         "SB" = ifelse(is.na(SB), 0, SB)) %>%  
  mutate("Pts" = (`Single`*3)+(`Double`*6) + (`Triple`*9) + (`Walk`*3) + (`Hit By Pitch`*3) + (`Intent Walk`*3) + (`Home Run`*12) + (`Runs`*3.2) + (`RBI`*3.5) + (`SB`*6)) %>% 
  left_join(player_ids, by = c("batter"="mlb_id","mlb_name"))


# Graph Pts per game
player_game %>% 
  filter(!is.na(mlb_team)) %>% 
  ggplot(aes(x=date, y=Pts, color = mlb_team)) +
  geom_line()+
  facet_wrap(~mlb_team)+
  theme(legend.position="none")+
  ggtitle("FanDuel Daily Pts (Batting Only)") +
  ggsave("FanDuel Daily Pts Per Team.png")



# Scoring Analysis --------------------------------------------------------

### FanDuel Scoring Analysis

library(dplyr)
library(rvest)
library(lubridate)
library(magrittr)
library(mlbgameday)

# Load Batting Data from FG -----------------------------------------------

## Get standard batting stats from fangraphs
url <- read_html("https://www.fangraphs.com/leaders.aspx?pos=all&stats=bat&lg=all&qual=y&type=0&season=2018&month=0&season1=2018&ind=0&team=0&rost=0&age=0&filter=&players=0&page=1_2000") 

# Load batting dashboard
batting <- html_nodes(url,".rgMasterTable") %>% 
  html_table() %>% 
  as.data.frame() 

# Rename columns and remove the header rows
colnames(batting) <- as.character(unlist(batting[2,]))
batting <- batting %>% 
  slice(c(-1,-2,-3)) %>% 
  select(-`#`) #remove header rows and number column

# Coerce stats from characters to numeric
dbl_cols <- colnames(batting)[3:22]
batting %<>% mutate_at(dbl_cols, funs(as.numeric(.)))
rm(dbl_cols)

# Calculate FanDuel Points
batting <- batting %>% 
  mutate("FPS" = (`1B`*3)+(`2B`*6) + (`3B`*9) + (`BB`*3) + (`HBP`*3) + (`HR`*12) + (`R`*3.2) + (`RBI`*3.5) + (`SB`*6))



batting %>% 
  ggplot(aes(BB, y=FPS))+
  geom_point()

# Top FPS by Team

batting %>% 
  ggplot(aes(x=FPS, fill = Team)) +
  geom_density(alpha=0.3) +
  facet_wrap(~Team) +
  ggtitle("FPTS Density by Team (Qualified Players Only)") +
  ggsave(paste0("FP Density by Team ",today(),".png"))


# Hit Density by Team
batting %>% 
  ggplot(aes(x=H, fill = Team)) +
  geom_density(alpha=0.3) +
  facet_wrap(~Team) 

