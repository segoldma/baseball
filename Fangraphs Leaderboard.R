library(rvest)
library(dplyr)
library(tibble)
library(stringr)
library(magrittr)
library(readr)
library(ggplot2)

# Team Mappings -----------------------------------------------------------

team_mappings <- data.frame(Tm = as.character(
  c(
    "ARI",  "ATL",  "BAL",  "BOS",
    "CHC",  "CIN",  "CLE",  "COL",
    "CHW",  "DET",  "HOU",  "KCR",
    "LAA",  "LAD",  "MIA",  "MIL",
    "MIN",  "NYM",  "NYY",  "OAK",
    "PHI",  "PIT",  "SDP",   "SEA",
    "SFG",   "STL",  "TBR",   "TEX",
    "TOR",  "WAS"
  )
),
Team =
  as.character(
    c(
      "Diamondbacks",  "Braves",
      "Orioles",  "Red Sox",
      "Cubs",  "Reds",
      "Indians",  "Rockies",
      "White Sox", "Tigers",
      "Astros",  "Royals",
      "Angels",   "Dodgers",
      "Marlins",  "Brewers",
      "Twins",   "Mets",
      "Yankees", "Athletics",
      "Phillies",  "Pirates",
      "Padres",   "Mariners",
      "Giants",  "Cardinals",
      "Rays",   "Rangers",
      "Jays", "Nationals"
    )
  )) %>%
  mutate("Tm" = as.character(Tm),
         "Team" = as.character(Team))




 

# Get stats from Fangraphs ------------------------------------------------

## Get standard batting stats from fangraphs
url <- read_html("https://www.fangraphs.com/leaders.aspx?pos=all&stats=bat&lg=all&qual=0&type=8&season=2018&month=0&season1=2018&ind=0&team=&rost=&age=&filter=&players=&page=1_2000") 

# Load batting dashboard
batting <- html_nodes(url,".rgMasterTable") %>% 
  html_table() %>% 
  as.data.frame() 

# Rename columns and remove the header rows
colnames(batting) <- as.character(unlist(batting[2,]))
batting <- batting %>% 
  slice(c(-1,-2,-3)) %>% #remove header rows
  select(Name,Team,G, HR,R,RBI,SB,BsR) #keep counting stats that aren't in the advanced stats table
  
# Get advanced batting stats from fangraphs
url_adv <- read_html("https://www.fangraphs.com/leaders.aspx?pos=all&stats=bat&lg=all&qual=0&type=1&season=2018&month=0&season1=2018&ind=0&team=0&rost=0&age=0&filter=&players=0&page=1_2000")

adv_batting_fangraphs <- html_nodes(url_adv,".rgMasterTable") %>% 
  html_table() %>% 
  as.data.frame() 

colnames(adv_batting_fangraphs) <- as.character(unlist(adv_batting_fangraphs[2,]))
adv_batting_fangraphs <- adv_batting_fangraphs %>% 
  slice(c(-1,-2,-3)) %>% #remove header rows
  select(-1) #remove number column

# Combine both tables
batting_stats <- batting %>% 
  left_join(adv_batting_fangraphs, by = c("Name","Team"))

# remove percentage signs from the K-rate and walk-rate stats
batting_stats$`BB%` <- str_replace(batting_stats$`BB%`, pattern = " %", replacement = "")
batting_stats$`K%` <- str_replace(batting_stats$`K%`,pattern = " %",replacement = "")

# Coerce stats from characters to numeric
dbl_cols <- colnames(batting_stats)[3:26]
batting_stats %<>% mutate_at(dbl_cols, funs(as.numeric(.)))


# Pitch Type and Pitch Value ----------------------------------------------

# Load pitch type data from fangraphs (Pitchers)
url_ptype <- read_html("https://www.fangraphs.com/leaders.aspx?pos=all&stats=pit&lg=all&qual=y&type=4&season=2018&month=0&season1=2018&ind=0&team=0&rost=0&age=0&filter=&players=0&page=1_150")

pitch_type_pitchers <- html_nodes(url_ptype, ".rgMasterTable") %>% 
  html_table() %>% 
  as.data.frame()

colnames(pitch_type_pitchers) <- as.character(unlist(pitch_type_pitchers[2,]))
pitch_type_pitchers <- pitch_type_pitchers %>% 
  slice(c(-1,-2,-3)) %>% 
  select(-1)

# remove percentage signs from pitch-type percentage stats
pitch_type_pitchers$`FB%` <- str_replace(pitch_type_pitchers$`FB%`, pattern = " %", replacement = "")
pitch_type_pitchers$`SL%` <- str_replace(pitch_type_pitchers$`SL%`, pattern = " %", replacement = "")
pitch_type_pitchers$`CT%` <- str_replace(pitch_type_pitchers$`CT%`, pattern = " %", replacement = "")
pitch_type_pitchers$`CB%` <- str_replace(pitch_type_pitchers$`CB%`, pattern = " %", replacement = "")
pitch_type_pitchers$`CH%` <- str_replace(pitch_type_pitchers$`CH%`, pattern = " %", replacement = "")
pitch_type_pitchers$`SF%` <- str_replace(pitch_type_pitchers$`SF%`, pattern = " %", replacement = "")
pitch_type_pitchers$`KN%` <- str_replace(pitch_type_pitchers$`KN%`, pattern = " %", replacement = "")
pitch_type_pitchers$`XX%` <- str_replace(pitch_type_pitchers$`XX%`, pattern = " %", replacement = "")

dbl_cols <- colnames(pitch_type_pitchers)[3:17]
pitch_type_pitchers %<>% mutate_at(dbl_cols, funs(as.numeric(.)))

pitch_type_pitchers %>% 
  group_by(Team) %>% 
  summarise("fb_rate" = mean(`FB%`)) %>% 
  arrange(desc(fb_rate))


# Get FD Salaries ---------------------------------------------------------

rg_fanduel_hitters <-
  read_csv(
    "https://rotogrinders.com/projected-stats/mlb-hitter.csv?site=fanduel",
    col_names = c(
      "Name",
      "Salary",
      "Team",
      "Position",
      "Opponent",
      "var1",
      "var2",
      "var3"
    )
  ) %>% 
  select(Name, Team, Position, Salary)

rg_fanduel_pitchers <-
  read_csv(
    "https://rotogrinders.com/projected-stats/mlb-pitcher.csv?site=fanduel",
    col_names = c(
      "Name",
      "Salary",
      "Team",
      "Position",
      "Opponent",
      "var1",
      "var2",
      "var3"
    )
  ) %>% 
  select(Name, Team, Position, Salary)

rg_fanduel_salaries <- bind_rows(rg_fanduel_hitters, rg_fanduel_pitchers) %>% 
  left_join(team_mappings, by = c("Team" = "Tm")) %>% 
  rename("Tm" = Team,
         "Team" = `Team.y`)

# Clean up workplace
rm(list=c("batting", "adv_batting_fangraphs", "url", "url_adv", "url_ptype", "dbl_cols", "url_fd"))

# Combine the batting stats with fanduel salaries
batting_stats <- batting_stats %>% 
  inner_join(rg_fanduel_salaries, by = c("Name", "Team")) 

## Average price per position
batting_stats %>% 
  group_by(Position) %>% 
  summarise(mean(Salary))

# wOBA vs. Salary
batting_stats %>% 
  filter(!Position == "P") %>% 
  ggplot(aes(x = wOBA, y = Salary, label = Name)) +
  geom_text() 

# ISO vs. Salary
batting_stats %>% 
  filter(!Position == "P") %>% 
  ggplot(aes(x = ISO, y = Salary, label = Name)) +
  geom_text() 

# Spd vs. Salary
batting_stats %>% 
  filter(!Position == "P") %>% 
  ggplot(aes(x = Spd, y = Salary, label = Name)) +
  geom_text() 

# K/BB vs. Salary
batting_stats %>% 
  filter(!Position == "P") %>% 
  ggplot(aes(x = `BB/K`, y = Salary, label = Name)) +
  geom_text() 


