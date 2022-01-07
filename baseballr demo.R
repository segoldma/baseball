library(baseballr)
library(lubridate)
library(ggplot2)
library(dplyr)
library(purrr)

df <- fg_bat_leaders(2018, 2018)

consistency <- baseballr::team_consistency(2018)

baseballr::
df %>% 
  ggplot(aes(x=reorder(Name,RE24), y = RE24))+
  geom_point()+
  coord_flip()

df %>% 
  ggplot(aes(x=OBP,y=wOBA))+
  geom_point()

baseballr::scrape_statcast_savant(start_date = "2018-06-01", end_date = "2018-06-01", player_type = "batter")

statcast_all_pitchers <- baseballr::scrape_statcast_savant_pitcher_all(start_date = "2018-09-15", end_date = "2018-09-30")

library(ggplot2)
library(forcats)

statcast_all_pitchers %>% 
  filter(!is.na(pitch_type)) %>% 
  mutate(pitch_type = as.factor(pitch_type)) %>% 
  ggplot(aes(x=fct_reorder(pitch_type, release_speed), y = release_speed)) +
  geom_boxplot()+
  coord_flip()

statcast_all_pitchers %>% 
  filter(pitch_type == "PO", release_speed > 90) %>% 
  View("fastpo")

# Print today's NL East standings
standings_on_date_bref(today(), "NL East")

# Past two days
standings_on_date_bref(today()-1, "NL East")
standings_on_date_bref(today()-2, "NL East")

# View all functions
lsf.str("package:baseballr")


# Get fangraphs batting leaders from 2017-2018
fangraphs_batting_leaders <- fg_bat_leaders(2017,2018, league = "all", qual = "y") 

# Try Map to calculate K/BB ration
map(fangraphs_batting_leaders, ~fangraphs_batting_leaders$SO/fangraphs_batting_leaders$BB)

# View top 10 BABIP
fangraphs_batting_leaders %>% 
  select(Name,Team,BABIP) %>% 
  arrange(desc(BABIP)) %>% 
  head(10)

df <- baseballr::scrape_statcast_savant(start_date = Sys.Date()-1, end_date = Sys.Date()-1)

??scrape_statcast_savant
devtools::install_github("BillPetti/baseballr")

