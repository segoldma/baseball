require(devtools)
install_github("BillPetti/baseballr")
require(baseballr)
library(lubridate)
library(dplyr)

# Print today's NL East standings
standings_on_date_bref(today(), "NL East")

# Past two days
standings_on_date_bref(today()-1, "NL East")
standings_on_date_bref(today()-2, "NL East")

# View all functions
lsf.str("package:baseballr")


# Get fangraphs batting leaders from 2017-2018
fangraphs_batting_leaders <- fg_bat_leaders(2017,2018, league = "all", qual = "y") 

# View top 10 BABIP
fangraphs_batting_leaders %>% 
  select(Name,Team,BABIP) %>% 
  arrange(desc(BABIP)) %>% 
  head(10)
