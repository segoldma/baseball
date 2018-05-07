library(rvest)
library(dplyr)
library(tibble)
library(stringr)

## Get standard batting stats from fangraphs
url <- read_html("https://www.fangraphs.com/leaders.aspx?pos=all&stats=bat&lg=all&qual=y&type=8&season=2018&month=0&season1=2018&ind=0&page=1_500") 

batting <- html_nodes(url,".rgMasterTable") %>% 
  html_table() %>% 
  as.data.frame() 

# Rename columns and remove the header rows
colnames(batting) <- as.character(unlist(batting[2,]))
batting <- batting %>% 
  slice(c(-1,-2,-3)) %>% #remove header rows
  select(Name,Team,G, HR,R,RBI,SB,BsR) #keep counting stats that aren't in the advanced stats table
  
# Get advanced batting stats from fangraphs
url_adv <- read_html("https://www.fangraphs.com/leaders.aspx?pos=all&stats=bat&lg=all&qual=y&type=1&season=2018&month=0&season1=2018&ind=0&team=0&rost=0&age=0&filter=&players=0&page=1_500")

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
