### FanDuel Scoring Analysis



# Load Batting Data from FG -----------------------------------------------

# Get stats from Fangraphs ------------------------------------------------

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
  facet_wrap(~Team) %>% 
  ggtitle("FPTS Density by Team (Qualified Players Only)") +
  ggsave(paste0("FP Density by Team ", today()),device = png)



# Hit Density by Team
batting %>% 
  ggplot(aes(x=H, fill = Team)) +
  geom_density(alpha=0.3) +
  facet_wrap(~Team) 
