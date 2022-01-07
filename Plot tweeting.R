#Tweet a plot

library(twitteR)
library(dplyr)
library(lubridate)
library(ggplot2)
library(mlbgameday)

# Authenticate to twitter
setup_twitter_oauth(consumer_key = Sys.getenv("consumer_key"),
                    access_token = Sys.getenv("access_token"),
                    consumer_secret = Sys.getenv("consumer_secret"),
                    access_secret = Sys.getenv("access_secret"))

# Query some data
df <- get_payload(start = today()-2, end = today()-1)

pitches <- df$pitch

pitches %>% 
  ggplot(aes(x=spin_dir, y = start_speed, col = pitch_type)) +
  geom_point() +
  ggtitle("Spin direction vs Initial velocity") +
  facet_wrap(~pitch_type) +
  theme(legend.position = "none")

ggsave("spinvelo.png")

updateStatus("Spin direction vs. starting velocity of pitches in MLB games played yesterday", mediaPath = "spinvelo.png")

pitches %>% 
  ggplot(aes(x=spin_rate, y = start_speed, col = pitch_type)) +
  geom_point() +
  ggtitle("Spin rate vs Initial velocity") +
  facet_wrap(~pitch_type) +
  theme(legend.position = "none")

ggsave("spinratevelo.png")

updateStatus("... and spin rate vs. starting velocity of pitches in MLB games played yesterday", mediaPath = "spinratevelo.png")


pitches %>% 
  ggplot(aes(x=spin_rate,y=nasty, col = code)) +
  geom_point()+
  facet_wrap(~des, nrow=4)

ggsave("nastyplot.png")

updateStatus("Not sure what this 'nasty' stat means", mediaPath = "nastyplot.png")


summary(df$action)



