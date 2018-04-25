### mlbgameday: Tools to Gather Data from Major League Baseball Advanced Media (MLBAM)


#documentation: https://cran.r-project.org/web/packages/mlbgameday/mlbgameday.pdf

# Install and load package from CRAN
install.packages("mlbgameday")
library(mlbgameday)
library(dplyr)
library(lubridate)
library(skimr)

yesterday()

# View vignette
?mlbgameday

df <- get_payload(start = today()-1, end = today()-1)

action <- df$atbat %>% 
  group_by(event) %>% 
  tally()


# Skim the payload
skim(df$atbat)
skim(df$action)
skim(df$pitch)
skim(df$runner)
skim(df$po)

