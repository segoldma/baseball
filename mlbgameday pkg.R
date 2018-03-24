### mlbgameday: Tools to Gather Data from Major League Baseball Advanced Media (MLBAM)


#documentation: https://cran.r-project.org/web/packages/mlbgameday/mlbgameday.pdf

# Install and load package from CRAN
install.packages("mlbgameday")
library(mlbgameday)

# View vignette
vignettes(mlbgameday)

df <- get_payload(start = "2016-06-01", end = "2016-06-01")
