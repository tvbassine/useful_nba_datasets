# 8/19/2020
# Purpose: Code for predicting the remaining average score in the series 
# based on the score in game 1.
########################################################################


x <- read.csv('https://raw.githubusercontent.com/tvbassine/bubble_playoffs_nylon/master/playoff_games_2003_2018.csv',
              stringsAsFactors = F)
head(x)
# Get data organized into 
# 1) pd_diff in regular season.
# 2) pd_diff in series so far.
# 3) Who is home?
# 4) score of remainder of series (output)

x$dateR <- as.Date(x$datee, '%b %d, %Y')

count <- 1
dat <- list()

for(j in unique(x$series_code)){
  z <- x[x$series_code ==j,]
  z <- z[order(z$dateR, decreasing = F),]
  z$game_num = 1:nrow(z)
  w <- data.frame(series_code = j,
                  game_num = 2,
                  score_game_1 = z$result[1],
                  pd_dif_reg_season = z$pd_diff[1],
                  home = z$home[2],
                  score_game_2 = z$result[2],
                  rest_of_series = 0,
                  stringsAsFactors = F)
  temp <- z$result
  ind <- which(z$away == z$home[1])
  temp[ind] <- -1 * temp[ind]
  w$rest_of_series[1] <- mean(temp[2:length(temp)])
  dat[[count]] = w
  count <- count + 1
  print(count)
}

# z <- do.call('rbind', dat)
# fit <- lm(score_game_2 ~ score_game_1 + pd_dif_reg_season,
#           data = z)
# summary(fit)
fit <- lm(rest_of_series ~ score_game_1 + pd_dif_reg_season,
          data = z)
summary(fit) # The model is here!
