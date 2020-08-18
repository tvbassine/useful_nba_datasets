# This code creates a simple model to simulate the 1st round of the NBA
# playoffs :

#################################
# 1) Get the results so far:
source('https://raw.githubusercontent.com/tvbassine/useful_nba_datasets/master/schedule_scraper_july_august.R')
out <- sched_2020(2020)
# Playoff games start on August 17th
out <- out[out$dateR >= as.Date('2020-08-17'),]
# Who was the winner?
out$home_margin <- out$h.sc - out$a.sc
out$winner <- dplyr::if_else(out$home_margin > 0, out$home, out$away)
# Get the teams playing in each series:
z <- data.frame(series_code = 1:8,
                away = out$away[1:8],
                home = out$home[1:8],
                pd_dif = 0,
                home_wins = 0,
                away_wins = 0,
                home_win_prob = 0)
# How many wins for each team in the series?
for(i in 1:8){
  z$away_wins[i] = sum(out$winner == z$away[i], na.rm = T)
  z$home_wins[i] = sum(out$winner == z$home[i], na.rm = T)
}

#################################
# 2) 
# Get regular season point differential for each team in the series:
source('https://raw.githubusercontent.com/tvbassine/useful_nba_datasets/master/tm_table_scrape.R')
misc <- tm_table_scrape(2020, 'misc')
z <- merge(z, misc[,c('tm', 'mov')],
           by.x = 'away', by.y = 'tm',
           all.x = T)
z <- merge(z, misc[,c('tm', 'mov')],
           by.x = 'home', by.y = 'tm',
           all.x = T)
z$pd_dif <- z$mov.y - z$mov.x


#################################
# 3) Create a function for computing series win probabilities based on 
# the point differentials of the teams playing and the results so far.

# Read in the game level data for all playoff games from 2003-2018
hist_data <- read.csv('https://raw.githubusercontent.com/tvbassine/bubble_playoffs_nylon/master/playoff_games_2003_2018.csv', stringsAsFactors = F)

# Make a simple model for the probability of winning an individual playoff game
# based on the point differentials of the teams playing.
summary(fit <- lm(result ~ pd_diff, data = hist_data))

# Use the model just created to get the 'home' team's probability of winning any one game:
z$home_single_game <-  pnorm(1.04 * z$pd_dif, mean = 0, sd = 12.68)

# Function for computing series win probability:
series_win_prob = function(home_single_game, hwins, awins){
  if(hwins == 4){
    win_prob = 1
  }
  if(awins == 4){
    win_prob = 0
  }
  if(hwins < 4 & awins < 4){
    gms_left = 7 - (hwins + awins)
    h_wins_needed = 4 - hwins
    win_prob = pbinom(q = (h_wins_needed-1), size = gms_left, 
           prob = home_single_game, lower.tail = F)
  }
  
  return(win_prob)
}

# Apply series win probability function to get cureent win probs:
for(i in 1:8){
  z$home_win_prob[i] = series_win_prob(home_single_game = z$home_single_game[i],
                  hwins = z$home_wins[i],
                  awins = z$away_wins[i])
}


#################################
# 4) Display results in a pretty table:
library(formattable)

z$home_win_prob <- round(z$home_win_prob,2)
a = z[,c('home','away','home_wins', 'away_wins', 'home_win_prob')]
a$rec <- paste(a$home_wins, a$away_wins, sep = '-')
a = a[,c('home','away','rec', 'home_win_prob')]
colnames(a) = c('Home', 'Away', 'Home/Away Wins', 'Home Win Prob')
a$`Home Win Prob` <- percent(a$`Home Win Prob`, digits = 0)
formattable(a,
            list(`Home Win Prob` = color_bar("lightblue")))


