# Thomas Bassine
# Date: 2/3/2020
# Purpose: Scrape 2020 and 2021 Advanced Player Stats from bbref.
# Make a pretty table of the largest BPM risers and fallers using
# formattable.
#######################################################################

source('https://raw.githubusercontent.com/tvbassine/useful_nba_datasets/master/player_overall_scraper_function.R')

x <- list()
count <- 1
for(i in 2020:2021){
  temp <- pl_overall_scrape(yr = i, id = 'advanced')
  temp$yr = i
  
  for(j in 1:nrow(temp)){
    ind <- which(temp$id == temp$id[j] & temp$yr == temp$yr[j])
    if(sum(temp$team_id[ind] == '') > 0){
      temp <- temp[!(temp$id == temp$id[j] & temp$yr == temp$yr[i] & temp$team_id != ''), ]
    }
  }
  
  if(i == 2020){
    temp <- temp[temp$mp >= 800,]}
  
  if(i == 2021){
    temp <- temp[temp$mp >= 125,]}
  
  #temp <- temp[order(temp$bpm, decreasing = T),]
  #temp$rank <- 1:nrow(temp)
  
  x[[count]] <- temp
  count <- count + 1
  print(i)
}

y <- do.call( 'rbind', x)
y <- y[!is.na(y$id),]
z <- merge(y[y$yr == 2020,],
           y[y$yr == 2021,],
           by = 'id', all = T)
z$`BPM delta` <- z$bpm.y - z$bpm.x
z$`BPM 2020` <- z$bpm.x
z$`BPM 2021` <- z$bpm.y
z$Player <- z$name.x
z$Team <- z$team_id.y
z$`MP 2020` <- z$mp.x
z$`MP 2021` <- z$mp.y

library(formattable)
library(data.table)
library(dplyr)

customGreen0 = "#DeF7E9"

customGreen = "#71CA97"

customRed = "#ff7f7f"

w <- z[,c('Player', 'Team', 'MP 2020', 'MP 2021',
          'BPM 2020', 'BPM 2021', 'BPM delta')]

improvement_formatter <- formatter("span", 
                                   style = x ~ style(font.weight = "bold", 
                                                     color = ifelse(x > 0, customGreen, ifelse(x < 0, customRed, "black"))), 
                                   x ~ icontext(ifelse(x>0, "arrow-up", "arrow-down"), x)
)


w <- w[order(w$`BPM delta`, decreasing = F),]


formattable(w, 
            align =c("l","c","c","c", "c","c", "c"), 
            list(`Player` = formatter(
              "span", style = ~ style(color = "grey",font.weight = "bold")),
              `BPM 2020`= color_tile('white', 'lightblue'), 
              `BPM 2021`= color_tile('white', 'lightblue'),
              `BPM delta` = improvement_formatter
            ))

