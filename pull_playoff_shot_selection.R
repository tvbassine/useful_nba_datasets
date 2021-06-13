# Thomas Bassine
# June 13, 2021
# Purpose: Compare shot making and selection in a playoff round to the regular season.

##########################################################################
# Get each playoff team's shooting stats from a particular playoff round:

PO_round <- 2
yr = '2020-21'

url <- paste('https://stats.nba.com/stats/leaguedashteamshotlocations?Conference=&DateFrom=&DateTo=&DistanceRange=By+Zone&Division=&GameScope=&GameSegment=&LastNGames=0&LeagueID=00&Location=&MeasureType=Base&Month=0&OpponentTeamID=0&Outcome=&PORound=',
             PO_round,
             '&PaceAdjust=N&PerMode=PerGame&Period=0&PlayerExperience=&PlayerPosition=&PlusMinus=N&Rank=N&Season=',
             yr,
             '&SeasonSegment=&SeasonType=Playoffs&ShotClockRange=&StarterBench=&TeamID=0&VsConference=&VsDivision=',
             sep = '')

#url = c('https://stats.nba.com/stats/leaguedashteamshotlocations?Conference=&DateFrom=&DateTo=&DistanceRange=By+Zone&Division=&GameScope=&GameSegment=&LastNGames=0&LeagueID=00&Location=&MeasureType=Base&Month=0&OpponentTeamID=0&Outcome=&PORound=2&PaceAdjust=N&PerMode=PerGame&Period=0&PlayerExperience=&PlayerPosition=&PlusMinus=N&Rank=N&Season=2020-21&SeasonSegment=&SeasonType=Playoffs&ShotClockRange=&StarterBench=&TeamID=0&VsConference=&VsDivision=')

library(tidyverse)
library(httr)

headers = c(
  `Connection` = 'keep-alive',
  `Accept` = 'application/json, text/plain, */*',
  `x-nba-stats-token` = 'true',
  `X-NewRelic-ID` = 'VQECWF5UChAHUlNTBwgBVw==',
  `User-Agent` = 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_14_6) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/78.0.3904.87 Safari/537.36',
  `x-nba-stats-origin` = 'stats',
  `Sec-Fetch-Site` = 'same-origin',
  `Sec-Fetch-Mode` = 'cors',
  `Referer` = 'https://stats.nba.com/players/leaguedashteamshotlocations/',
  `Accept-Encoding` = 'gzip, deflate, br',
  `Accept-Language` = 'en-US,en;q=0.9'
)

res <- httr::GET(url = url, httr::add_headers(.headers=headers))
json_resp <- jsonlite::fromJSON(content(res, "text"))

df <- data.frame(json_resp$resultSets$rowSet)
colnames(df) <- json_resp$resultSets$headers$columnNames[[2]]

head(df)

for(i in 3:23){
  df[,i] = as.numeric(as.character(df[,i]))
}

######################################################
# Get each team's regular season shooting stats:

url <- paste('https://stats.nba.com/stats/leaguedashteamshotlocations?Conference=&DateFrom=&DateTo=&DistanceRange=By+Zone&Division=&GameScope=&GameSegment=&LastNGames=0&LeagueID=00&Location=&MeasureType=Base&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PaceAdjust=N&PerMode=PerGame&Period=0&PlayerExperience=&PlayerPosition=&PlusMinus=N&Rank=N&Season=',
             yr,
             '&SeasonSegment=&SeasonType=Regular+Season&ShotClockRange=&StarterBench=&TeamID=0&VsConference=&VsDivision=',
             sep ='')

#url = c('https://stats.nba.com/stats/leaguedashteamshotlocations?Conference=&DateFrom=&DateTo=&DistanceRange=By+Zone&Division=&GameScope=&GameSegment=&LastNGames=0&LeagueID=00&Location=&MeasureType=Base&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PaceAdjust=N&PerMode=PerGame&Period=0&PlayerExperience=&PlayerPosition=&PlusMinus=N&Rank=N&Season=2020-21&SeasonSegment=&SeasonType=Regular+Season&ShotClockRange=&StarterBench=&TeamID=0&VsConference=&VsDivision=')

library(tidyverse)
library(httr)

headers = c(
  `Connection` = 'keep-alive',
  `Accept` = 'application/json, text/plain, */*',
  `x-nba-stats-token` = 'true',
  `X-NewRelic-ID` = 'VQECWF5UChAHUlNTBwgBVw==',
  `User-Agent` = 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_14_6) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/78.0.3904.87 Safari/537.36',
  `x-nba-stats-origin` = 'stats',
  `Sec-Fetch-Site` = 'same-origin',
  `Sec-Fetch-Mode` = 'cors',
  `Referer` = 'https://stats.nba.com/players/leaguedashteamshotlocations/',
  `Accept-Encoding` = 'gzip, deflate, br',
  `Accept-Language` = 'en-US,en;q=0.9'
)

res <- httr::GET(url = url, httr::add_headers(.headers=headers))
json_resp <- jsonlite::fromJSON(content(res, "text"))

df2 <- data.frame(json_resp$resultSets$rowSet)
colnames(df2) <- json_resp$resultSets$headers$columnNames[[2]]

head(df2)

for(i in 3:23){
  df2[,i] = as.numeric(as.character(df2[,i]))
}

#######################################################################
# Get proportion of shots from at the basket, midrange, and 3:
# REGULAR SEASON

df2$total_fga <- df2[,4] + df2[,7] + df2[,10] + df2[,13] + df2[,16] + + df2[,19]
df2$prop_ra <- df2[,4] / df2$total_fga #restricted area
df2$prop_2_non_ra <- (df2[,7] + df2[,10]) / df2$total_fga #2's not in restricted area
df2$prop_3 <- 1 - (df2$prop_ra + df2$prop_2_non_ra) #3's

df2$ra_pct <- df2[,5]
df2$non_ra_2_pct <- (df2[,6] + df2[,9]) / (df2[,7] + df2[,10])
df2$three_pct <- (df2[,12] + df2[,15] + df2[,18]) / (df2[,13] + df2[,16] + df2[,19])

# Get proportion of shots from at the basket, midrange, and 3:
# PLAYOFFS

df$total_fga <- df[,4] + df[,7] + df[,10] + df[,13] + df[,16] + + df[,19]
df$prop_ra <- df[,4] / df$total_fga #restricted area
df$prop_2_non_ra <- (df[,7] + df[,10]) / df$total_fga #2's not in restricted area
df$prop_3 <- 1 - (df$prop_ra + df$prop_2_non_ra) #3's

df$ra_pct <- df[,5]
df$non_ra_2_pct <- (df[,6] + df[,9]) / (df[,7] + df[,10])
df$three_pct <- (df[,12] + df[,15] + df[,18]) / (df[,13] + df[,16] + df[,19])

x <- merge(df, df2, by = 'TEAM_NAME', all.x = T)

y <- x[,c(1,25:30,54:59)]
colnames(y) = c('Team', 'PO_prop_restricted_area',
                'PO_prop_2_non_ra', 'PO_prop_3',
                'PO_ra_pct', 'PO_2_non_ra_pct', 'PO_3_pct',
                'RS_prop_restricted_area',
                'RS_prop_2_non_ra', 'RS_prop_3',
                'RS_ra_pct', 'RS_2_non_ra_pct', 'RS_3_pct')
y$delta_ra <- y$PO_prop_restricted_area - y$RS_prop_restricted_area
y$delta_2_non_ra <- y$PO_prop_2_non_ra - y$RS_prop_2_non_ra
y$delta_3 <- y$PO_prop_3 - y$RS_prop_3

y$delta_ra_pct <- y$PO_ra_pct - y$RS_ra_pct
y$delta_2_non_ra_pct <- y$PO_2_non_ra_pct - y$RS_2_non_ra_pct
y$delta_3_pct <- y$PO_3_pct - y$RS_3_pct

#######################################################################
# Make a nice table of shot selection:

z <- y[,c(1,14:16)]
for(i in 2:4){
  z[,i] = round(z[,i], 3)
}
colnames(z) = c('Team', 'Change in Proportion of RA',
                'Change in Proportion of 2 Non-RA',
                'Change in Proportion of 3')

library(formattable)
library(data.table)
library(dplyr)

improvement_formatter <- 
  formatter("span", 
            style = x ~ style(
              font.weight = "bold", 
              color = ifelse(x > 0, 'green', 'red')))


formattable(z, 
            align =c("l","c","c","c"), 
            list(`Team` = formatter(
              "span", style = ~ style(color = "grey",font.weight = "bold")),
              `Change in Proportion of RA`= improvement_formatter, 
              `Change in Proportion of 2 Non-RA`= improvement_formatter, 
              `Change in Proportion of 3`= improvement_formatter
            ))

#######################################################################
# Make a nice table of shot making:

z <- y[,c(1,17:19)]
for(i in 2:4){
  z[,i] = round(z[,i], 3)
}
colnames(z) = c('Team', 'Change in RA%',
                'Change in 2 Non-RA%',
                'Change in 3P%')

library(formattable)
library(data.table)
library(dplyr)

improvement_formatter <- 
  formatter("span", 
            style = x ~ style(
              font.weight = "bold", 
              color = ifelse(x > 0, 'green', 'red')))

formattable(z, 
            align =c("l","c","c","c"), 
            list(`Team` = formatter(
              "span", style = ~ style(color = "grey",font.weight = "bold")),
              `Change in RA%`= improvement_formatter, 
              `Change in 2 Non-RA%`= improvement_formatter, 
              `Change in 3P%`= improvement_formatter
            ))