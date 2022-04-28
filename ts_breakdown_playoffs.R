# By: Thomas Bassine
# Date: 4/15/21
# Purpose: Get playoffs true shooting breakdown for each team.
############################################################################

# LOAD PACKAGES AND SOURCE SCRAPER FUNCTION;

library(dplyr)
library(ggplot2)
library(httr)

############################################################
# GATHER DATA:

# Let's get offensive and defensive rating for each team in each 
# playoff series.

library(httr)
library(jsonlite)
library(gt)
library(gtable)
library(dplyr)

url = 'https://stats.nba.com/stats/leaguedashplayerstats?College=&Conference=&Country=&DateFrom=&DateTo=&Division=&DraftPick=&DraftYear=&GameScope=&GameSegment=&Height=&LastNGames=0&LeagueID=00&Location=&MeasureType=Base&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PaceAdjust=N&PerMode=Totals&Period=0&PlayerExperience=&PlayerPosition=&PlusMinus=N&Rank=N&Season=2021-22&SeasonSegment=&SeasonType=Playoffs&ShotClockRange=&StarterBench=&TeamID=0&TwoWay=0&VsConference=&VsDivision=&Weight='


headers = c(
  `Connection` = 'keep-alive',
  `Accept` = 'application/json, text/plain, */*',
  `x-nba-stats-token` = 'true',
  `X-NewRelic-ID` = 'VQECWF5UChAHUlNTBwgBVw==',
  `User-Agent` = 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_14_6) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/78.0.3904.87 Safari/537.36',
  `x-nba-stats-origin` = 'stats',
  `Sec-Fetch-Site` = 'same-origin',
  `Sec-Fetch-Mode` = 'cors',
  `Referer` = 'https://stats.nba.com/',
  `Accept-Encoding` = 'gzip, deflate, br',
  `Accept-Language` = 'en-US,en;q=0.9'
)

res <- httr::GET(url = url, httr::add_headers(.headers=headers))
json_resp <- jsonlite::fromJSON(content(res, "text"))

df <- data.frame(json_resp$resultSets$rowSet)
colnames(df) <- json_resp$resultSets$headers[[1]]

############################################################
# TRANSFORM DATA:

df$tsa = as.numeric(df$FGA) + .44 * as.numeric(df$FTA)
df$ts_pct = as.numeric(df$PTS) / (2 * df$tsa)
head(df$ts_pct)

# Get total Tsa by team:
q <- aggregate(tsa ~ TEAM_ABBREVIATION, data = df,
               sum)
colnames(q)[2] = 'tsa_team'

library(dplyr)

z <- list()
count <- 1

for(tm in unique(df$TEAM_ABBREVIATION)){
  temp <- df[df$TEAM_ABBREVIATION == tm,]
  temp <- temp[order(temp$tsa, decreasing = T),]
  z[[count]] <- temp
  count <- count + 1
}

z <- do.call('rbind',z)

w <- z[,c('TEAM_ABBREVIATION', 'PLAYER_NAME', 'tsa', 'ts_pct')]

w <- merge(w, q, by = 'TEAM_ABBREVIATION')


w$tsa_of_team <- w$tsa / w$tsa_team

#Only keep guys with at least 5% of teams shots
w = w[w$tsa_of_team >= 0.05,]



# Get last names

w$last_nm <- 0
for(i in 1:nrow(w)){
  temp = gsub('Jr.' , '', w$PLAYER_NAME[i])
  temp = gsub('Sr.' , '', temp)
  temp <- unlist(strsplit(temp, split = ' '))
  temp = temp[length(temp)]
  w$last_nm[i] <- temp
}


east <- w[w$TEAM_ABBREVIATION %in% c('ATL','BOS','BKN','CHI', 'CHO',
                             'CLE','DET','IND','MIA','MIL',
                             'NYK','ORL','PHI','TOR','WAS'),]
west <- w[(!w$TEAM_ABBREVIATION %in% c('ATL','BOS','BKN','CHI', 'CHO',
                               'CLE','DET','IND','MIA','MIL',
                               'NYK','ORL','PHI','TOR','WAS')),]

############################################################
# PLOT DATA:

#Plot of Eastern Conference Teams
p <- ggplot(data=east, aes(x=TEAM_ABBREVIATION, 
                           y=tsa_of_team, fill=ts_pct)) +
  geom_bar(stat="identity", colour="black") +
  ylim(c(0,1)) + 
  xlab('Team') +
  ylab('Proportion of Team True Shot Attempts') + 
  ggtitle('Eastern Conference Playoffs Shooting Breakdown',
          subtitle = "1st Round | Only displaying players with at least 5% of team shot attempts. | source = nba.com") +
  labs(fill = "True Shooting %") +
  theme(plot.title = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 8, face = 'bold'),
        axis.title.x = element_text( size=13),
        axis.title.y = element_text(size=11),
        axis.text.x = element_text( size = 12),
        axis.text.y = element_text( size = 12),
        panel.background = element_rect(fill = "white",
                                        colour = "black",
                                        size = 1, linetype = "solid"),
        legend.title = element_text(color = "black", size = 10),
        legend.text = element_text(color = "black", size = 8),
        legend.box.background = element_rect(colour = "black", size = 1.2)
        
  ) + geom_text(aes(label=last_nm), position = position_stack(vjust = 0.5),
                               col = "black",
                               fontface = "bold", size=3.5) + 
  #scale_fill_gradient2(low = "darkred", mid = "white", high = "darkgreen", midpoint = .549)
  scale_fill_gradient2(low = "blue", mid = "white", high = "orange", midpoint = .563,
                       breaks=seq(.4,.8,.1))
p
# Save the plot (can change directory)
dt = Sys.Date()
ggsave(paste0("~/Documents/misc_nba_stuff/true_shooting_breakdown/ts_east_",
              dt,
              ".png"), dpi = 370,
       height = 5.5, width = 8)

# Plot of Western Conference Teams
p <- ggplot(data=west, aes(x=TEAM_ABBREVIATION, 
                           y=tsa_of_team, fill=ts_pct)) +
  geom_bar(stat="identity", colour="black") +
  ylim(c(0,1)) + 
  xlab('Team') +
  ylab('Proportion of Team True Shot Attempts') + 
  ggtitle('Western Conference Playoffs Shooting Breakdown',
          subtitle = "1st Round | Only displaying players with at least 5% of team shot attempts. | source = nba.com") +
  labs(fill = "True Shooting %") +
  theme(plot.title = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 8, face = 'bold'),
        axis.title.x = element_text( size=13),
        axis.title.y = element_text(size=11),
        axis.text.x = element_text( size = 12),
        axis.text.y = element_text( size = 12),
        panel.background = element_rect(fill = "white",
                                        colour = "black",
                                        size = 1, linetype = "solid"),
        legend.title = element_text(color = "black", size = 10),
        legend.text = element_text(color = "black", size = 8),
        legend.box.background = element_rect(colour = "black", size = 1.2)
        
  ) + geom_text(aes(label=last_nm), position = position_stack(vjust = 0.5),
                col = "black",
                fontface = "bold", size=3.5) + 
  #scale_fill_gradient2(low = "darkred", mid = "white", high = "darkgreen", midpoint = .549)
  scale_fill_gradient2(low = "blue", mid = "white", high = "orange", midpoint = .563,
                       breaks=seq(.4,.8,.1))
p

# Save the plot (can change directory)
dt = Sys.Date()
ggsave(paste0("~/Documents/misc_nba_stuff/true_shooting_breakdown/ts_west_",
              dt,
              ".png"), dpi = 370,
       height = 5.5, width = 8)


