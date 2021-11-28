# Generic playing time function for a given team:

# Enter the team you wish to examine here
tm_nm = 'Washington Wizards'

# Need to install nbastatR if youi have not already.
library(nbastatR)
library(ggplot2)
library(dplyr)

# I needed to change this setting to read in the data for game_logs, 
# so uncomment if you need to.
# Sys.setenv("VROOM_CONNECTION_SIZE" = 131072 * 2)

x = game_logs(seasons = 2022)
nets = x[x$nameTeam == tm_nm,]


y = expand.grid(namePlayer = unique(nets$namePlayer),
                gameNumber = unique(nets$numberGameTeamSeason),
                mp = 0,
                stringsAsFactors = F)

for(i in 1:nrow(y)){
  temp = nets[nets$namePlayer == y$namePlayer[i] &
                nets$numberGameTeamSeason == y$gameNumber[i],]
  if(nrow(temp) > 0){
    y$mp[i] = temp$minutes
  }
}

out = aggregate(mp ~ namePlayer, y, sum)
out = out[order(out$mp, decreasing = F),]

y$player = factor(y$namePlayer, levels = out$namePlayer)

p = ggplot(y, aes(gameNumber, player, fill= mp)) + 
  geom_tile() +
  xlab('Game Number') + ylab('') +
  scale_fill_gradient(low = "white", high = "red") + 
  labs(fill = 'Minutes Played') +
  ggtitle(paste0(tm_nm, ' Playing Time By Game'),
          subtitle = 'Plot by @tvbassine | Data from nbastatR package') +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.subtitle = element_text( hjust = 0.5),
    axis.title.x = element_text( size=12),
    axis.title.y = element_text(size=14),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(color = 'white') )

p


# Save your plot:
# ggsave(paste0('~/Documents/misc_nba_stuff/nets_playing_time/nets_playing_time_', Sys.Date(), '.png'), 
#        dpi = 370,
#        height = 5.5, width = 8)

