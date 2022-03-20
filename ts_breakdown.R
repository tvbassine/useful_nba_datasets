# By: Thomas Bassine
# Date: 4/15/21
# Purpose: give a true shooting breakdown for each team.
############################################################################

# LOAD PACKAGES AND SOURCE SCRAPER FUNCTION;

source('https://raw.githubusercontent.com/tvbassine/useful_nba_datasets/master/player_overall_scraper_function.R')
library(dplyr)
library(ggplot2)

############################################################
# GATHER DATA:
ad <- pl_overall_scrape(yr = 2022, id = 'advanced')
tot <- pl_overall_scrape(yr = 2022, id = 'totals')
ad$yr = 2022
tot$yr = 2022

ad <- ad[ad$team_id != '',]
ad$id_tm <- paste(ad$id, ad$team_id, sep = '_')
tot <- tot[tot$team_id != '',]
tot$id_tm <- paste(tot$id, tot$team_id, sep = '_')

y <- merge(ad,
           tot,
           by = 'id_tm', all = T)
y <- data.frame(y)


############################################################
# TRANSFORM DATA:
y$tsa <- y$fga + 0.44 * y$fta

# Get total Tsa by team:
q <- aggregate(tsa ~ team_id.x, data = y,
               sum)
colnames(q)[2] = 'tsa_team'

library(dplyr)

z <- list()
count <- 1

for(tm in unique(y$team_id.x)){
  temp <- y[y$team_id.x == tm,]
  temp <- temp[order(temp$tsa, decreasing = T),]
  z[[count]] <- temp
  count <- count + 1
}

z <- do.call('rbind',z)

w <- z[,c('team_id.x', 'name.x', 'tsa', 'ts_pct')]

w <- merge(w, q, by = 'team_id.x')


w$tsa_of_team <- w$tsa / w$tsa_team

#Only keep guys with at least 5% of teams shots
w = w[w$tsa_of_team >= 0.05,]



# Get last names
w$last_nm <- 0
for(i in 1:nrow(w)){
  temp <- unlist(strsplit(w$name.x[i], split = ','))[1]
  w$last_nm[i] <- temp
}


east <- w[w$team_id.x %in% c('ATL','BOS','BRK','CHI', 'CHO',
                             'CLE','DET','IND','MIA','MIL',
                             'NYK','ORL','PHI','TOR','WAS'),]
west <- w[(!w$team_id.x %in% c('ATL','BOS','BRK','CHI', 'CHO',
                               'CLE','DET','IND','MIA','MIL',
                               'NYK','ORL','PHI','TOR','WAS')),]

############################################################
# PLOT DATA:

#Plot of Eastern Conference Teams
p <- ggplot(data=east, aes(x=team_id.x, y=tsa_of_team, fill=ts_pct)) +
  geom_bar(stat="identity") +
  xlab('Team') +
  ylab('Proportion of Team True Shot Attempts') + ggtitle('Eastern Conference Shooting Breakdown') +
  labs(fill = "True Shooting %") +
  theme(plot.title = element_text(size = 17, face = "bold"),
        axis.title.x = element_text( size=14),
        axis.title.y = element_text(size=14),
        axis.text.x = element_text( size = 12),
        axis.text.y = element_text( size = 12),
        panel.background = element_rect(fill = "white",
                                        colour = "grey",
                                        size = 0.5, linetype = "solid"),
        legend.title = element_text(color = "black", size = 14),
        legend.text = element_text(color = "black", size = 12),
        legend.box.background = element_rect(colour = "black", size = 1.2)
        
  ) + coord_flip() + geom_text(aes(label=last_nm), position = position_stack(vjust = 0.5),
                               color="black", size=2.5) + 
  #scale_fill_gradient2(low = "darkred", mid = "white", high = "darkgreen", midpoint = .549)
  scale_fill_gradient2(low = "blue", mid = "white", high = "orange", midpoint = .563)

p
# Save the plot (can change directory)
dt = Sys.Date()
ggsave(paste0("~/Documents/misc_nba_stuff/true_shooting_breakdown/ts_east_",
              dt,
              ".png"), dpi = 370,
       height = 5.5, width = 8)

# Plot of Western Conference Teams
p <- ggplot(data=west, aes(x=team_id.x, y=tsa_of_team, fill=ts_pct)) +
  geom_bar(stat="identity") +
  xlab('Team') +
  ylab('Proportion of Team True Shot Attempts') + ggtitle('Western Conference Shooting Breakdown') +
  labs(fill = "True Shooting %") +
  theme(plot.title = element_text(size = 17, face = "bold"),
        axis.title.x = element_text( size=14),
        axis.title.y = element_text(size=14),
        axis.text.x = element_text( size = 12),
        axis.text.y = element_text( size = 12),
        panel.background = element_rect(fill = "white",
                                        colour = "grey",
                                        size = 0.5, linetype = "solid"),
        legend.title = element_text(color = "black", size = 14),
        legend.text = element_text(color = "black", size = 12),
        legend.box.background = element_rect(colour = "black", size = 1.2)
        
  ) + coord_flip() + geom_text(aes(label=last_nm), position = position_stack(vjust = 0.5),
                               color="black", size=2.5) + 
  #scale_fill_gradient2(low = "darkred", mid = "white", high = "darkgreen", midpoint = .549)
  scale_fill_gradient2(low = "blue", mid = "white", high = "orange", midpoint = .563)
p

# Save the plot (can change directory)
dt = Sys.Date()
ggsave(paste0("~/Documents/misc_nba_stuff/true_shooting_breakdown/ts_west_",
              dt,
              ".png"), dpi = 370,
       height = 5.5, width = 8)


