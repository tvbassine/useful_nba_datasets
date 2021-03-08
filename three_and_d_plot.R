# Get the player with the most true shooting attempts per team.
# Merge with true shooting percentage.

source('~/Desktop/Threes and Layups Articles/Useful Things/player_overall_scraper_function.R')

# GET STATS:
ad <- pl_overall_scrape(yr = 2021, id = 'advanced')
tot <- pl_overall_scrape(yr = 2021, id = 'totals')
ad$yr = 2021
tot$yr = 2021

ad <- ad[ad$team_id != '',]
ad$id_tm <- paste(ad$id, ad$team_id, sep = '_')
tot <- tot[tot$team_id != '',]
tot$id_tm <- paste(tot$id, tot$team_id, sep = '_')

y <- merge(ad,
           tot,
           by = 'id_tm', all = T)
y <- data.frame(y)

summary(y$fg3a)
sum(y$fg3a >= 100)

z <- y[y$fg3a >= 100,]
plot(z$fg3_pct, z$dbpm)


z$last_nm <- 0
for(i in 1:nrow(z)){
  temp <- unlist(strsplit(z$name.x[i], split = ','))[1]
  z$last_nm[i] <- paste(temp, z$team_id.x[i], sep = ' ')
}

z$label_restrict <- NA
z$label_restrict[z$fg3_pct >= .4 & z$dbpm >= 0.5] <- z$last_nm[z$fg3_pct >= .4 & z$dbpm >= 0.5]
z$label_restrict[z$fg3_pct >= .45] <- z$last_nm[z$fg3_pct >= .45]
z$label_restrict[z$fg3_pct >= .425 & z$dbpm >= 0] <- z$last_nm[z$fg3_pct >= .425 & z$dbpm >= 0]
z$label_restrict[z$dbpm >= 1] <- z$last_nm[z$dbpm >= 1]

z$pt_size <- z$fg3a / 60

library(ggplot2)
library(ggrepel)

b <- ggplot(data = z, aes(x = fg3_pct, y=dbpm, label = label_restrict)) + 
  geom_point(col = 'blue', size = z$pt_size) + 
  geom_text_repel() + 
  ggtitle('The \"3 And D\" Plot',
          subtitle = paste('Plot by @tvbassine | Data from basketball-reference | As of ', Sys.Date(), sep = '')) +
  xlab('3P% (minimum 100 3PA)') +
  ylab('Defensive Box Plus-Minus\n (via basketball-reference)') +
  theme(plot.title = element_text(size = 20, face = "bold", hjust = .5),
        plot.subtitle = element_text(size = 12, face = "bold", hjust = .5),
        axis.title.x = element_text( size=14),
        axis.title.y = element_text(size=14),
        axis.text.x = element_text( size = 12),
        axis.text.y = element_text( size = 12),
        panel.background = element_rect(fill = "white",
                                        colour = "black"),
        panel.grid.major = element_line(size = 0.5, linetype = 'dashed',
                                        colour = "grey"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'dashed',
                                        colour = "grey"),
        plot.background = element_rect(fill = "lightblue")) +
  annotate(geom = 'text', x = .48, y = 1.8, label = 'Circle size proportional\n to 3FGA',
           size = 5)
b
