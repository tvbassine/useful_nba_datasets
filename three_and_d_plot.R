# 3 And D Plot
# Purpose: scrape basketball-reference and to get 3P% and DBPM and make a nice plot.
# Date: 3/8/2021

library(ggplot2)
library(ggrepel)

source('https://raw.githubusercontent.com/tvbassine/useful_nba_datasets/master/player_overall_scraper_function.R')

# GET STATS:
ad <- pl_overall_scrape(yr = 2021, id = 'advanced')
tot <- pl_overall_scrape(yr = 2021, id = 'totals')
ad$yr = 2021
tot$yr = 2021

# Remove combined entries (i.e. players who played on more than 1 team)
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

# Restrict to 100 3PA
z <- y[y$fg3a >= 100,]
plot(z$fg3_pct, z$dbpm)

# Get labels to put on graph for select players:
z$last_nm <- 0
for(i in 1:nrow(z)){
  temp <- unlist(strsplit(z$name.x[i], split = ','))[1]
  z$last_nm[i] <- paste(temp, z$team_id.x[i], sep = ' ')
}

z$label_restrict <- NA
z$label_restrict[z$fg3_pct >= .4 & z$dbpm >= 0.5] <- z$last_nm[z$fg3_pct >= .4 & z$dbpm >= 0.5]
z$label_restrict[z$fg3_pct >= .45] <- z$last_nm[z$fg3_pct >= .45]
z$label_restrict[z$fg3_pct >= .425 & z$dbpm >= 0] <- z$last_nm[z$fg3_pct >= .425 & z$dbpm >= 0]
z$label_restrict[z$dbpm >= 1.3] <- z$last_nm[z$dbpm >= 1.3]

z$pt_size <- z$fg3a / 90

# Make ggplot:
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
# Save the plot (can change directory)
ggsave("~/Documents/three_and_d_plot.png", dpi = 400,
       height = 5.5, width = 8)
