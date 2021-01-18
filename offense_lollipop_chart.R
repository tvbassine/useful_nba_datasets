# Plot Improvements in Offensive and Defensive Rating:
# Purpose: This code scrapes basketball-reference and compares o and d ratings 
# from this season to last season.
# Created: Jan. 18, 2020
# By: Thomas Bassine

# Get function used to scrape net ratings from basketball-reference:
source('https://raw.githubusercontent.com/tvbassine/useful_nba_datasets/master/tm_table_scrape.R')

# Scrape 2019-20 and 2020-21 stats
x20 <- tm_table_scrape(yr = 2020, id = 'misc_stats_link')
x21 <- tm_table_scrape(yr = 2021, id = 'misc_stats_link')

y <- merge(x20, x21, by = 'tm')

# Offensive Plot:
y$off_rtg_21 <- y$off_rtg.y - mean(y$off_rtg.y)
y$off_rtg_20 <- y$off_rtg.x - mean(y$off_rtg.x)

library(ggplot2)

y <- y[order(y$off_rtg_21, decreasing = F),]
y$tm <- factor(y$tm, y$tm)

ggplot(y) +
  geom_segment( aes(x=tm, xend=tm, y=off_rtg_20, yend=off_rtg_21), color="grey") +
  geom_point( aes(x=tm, y=off_rtg_20), color=rgb(0.2,0.7,0.1,0.5), size=3 ) +
  geom_point( aes(x=tm, y=off_rtg_21), color=rgb(0.7,0.2,0.1,0.5), size=3 ) +
  coord_flip()+
  xlab("") +
  ylab("Relative Offensive Rating")+
  ggtitle('Which Offenses Have Improved The Most?',
        subtitle = paste('Red = 2021, Green = 2020 | Plot by @tvbassine | Data from basketball-reference | As of ', Sys.Date(), sep = '')) + 
  theme(plot.title = element_text(size = 20, lineheight=.8, face="bold"),
        legend.position="bottom")
  

# Defensive Plot:
y$def_rtg_21 <- -1 * (y$def_rtg.y - mean(y$def_rtg.y))
y$def_rtg_20 <- -1 * (y$def_rtg.x - mean(y$def_rtg.x))

library(ggplot2)

y <- y[order(y$def_rtg_21, decreasing = F),]
y$tm <- factor(y$tm, y$tm)

ggplot(y) +
  geom_segment( aes(x=tm, xend=tm, y=def_rtg_20, yend=def_rtg_21), color="grey") +
  geom_point( aes(x=tm, y=def_rtg_20), color=rgb(0.2,0.7,0.1,0.5), size=3 ) +
  geom_point( aes(x=tm, y=def_rtg_21), color=rgb(0.7,0.2,0.1,0.5), size=3 ) +
  coord_flip()+
  xlab("") +
  ylab("Relative Defensive Rating")+
  ggtitle('Which Defenses Have Improved The Most?',
          subtitle = paste('Red = 2021, Green = 2020 | Plot by @tvbassine | Data from basketball-reference | As of ', Sys.Date(), sep = '')) + 
  theme(plot.title = element_text(size = 20, lineheight=.8, face="bold"),
        legend.position="bottom")
