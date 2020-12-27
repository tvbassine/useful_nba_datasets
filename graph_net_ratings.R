# Automatically Graph Net Ratings
# Purpose: This code scrapes basketball-reference and compares net ratings 
# from this season to last season.
# Created: Dec. 27, 2020
# By: Thomas Bassine

# Get function used to scrape net ratings from basketball-reference:
source('https://raw.githubusercontent.com/tvbassine/useful_nba_datasets/master/tm_table_scrape.R')

# Scrape 2019-20 and 2020-21 stats
x20 <- tm_table_scrape(yr = 2020, id = 'misc_stats_link')
x21 <- tm_table_scrape(yr = 2021, id = 'misc_stats_link')

# Merge data from two seasons by team name
y <- merge(x20, x21, by = 'tm')

# Label net rating columns:
y$net_rating_2021 <- y$net_rtg.y
y$net_rating_2020 <- y$net_rtg.x

# Load plotting libraries
# You can install these using the install.package function if they aren't installed.
library(ggplot2)
library(ggrepel)

# Make pretty axis limits
max_plot <- max(abs(c(y$net_rating_2021,y$net_rating_2020)))


# Plot net ratings for 2021 against 2020
ggplot(y, aes(x=net_rating_2020, y=net_rating_2021, label = tm)) + 
  geom_point() + 
  xlim(c(-1*max_plot - 1, max_plot + 1)) +
  ylim(c(-1*max_plot - 1, max_plot + 1)) +
  xlab('Net Rating 2019-20 Regular Season') +
  ylab('Net Rating 2020-21 Regular Season') +
  geom_abline(intercept = 0, slope = 1, lty = 2, col = 'red') +
  geom_text_repel() +
  ggtitle('How Does Each NBA Team Compare To Last Season?',
          subtitle = paste('Plot by @tvbassine | Data from basketball-reference | As of ', Sys.Date(), sep = ''))
