# Thomas Bassine
# Date 12/27/21
# Purpose: This code tracks the eFG% of 2P and 3P shots over time. 
# We see (as of December 2021) that the eFG% of 2P has caught up to
# that of 3P, after steadily rising ove the last few seasons.
####################################################################

# Load required packages:
library(rvest)
library(ggplot2)

# Read in league-wide trends data from basketball-reference
url = 'https://www.basketball-reference.com/leagues/NBA_stats_totals.html'

df <- url %>%
  read_html() %>%
  html_table() %>%
  .[[1]]

# The first row are actually the column names.
colnames(df) = df[1,]
df = df[-1,]

# Calculate 2PT and 3PT eFG%
df$FG = as.numeric(df$FG)
df$FGA = as.numeric(df$FGA)
df$`3P` = as.numeric(df$`3P`)
df$`3PA` = as.numeric(df$`3PA`)

df$efg2 = (df$FG - df$`3P`) / (df$FGA - df$`3PA`) 
df$efg3 = 1.5 * (df$`3P`) / (df$`3PA`) 

df = df[1:20,]
df$yr = 2022:2003

# Manipulate our data into one long table to prepare for ggplot.
# Final Cols = yr | efg | Shot
x = df %>%
  select(yr,efg2) %>%
  rename(efg = efg2) %>%
  mutate(Shot = '2P')

x = rbind(x,
          df %>%
            select(yr,efg3) %>%
            rename(efg = efg3) %>%
            mutate(Shot = '3P') )

# Make the ggplot:
p = x %>%
  ggplot( aes(x=yr, y=efg, group=Shot, color=Shot)) +
  geom_line(size = 2) +
  ggtitle("Effective Field Goal % of 2P and 3P Shots") +
  theme_bw() +
  ylab("eFG%") +
  xlab("Year") +
  scale_y_continuous(breaks=seq(0.44,0.56,.02)) +
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
        
  )

# Show plot:
p

# Save the plot (can change directory):
file = paste0("~/Documents/misc_nba_stuff/leaguewide_trends/",
             'efg_2P_vs_3P_', Sys.Date(), '.png')
ggsave(file, dpi = 370,
       height = 5.5, width = 8)

