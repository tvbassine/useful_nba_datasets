ref = 'd/doncilu01'
col = '#002B5E'
nm = 'Luka Doncic'
yr = '2022'

library(dplyr)
library(rvest)
library(tidyr)

url = paste0('https://www.basketball-reference.com/players/', ref, '/gamelog/', yr)
df <- url %>%
  read_html() %>%
  html_table() 
x = df[[8]]


x$TRB = as.numeric(x$TRB)
x = x[!is.na(x$TRB),]
x$mp_num = 0
for(i in 1:nrow(x)){
  temp = unlist(strsplit(x$MP[i], split = ':'))
  x$mp_num[i] = as.numeric(temp[1]) + (as.numeric(temp[2]) / 60)
}

x$FG = as.numeric(x$FG)
x$PTS = as.numeric(x$PTS)
x$FGA = as.numeric(x$FGA)
x$FTA = as.numeric(x$FTA)
x$`3PA` = as.numeric(x$`3PA`)
x$`3P` = as.numeric(x$`3P`)
x$`2PA` = x$FGA - x$`3PA`
x$`2P` = x$FG - x$`3P`

x$ts_pct_cum = 0
x$points_cum = 0
x$two_pt_pct_cum = 0
x$thre_pt_pct_cum = 0
x$G = as.numeric(x$G)

for(i in 5:nrow(x)){
  temp = x[(i-4):i,]
  x$ts_pct_cum[i] = sum(temp$PTS) / (2*(sum(temp$FGA) + .44* sum(temp$FTA)))
  x$points_cum[i] = 36 * sum(temp$PTS) / sum(temp$mp_num)
  x$two_pt_pct_cum[i] = sum(temp$`2P`) / sum(temp$`2PA`)
  x$thre_pt_pct_cum[i] = sum(temp$`3P`) / sum(temp$`3PA`)
}


library(ggplot2)

# True Shooting Plot
ggplot(x[x$G >=5,], aes(x=G, y=ts_pct_cum)) +
  geom_line(color = col, size = 2) +
  labs(x = 'Game Played') +
  labs(y = 'TS% Over Last 5 Games') +
  ylim(c(.4,.75)) +
  theme_bw() +
  scale_x_continuous(breaks=seq(5,max(x$G), 4)) + 
  ggtitle(paste0(nm,' 5 Game Rolling Average of TS%'),
          subtitle = 'TS% over 5 game intervals. | source: basketball-reference') +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 10, hjust = 0.5),
    axis.title.x = element_text( size=12),
    axis.title.y = element_text(size=14),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(color = 'white') )


#PTS per 36 MP Plot
ggplot(x[x$G >=5,], aes(x=G, y=points_cum)) +
  geom_line(color = col, size = 2) +
  labs(x = 'Game Played') +
  labs(y = 'PTS per 36 MP Last 5 Games') +
  ylim(c(10,50)) +
  theme_bw() +
  scale_x_continuous(breaks=seq(5,max(x$G), 4)) + 
  ggtitle(paste0(nm, ' 5 Game Rolling Average of PTS Per 36 MP'),
          subtitle = 'PTS Per 36 MP over last 5 games. | source: basketball-reference') +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.subtitle = element_text( hjust = 0.5),
    axis.title.x = element_text( size=12),
    axis.title.y = element_text(size=14),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(color = 'white') ) 






