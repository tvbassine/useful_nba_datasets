####################################################
# This script simply scraped the team stats for each 
# team from 2005 to 2018 form bball reference.
####################################################

setwd("~/Desktop/Threes and Layups Articles/Useful Things")
source('tm_table_scrape.R')
years <- seq(2005, 2020,1)
tab <- list()

for(i in 1:length(years)){
  
  yr <- years[i]
misc <- tm_table_scrape(yr = yr, id = 'misc_stats_link')
opp_per_poss <- tm_table_scrape(yr = yr, id ='opponent-stats-per_poss_link')
tm_per_poss <- tm_table_scrape(yr = yr, id = 'team-stats-per_poss_link')
opp_shoot <- tm_table_scrape(yr = yr, id ='opponent_shooting_link')
tm_shoot <- tm_table_scrape(yr = yr, id ='team_shooting_link')

misc <- merge(misc, tm_per_poss, by = 'tm')
misc <- merge(misc, opp_per_poss, by = 'tm')
misc <- merge(misc, tm_shoot, by = 'tm')
misc <- merge(misc, opp_shoot, by = 'tm')

tab[[i]] <- misc
print(yr)
}

tm_stats <- do.call('rbind', tab)

write.csv(tm_stats, 'tm.data.05.20.csv', row.names = F)
