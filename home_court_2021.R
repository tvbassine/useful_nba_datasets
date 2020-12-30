# 2021 Schedule Scraper
# Date: 12/30/2020
# Purpose: Scrape the 2020-2021 NBA schedule from basketball-reference.
# Determine how home court advantage stacks up to previous seasons.
# Created By: Thomas Bassine


sched_2021 <- function(year){
  
  tms.away <- c()
  tms.home <- c()
  pts.away <- c()
  pts.home <- c()
  da <- c()
  
  get.stat <- function(nm, linee){
    temp <- unlist(strsplit(linee, split = nm))[2]
    temp <- unlist(strsplit(temp, split = "<"))[1]
    temp <- unlist(strsplit(temp, split = ">"))[2]
    temp <- try(as.numeric(temp))
    return(list(out = temp))
  }
  
  get.date <- function(nm, linee){
    temp <- unlist(strsplit(linee, split = nm))[2]
    temp <- unlist(strsplit(temp, split = "<"))[1]
    temp <- unlist(strsplit(temp, split = ">"))[2]
    #temp <- try(as.numeric(temp))
    return(list(out = temp))
  }
  
  get.tm <- function(linee, away){
    if(away == T){
      temp <- unlist(strsplit(linee, split = "/teams/"))[2]
    } else {temp <- unlist(strsplit(linee, split = "/teams/"))[3]}
    temp <- unlist(strsplit(temp, split = "<"))[1]
    temp <- unlist(strsplit(temp, split = ">"))[2]
    
    return(list(out = temp))
  }
  
  # This function will spit out the remaining games(home and away team)
  # month tells it where to start
  
  # This part has the months for the 2020-2021 hard-coded into it
  months.all <- c("december", "january","february","march")
  
  for(mon in months.all){
    
    file <- paste("https://www.basketball-reference.com/leagues/NBA_", year, "_games-",
                  mon, ".html", sep = "")
    pg <- scan(file, what="", sep="\n")
    
    
    start <- grep("Schedule</h2>",pg)
    end <- grep("</tbody></table>",pg)
    
    # if(mon == 'april'){
    #   end <- grep('colspan=\'10\'>Playoffs</th>', pg)
    # }
    
    pg <- pg[start:end]
    pg <- pg[grep("csk=\"20",pg)] #Might need this: pg <- pg[grep("csk=\"201",pg)]
    
    
    pts.away <- c(pts.away, unlist(lapply(pg, get.stat, nm = "visitor_pts")))
    pts.home <- c(pts.home, unlist(lapply(pg, get.stat, nm = "home_pts")))
    
    #rem.ind <- which(is.na(temp))
    #rem.ind <- min(rem.ind)
    
    
    tms.away <- c(tms.away, unlist(lapply(pg, get.tm, away = T)))
    tms.home <- c(tms.home, unlist(lapply(pg, get.tm, away = F)))
    
    da <- c(da, unlist(lapply(pg, get.date, nm = "year")))
    
    print(mon)
    
  }
  
  out <- data.frame(away = tms.away, home = tms.home,
                    a.sc = pts.away, h.sc = pts.home,
                    date = da,
                    stringsAsFactors = F)
  
  
  # Next steps: Make dates into proper R date format.
  
  out$dateR <- out$da
  for(i in 1:nrow(out)){
    temp <- unlist(strsplit(out$dateR[i], split = ','))
    temp <- trimws(temp)
    temp2 <- unlist(strsplit(temp[2], split = ' '))
    out$dateR[i] <- paste(temp2[1], temp2[2], temp[3], sep = '-')
  }
  
  out$dateR <- as.Date(out$dateR, format = '%b-%d-%Y')
  
  # Next steps: Identify games yet to be played. (i.e., NA's for score)
  
  out$yet_to_play <- 0
  out$yet_to_play[is.na(out$a.sc)] <- 1
  
  # Identify back to backs for home and road team
  
  # Kind of dumb, but here goes
  out$away_bb <- 0
  
  for(i in 1:nrow(out)){
    ind <- which((out$away == out$away[i]) | (out$home == out$away[i]))
    datediff <- out$dateR[i] - out$dateR[ind]
    if(c(1) %in% datediff){
      out$away_bb[i] <- 1
    }
  }
  
  out$home_bb <- 0
  
  for(i in 1:nrow(out)){
    ind <- which((out$away == out$home[i]) | (out$home == out$home[i]))
    datediff <- out$dateR[i] - out$dateR[ind]
    if(c(1) %in% datediff){
      out$home_bb[i] <- 1
    }
  }
  
  out$year <- year
  out$h_margin <- out$h.sc - out$a.sc
  out$away_year <- paste(out$away, out$year, sep = '_')
  out$home_year <- paste(out$home, out$year, sep = '_')
  
  return(out)
  
}

# Get 2021 schedule and results.
out_2021 <- sched_2021(2021)

# Read in results for previous seasons:
x <- read.csv('https://raw.githubusercontent.com/tvbassine/useful_nba_datasets/master/schedule_data_2007_2020',
              stringsAsFactors = F)

# Combine the data into one dataset with all results from 2007-2021.
x <- rbind(x, out_2021)

# Get average home margin of victory by year
y <- aggregate(h_margin ~ year, data = x,
               function(j) mean(j, na.rm = T))
y$labels <- paste(y$year - 1, '-', y$year - 2000, sep = '')

library(ggplot2)
ggplot(y[y$year >= 2013,], aes(x=year, y = h_margin)) + 
  geom_line(color = 'red') + geom_point() + 
  ylab('Home Per Game Margin of Victory') + xlab('Season') +
  scale_x_continuous(breaks = seq(2013,2021), labels = y$labels[6:14]) +
  ggtitle('Home Sweet Home Without Fans?',
          subtitle = paste('Plot by @tvbassine | Data from basketball-reference | As of ', Sys.Date(), sep = '')) + 
  theme(plot.title = element_text(lineheight=.8, face="bold")) 
