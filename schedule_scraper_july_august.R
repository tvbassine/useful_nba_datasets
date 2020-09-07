# Schedule Scraper 2020:
# This function is very similar to schedule scraper except it
# gets schedules for the 2020 season. Includes the new July/August
# schedule.

# Schedule Function

sched_2020 <- function(year){
  
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
  
  #This function has the year 2018 hard-coded into it
  months.all <- c("october", "november", "december", "january","february","march","july","august","september")
  
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
  
  return(out)
  
}

# out <- sched_2020(2020)
# head(out)
# tail(out)
# nrow(out)
