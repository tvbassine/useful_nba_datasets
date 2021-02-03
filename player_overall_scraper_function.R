#########################################################
# Player  Overall Statistics Scraper
# Purpose: This script will scrape the overall 
# statistics for all players over a given year.
# It will be useful in many articles to see compare
# how a player's stat or improvement stacks up 
# historically.
# Date: 12/02/18
##########################################################

pl_overall_scrape  <- function(yr, id, 
                               line_id = 'data-stat=\"ranker\" csk='){
  
  
  # Going to write a function that gobbles up an entire table:
  page <- paste('https://www.basketball-reference.com/leagues/NBA_', yr,
                '_', id,  '.html', sep = '')
  # Read page
  j <- scan(page, what ="", sep = "\n")
  
  mat <- j[grep(line_id, j)]
  
  mat <- gsub('<strong>', '', mat)
  mat <- gsub('</strong>', '', mat)
  
  dat <- list()
  
  for(j in 1:length(mat)){
    
    out <- unlist(strsplit(mat[j], split = 'data-stat=\"'))
    out <- out[2: length(out)]
    
    col_names <- c()
    statz <- c()
    
    for(i in 1:length(out)){
      # Building code for a couple scenarios:
      if(grepl('team_id', out[i]) | grepl('season', out[i]) | 
         grepl('lg_id', out[i])){
        out[i] <- gsub('\"', '', out[i])
        out[i] <- gsub(' ', '', out[i])
        temp <- unlist(strsplit(out[i], split = '>'))
        nm <- temp[1]
        temp <- unlist(strsplit(temp[3], split = '<'))[1]
        col_names = c(col_names, nm)
        statz = c(statz, temp)
        }
      if(grepl('data-append-csv', out[i])){
        out[i] <- gsub('\"', '', out[i])
        out[i] <- gsub(' ', '', out[i])
        temp <- unlist(strsplit(out[i], split = 'data-append-csv='))[2]
        temp <- trimws(temp)
        col_names = c(col_names, 'id')
        statz = c(statz, temp)
      }
      if(grepl('/players/', out[i])){
        out[i] <- gsub('\"', '', out[i])
        temp <- unlist(strsplit(out[i], split = 'csk='))[2]
        temp <- unlist(strsplit(temp, split = '>'))[1]
        temp <- trimws(temp)
        col_names = c(col_names, 'name')
        statz = c(statz, temp)
      }
      
      if((grepl('team_id', out[i]) | grepl('season', out[i]) | 
         grepl('lg_id', out[i]) | grepl('data-append-csv', out[i]) |
         grepl('/players/', out[i])) == F){
        out[i] <- gsub('\"', '', out[i])
        out[i] <- gsub(' ', '', out[i])
        temp <- unlist(strsplit(out[i], split = '>'))
        nm <- temp[1]
        temp <- unlist(strsplit(temp[2], split = '<'))[1]
        col_names = c(col_names, nm)
        statz = c(statz, temp)
      }
      
    }
    
    
    dat[[j]] <- data.frame(t(statz), stringsAsFactors = F)
    colnames(dat[[j]]) <- col_names
    
  }
  
  out <- do.call('rbind', dat)
  for(j in c(4,6:ncol(out)) ){
    out[,j] <- as.numeric(out[,j])
  }
  
  
  return(out)
  
}

#out = pl_overall_scrape(yr = 2020, id = 'totals')
#head(out)
#tail(out,10)

#write.csv()
