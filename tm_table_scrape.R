#########################################################
# Team Table Scrape Function
# Purpose: This script will scrape the overall 
# statistics for all teams over a given year.
# It will be useful in many articles to see compare
# how a team's stat or improvement stacks up 
# historically.
# Date: 11/21/18
################################################################

############################ Tm_table_scrape
tm_table_scrape  <- function(yr, id){
  
  
  # Going to write a function that gobbles up an entire table:
  page <- paste('https://www.basketball-reference.com/leagues/NBA_',
                yr, '.html', sep = '')
  # Read page
  j <- scan(page, what ="", sep = "\n")
  
marker <- grep(id, j)
start <- grep('data-stat=\"ranker\" csk=\"1', j)
start <- start[which(start > marker)[1]]
end <- grep('data-stat=\"ranker\" csk=\"30', j)
end <- end[which(end > marker)[1]]
mat <- j[start:end]

dat <- list()

for(j in 1:length(mat)){
  
  out <- unlist(strsplit(mat[j], split = 'data-stat=\"'))
  out <- out[grep('team_name', out): length(out)]
  
  col_names <- c()
  statz <- c()
  
  for(i in 1:length(out)){
    if(grepl('team_name', out[i])){
      nm <- 'tm'
      temp <- unlist(strsplit(out[i], split = '>'))[3]
      temp <- unlist(strsplit(temp, split = '<'))[1]
      col_names = c(col_names, nm)
      statz = c(statz, temp)
      
    } else{
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
for(j in 2:ncol(out)){
  out[,j] <- as.numeric(out[,j])
}

out$yr <- yr

return(out)

}



