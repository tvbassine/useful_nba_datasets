# Team Life Cycles 
# 2011-2020
################################################################

# Read in data:
x <- read.csv("https://raw.githubusercontent.com/tvbassine/useful_nba_datasets/master/tm.data.05.20.csv",
              stringsAsFactors = F)
x <- x[x$yr >= 2011,]

# Name massaging:
x$tm[x$tm == 'New Jersey Nets'] <- 'Brooklyn Nets'
x$tm[x$tm == "New Orleans/Oklahoma City Hornets"] <- "New Orleans Pelicans"
x$tm[x$tm == "New Orleans Hornets"] <- "New Orleans Pelicans"
x$tm[x$tm == "Seattle SuperSonics"] <- "Oklahoma City Thunder"
x$tm[x$tm == "Charlotte Bobcats"] <- "Charlotte Hornets"

head(x)
tail(x)

x$net <- x$off_rtg - x$def_rtg
summary(x$net)
summary(x$age)
max_age <- max(x$age)
min_age <- min(x$age)
ran_age <- max_age - min_age

x$age_cex <- 2 * (x$age - min_age + .1) / ran_age


# Get teams alphabetically: 
tms <- unique(x$tm)
tms <- tms[order(tms)]

# Set plotting window and margins:
par(mfrow = c(3, 10))
par(mar = c(.1,.1,1.5,.1))

# Plot each of the 30 teams:
for(i in 1:30){
  tm_now <- tms[i]
  
  w <- x[x$tm %in% tm_now,]
  
  plot(w$yr, w$net, type = 'b', pch = 19,
       cex = w$age_cex,
       ylim = c(-15.3,12), col = 'blue',
       main = w$tm[1],
       xlab = '', ylab = '', xaxt = 'n',
       yaxt = 'n', cex.main = .85)
  abline(h = 0, lty = 2)
}

# How many +6 seasons in last 10 years?
z <- aggregate(net ~ tm, data = x, function(j) sum(j>=6))
View(z)

