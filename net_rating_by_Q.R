# Thomas Bassine
# November 21, 2021
# Purpose: Pull quarter by quarter net rating for 
# each team and make a pretty table using the 'gt'
# package.

####################################################
####################################################
# Step 1: gather data from nba.com:
# Get quarter by quarter data:

period = c('Period=1',
              'Period=2',
              'Period=3',
              'Period=4')

out <- list()

for(i in 1:4){
  url <- gsub("Period=1", period[i], "https://stats.nba.com/stats/leaguedashteamstats?Conference=&DateFrom=&DateTo=&Division=&GameScope=&GameSegment=&LastNGames=0&LeagueID=00&Location=&MeasureType=Advanced&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PaceAdjust=N&PerMode=PerGame&Period=1&PlayerExperience=&PlayerPosition=&PlusMinus=N&Rank=N&Season=2021-22&SeasonSegment=&SeasonType=Regular+Season&ShotClockRange=&StarterBench=&TeamID=0&TwoWay=0&VsConference=&VsDivision=")
  
  library(tidyverse)
  library(httr)
  
  headers = c(
    `Connection` = 'keep-alive',
    `Accept` = 'application/json, text/plain, */*',
    `x-nba-stats-token` = 'true',
    `X-NewRelic-ID` = 'VQECWF5UChAHUlNTBwgBVw==',
    `User-Agent` = 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_14_6) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/78.0.3904.87 Safari/537.36',
    `x-nba-stats-origin` = 'stats',
    `Sec-Fetch-Site` = 'same-origin',
    `Sec-Fetch-Mode` = 'cors',
    `Referer` = 'https://stats.nba.com/stats/',
    `Accept-Encoding` = 'gzip, deflate, br',
    `Accept-Language` = 'en-US,en;q=0.9'
  )
  
  res <- httr::GET(url = url, httr::add_headers(.headers=headers))
  json_resp <- jsonlite::fromJSON(content(res, "text"))
  
  df <- data.frame(json_resp$resultSets$rowSet)
  colnames(df) <- json_resp$resultSets$headers[[1]]
  
  df$quarter = i
  
  head(df)
  
  out[[i]] = df
  print(i)
  
}

x <- do.call('rbind', out)

####################################################
# Step 2: Combine data into 1 tibble.

# Organize into 1 data_frame:
y = x %>%
    filter(quarter == 1) %>%
    select(TEAM_NAME, NET_RATING) %>%
    rename(Net_Rating_1 = 'NET_RATING')

y = merge(y,
          x %>%
            filter(quarter == 2) %>%
            select(TEAM_NAME, NET_RATING) %>%
            rename(Net_Rating_2 = 'NET_RATING'),
          by = 'TEAM_NAME')
y = merge(y,
          x %>%
            filter(quarter == 3) %>%
            select(TEAM_NAME, NET_RATING) %>%
            rename(Net_Rating_3 = 'NET_RATING'),
          by = 'TEAM_NAME')
y = merge(y,
          x %>%
            filter(quarter == 4) %>%
            select(TEAM_NAME, NET_RATING) %>%
            rename(Net_Rating_4 = 'NET_RATING'),
          by = 'TEAM_NAME')

y$Net_Rating_1 = as.numeric(y$Net_Rating_1)
y$Net_Rating_2 = as.numeric(y$Net_Rating_2)
y$Net_Rating_3 = as.numeric(y$Net_Rating_3)
y$Net_Rating_4 = as.numeric(y$Net_Rating_4)

y$TEAM_NAME[y$TEAM_NAME == 'LA Clippers'] = 'Los Angeles Clippers'

####################################################
# Step 3: Use teamcolors package to get logos.

library( teamcolors)
a = teamcolors %>%
  filter(league == 'nba')

y = merge(y, a[,c('name', 'logo')],
              by.x= 'TEAM_NAME',
              by.y = 'name',
              all.x = T)

####################################################
# Step 4: Make a nice table (and save it) using gt.


# Make a data table:
# A gt table: 
tab = y %>% 
  select(TEAM_NAME, logo, Net_Rating_1, Net_Rating_2, Net_Rating_3, Net_Rating_4 ) %>%
  gt() %>% 
  tab_header(
    title = md("**Who Owns Each Quarter?**"), # Add a title
    subtitle = paste0(
      "Net rating by quarter. | source: nba.com/stats | Games played through ",
      format(Sys.Date() - 1, '%B %d, %Y')) # And a subtitle
  ) %>% 
  cols_label(
    TEAM_NAME = 'Team',
    logo = '',
    Net_Rating_1 = 'Q1 Net Rating',
    Net_Rating_2 = 'Q2 Net Rating',
    Net_Rating_3 = 'Q3 Net Rating',
    Net_Rating_4 = 'Q4 Net Rating',
  )  %>%
  data_color( # Update cell colors...
    columns = c(Net_Rating_1, Net_Rating_2, Net_Rating_3, Net_Rating_4),
    colors = scales::col_bin(
      palette = c(
        "red", "white", "green"),
      bins = seq(-31,31, 1),
      alpha = .4
    )
  ) %>%
  cols_align(
    align = c("center"),
    columns = c(logo,  Net_Rating_1, Net_Rating_2, Net_Rating_3, Net_Rating_4)
  ) %>%
  text_transform(
    locations = cells_body(vars(logo)),
    fn = function(x) {
      web_image(url = x) 
    }
  ) %>% 
  tab_style(
    style = list(
      cell_text(font = "serif")
    ),
    location = list(
      cells_body(columns = gt::everything())
    )
  )  %>%
  tab_options(data_row.padding = px(1),
              table.font.size = 25)

# Save the plot image (change the directory here)
tab %>%
  gtsave(
    paste0('~/Documents/misc_nba_stuff/net_rating_by_Q/net_rating_by_Q_', Sys.Date(), '.png'),
    path = '/Users/thomasbassine/Library/Application Support/PhantomJS'
    #zoom = 7
  )
