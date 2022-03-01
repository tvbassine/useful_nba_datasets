# Thomas Bassine
# Feb 27, 2022
# Code for creating a table of average height, weight and draft position for all
# teams in 2022.
###############################################################################

# 1) Load in the data from nba.com:
library(tidyverse)
library(httr)
library(dplyr)

yrs = 2000:2022

url_base_bio = 'https://stats.nba.com/stats/leaguedashplayerbiostats?College=&Conference=&Country=&DateFrom=&DateTo=&Division=&DraftPick=&DraftYear=&GameScope=&GameSegment=&Height=&LastNGames=0&LeagueID=00&Location=&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PerMode=Totals&Period=0&PlayerExperience=&PlayerPosition=&Season=2000-01&SeasonSegment=&SeasonType=Regular+Season&ShotClockRange=&StarterBench=&TeamID=0&VsConference=&VsDivision=&Weight='
url_base_min = 'https://stats.nba.com/stats/leaguedashplayerstats?College=&Conference=&Country=&DateFrom=&DateTo=&Division=&DraftPick=&DraftYear=&GameScope=&GameSegment=&Height=&LastNGames=0&LeagueID=00&Location=&MeasureType=Base&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PaceAdjust=N&PerMode=Totals&Period=0&PlayerExperience=&PlayerPosition=&PlusMinus=N&Rank=N&Season=2021-22&SeasonSegment=&SeasonType=Regular+Season&ShotClockRange=&StarterBench=&TeamID=0&TwoWay=0&VsConference=&VsDivision=&Weight='


out = list()
count = 1

# Pull the bio data and minutes played for each year from 2000-2022 (we only need 2022 though)
for(i in 1:length(yrs)){
  
  yr = yrs[i]
  seg = paste0(yr - 1, '-', substr(yr, 3, 4))
  
  url = gsub('2000-01', seg, url_base_bio)
  
  headers = c(
    `Connection` = 'keep-alive',
    `Accept` = 'application/json, text/plain, */*',
    `x-nba-stats-token` = 'true',
    `X-NewRelic-ID` = 'VQECWF5UChAHUlNTBwgBVw==',
    `User-Agent` = 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_14_6) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/78.0.3904.87 Safari/537.36',
    `x-nba-stats-origin` = 'stats',
    `Sec-Fetch-Site` = 'same-origin',
    `Sec-Fetch-Mode` = 'cors',
    `Referer` = 'https://stats.nba.com/',
    `Accept-Encoding` = 'gzip, deflate, br',
    `Accept-Language` = 'en-US,en;q=0.9'
  )
  
  res <- httr::GET(url = url, httr::add_headers(.headers=headers))
  json_resp <- jsonlite::fromJSON(content(res, "text"))
  
  df <- data.frame(json_resp$resultSets$rowSet)
  colnames(df) <- json_resp$resultSets$headers[[1]]
  df$yr = yr
  
  # Get minutes played:
  url = gsub('2021-22', seg, url_base_min)
  
  res <- httr::GET(url = url, httr::add_headers(.headers=headers))
  json_resp <- jsonlite::fromJSON(content(res, "text"))
  
  df2 <- data.frame(json_resp$resultSets$rowSet)
  colnames(df2) <- json_resp$resultSets$headers[[1]]
  df2$yr = yr
  
  # Merge bio info with minutes played:
  z = merge(df, df2, by = 'PLAYER_ID', all.x = T, all.y = T)
  
  out[[count]] = z
  count = count +1
  print(seg)
}

# Combine data together:
x = do.call('rbind', out)
summary(x)

# Convert colums we want to numbers (they were strings)
x$PLAYER_HEIGHT_INCHES = as.numeric(x$PLAYER_HEIGHT_INCHES)
x$DRAFT_ROUND = as.numeric(x$DRAFT_ROUND)
x$DRAFT_NUMBER = as.numeric(x$DRAFT_NUMBER)
x$draft_pick = if_else(is.na(x$DRAFT_NUMBER), 61, x$DRAFT_NUMBER)
x$PLAYER_WEIGHT = as.numeric(x$PLAYER_WEIGHT)
x$MIN = as.numeric(x$MIN)

###################################################################
# 2) Do the computations for average height, weight, draft pick
y = x %>%
    filter(yr.x == 2022) %>%
    mutate(height_multiple = PLAYER_HEIGHT_INCHES * MIN,
           weight_multiple = PLAYER_WEIGHT * MIN,
           draft_multiple = draft_pick * MIN) %>% 
    select(TEAM_ABBREVIATION.x,
           height_multiple,
           weight_multiple,
           draft_multiple,
           MIN)
out = data.frame(tm = unique(y$TEAM_ABBREVIATION.x),
                 avg_height = 0,
                 avg_weight = 0,
                 avg_draft = 0)
for(i in 1:nrow(out)){
  temp = y[y$TEAM_ABBREVIATION.x == out$tm[i],]
  out$avg_height[i] = sum(temp$height_multiple) / sum(temp$MIN)
  out$avg_weight[i] = sum(temp$weight_multiple) / sum(temp$MIN)
  out$avg_draft[i] = sum(temp$draft_multiple) / sum(temp$MIN)
}

out$avg_height = round(out$avg_height, 1)
out$avg_weight = round(out$avg_weight, 1)
out$avg_draft = round(out$avg_draft, 1)


###################################################################
# 3) Make gt table:
library(gt)

tab = out %>% 
  select(tm, avg_height, avg_weight, avg_draft) %>%
  arrange(avg_draft) %>% # Sort by draft pick
  gt() %>% 
  tab_header(
    title = md("**Height, Weight, and Draft Position By Team**"), # Add a title
    subtitle = paste0(
      "Averages weighted by minutes played. | source : stats.nba.com | Games played through ",
      format(Sys.Date() - 1, '%B %d, %Y')) 
  ) %>% 
  cols_label(
    tm = 'Team',
    avg_height = 'Average Height (inches)', 
    avg_weight = 'Average Weight (lbs)', 
    avg_draft = 'Average Draft Position'
  )  %>%
  data_color( # Colors for height
    columns = c(avg_height),
    colors = scales::col_bin(
      palette = c(
        "orange", "white", "purple"),
      bins = seq(min(out$avg_height) - 0.1, max(out$avg_height) + 0.1, .1),
      alpha = .2
    )
  ) %>%
  data_color( # Colors for weight
    columns = c(avg_weight),
    colors = scales::col_bin(
      palette = c(
        "orange", "white", "purple"),
      bins = seq(min(out$avg_weight) - 0.1, max(out$avg_weight) + 0.1, .1),
      alpha = .2
    )
  ) %>%
  data_color( # Colors for draft
    columns = c(avg_draft),
    colors = scales::col_bin(
      palette = c(
        "orange", "white", "purple"),
      bins = seq(min(out$avg_draft) - 0.1, max(out$avg_draft) + 0.1, .1),
      alpha = .2
    )
  ) %>%
  cols_align(
    align = c("center"),
    columns = gt::everything()
  ) %>%
  tab_style(
    style = list(
      cell_text(font = "serif")
    ),
    location = list(
      cells_body(columns = gt::everything())
    )
  )  %>%
  tab_style(
    style = list(
      cell_borders(
        sides = "left",
        color = "black",
        weight = px(3)
      )
    ),
    locations = list(
      cells_body(
        columns = vars(avg_height, avg_weight, avg_draft)
      )
    )
  ) %>%
  tab_style(
    style = list(
      cell_borders(
        sides = "bottom",
        color = "black",
        weight = px(3)
      )
    ),
    locations = list(
      cells_column_labels(
        columns = gt::everything()
      )
    )
  ) %>%
  tab_options(data_row.padding = px(1),
              table.font.size = 25)
tab

# Save table (can change directory and save wherever):
tab %>%
  gtsave(
    paste0('~/Documents/misc_nba_stuff/bio_analysis/avg_height_weight_draft_table', Sys.Date(), '.png'),
    path = '/Users/thomasbassine/Library/Application Support/PhantomJS'
    #zoom = 7
  )
