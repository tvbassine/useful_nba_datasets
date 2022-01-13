# Purpose: Make a pretty table of team/opponent shot distribution.
# Date: 1/12/22
# Author: Thomas Bassine
###################################################################



library(rvest)
library(dplyr)

# Read in data using rvest::read_html
url_basic = 'https://www.basketball-reference.com/leagues/NBA_2022.html'


df <- url_basic %>%
    read_html() %>%
    html_table() 

# Get shot distribution:
  x = df[[13]] #Use 12 here for team shot charts and 13 for opponent shot charts
  colnames(x) = x[1,]
  x = x[-c(1,32),]
  x = x[,1:13]
  x$`0-3` = as.numeric(x$`0-3`)
  x$`3-10` = as.numeric(x$`3-10`)
  x$`10-16` = as.numeric(x$`10-16`)
  x$`16-3pt` = as.numeric(x$`16-3pt`)
  x$`3P` = as.numeric(x$`3P`)
  
  x$rim = x$`0-3`
  x$mid = x$`3-10` + x$`10-16` + x$`16-3pt`
  x$three = x$`3P`
  
# Get FG%
  y = df[[13]] #Use 12 here for team shot charts and 13 for opponent shot charts
  colnames(y) = y[1,]
  y = y[-c(1,32),] 
  y = y[,c(2,16:20)]
  y$rim_accuracy = as.numeric(y$`0-3`)
  y$three_accuracy = as.numeric(y$`3P`)
  y$mid_accuracy = ((as.numeric(y$`3-10`) * x$`3-10`) + 
                   (as.numeric(y$`10-16`) * x$`10-16`) +
                   (as.numeric(y$`16-3pt`) * x$`16-3pt`)) /
                   (x$mid)
  y$mid_accuracy = round(y$mid_accuracy,3)
  
w = merge(x[,c('Team','rim','mid','three')],
          y[,c('Team','rim_accuracy','mid_accuracy','three_accuracy')],
          by = 'Team')

############################################
# Get team logos:
library(teamcolors)
a = teamcolors %>%
  filter(league == 'nba')

z = merge(w, a[,c('name', 'logo')],
          by.x= 'Team',
          by.y = 'name',
          all.x = T)

############################################
library(gt)
# Make gt table: 
tab = z %>% 
  select(Team, logo, rim, rim_accuracy, 
         mid, mid_accuracy,
         three, three_accuracy ) %>%
  arrange(-1*rim) %>%
  gt() %>% # Make a gt table with it
  tab_header(
    title = md("**Opponent Shooting Profiles**"), # Add a title
    subtitle = paste0(
      "plot by @tvbassine | source : basketball-reference | Games played through ",
      format(Sys.Date() - 1, '%B %d, %Y')) # And a subtitle
  ) %>% 
  cols_label(
    logo = '',
    rim = 'Rim Att. Rate',
    mid =  "Mid Att. Rate",
    three = "3P Att. Rate",
    rim_accuracy = 'Rim FG%',
    mid_accuracy =  "Mid FG%",
    three_accuracy = "3P%",
  )  %>%
  fmt_percent(
    columns = c(rim, rim_accuracy, 
                mid, mid_accuracy,
                three, three_accuracy),
    decimals = 1
  ) %>%
  data_color( # Update cell colors...
    columns = c(rim),
    colors = scales::col_bin(
      palette = c(
        "white", "purple"),
      bins = seq(min(z$rim) - 0.01, max(z$rim) + 0.01, 0.01),
      alpha = .4
    )
  ) %>%
  data_color( # Update cell colors...
    columns = c(mid),
    colors = scales::col_bin(
      palette = c(
        "white", "purple"),
      bins = seq(min(z$mid) - 0.01, max(z$mid) + 0.01, 0.01),
      alpha = .4
    )
  ) %>%
  data_color( # Update cell colors...
    columns = c(three),
    colors = scales::col_bin(
      palette = c(
        "white", "purple"),
      bins = seq(min(z$three) - 0.01, max(z$three) + 0.01, 0.01),
      alpha = .4
    )
  ) %>%
  data_color( # Update cell colors...
    columns = c(rim_accuracy),
    colors = scales::col_bin(
      palette = c(
        "white", "orange"),
      bins = seq(min(z$rim_accuracy) - 0.01, max(z$rim_accuracy) + 0.01, 0.01),
      alpha = .4
    )
  ) %>%
  data_color( # Update cell colors...
    columns = c(mid_accuracy),
    colors = scales::col_bin(
      palette = c(
        "white", "orange"),
      bins = seq(min(z$mid_accuracy) - 0.01, max(z$mid_accuracy) + 0.01, 0.01),
      alpha = .4
    )
  ) %>%
  data_color( # Update cell colors...
    columns = c(three_accuracy),
    colors = scales::col_bin(
      palette = c(
        "white", "orange"),
      bins = seq(min(z$three_accuracy) - 0.01, max(z$three_accuracy) + 0.01, 0.01),
      alpha = .4
    )
  ) %>%
  cols_align(
    align = c("center"),
    columns = c(rim, rim_accuracy, 
                mid, mid_accuracy,
                three, three_accuracy)
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
        columns = vars(rim, mid, three)
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

tab %>%
  gtsave(
    paste0('~/Desktop/opp_shot_brekdown_', Sys.Date(), '.png'),
    path = '/Users/thomasbassine/Library/Application Support/PhantomJS'
    #zoom = 7
  )

