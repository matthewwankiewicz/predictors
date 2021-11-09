library(tidyverse)
library(rvest)

link <- "https://cleaningtheglass.com/stats/league/fourfactors/"
page <- read_html(link)
table <- page %>% html_table(fill = T)
df20 <- table[[1]]

# reformat headers

test20 <- df20 %>%
  row_to_names(row_number = 1) %>% 
  clean_names()

# select proper columns

data20 <- test20 %>% 
  select(team_2, w, l, pts_poss_2, e_fg_percent_2, tov_percent_2, orb_percent_2, 
         ft_rate_2, pts_poss_4, e_fg_percent_4, tov_percent_4, orb_percent_4, ft_rate_4)

# rename columns

data20 <- data20 %>% 
  rename("Team" = team_2,
         "W" = w,
         "L" = l,
         "pts_poss_off" = pts_poss_2,
         "efg_percent_off" = e_fg_percent_2,
         "tov_percent_off" = tov_percent_2,
         "orb_percent_off" = orb_percent_2,
         "ft_rate_off" = ft_rate_2,
         "pts_poss_def" = pts_poss_4,
         "efg_percent_def" = e_fg_percent_4,
         "tov_percent_def" = tov_percent_4,
         "orb_percent_def" = orb_percent_4,
         "ft_rate_def" = ft_rate_4)




