library(rvest)
library(tidyverse)

# hockey-ref
link <- "https://www.espn.com/nhl/stats/team"
table <- read_html(link) %>% 
  html_table()

table[[2]]

teams <- table[[1]]$Team
stats <- table[[2]]
stats$Team <- teams


stats[29, 15] <- "Montréal Canadiens"

# schedule

link <- "https://fixturedownload.com/results/nhl-2021"
table <- read_html(link) %>% 
  html_table()
schedule21 <- table[[1]]

schedule21 <- schedule21 %>% 
rename("home" = `Home Team`,
       "visitor" = `Away Team`) %>% 
  separate(Result, into = c("home_score", "visitor_score"), sep = " - ")

for(i in 1:nrow(schedule21)){
  if(schedule21[i, 6] == "-"){
    schedule21[i, 6] = NA
  }
}

schedule21$home_score <- as.numeric(schedule21$home_score)
schedule21$visitor_score <- as.numeric(schedule21$visitor_score)



schedule21 <- schedule21 %>% 
  select(home, visitor, home_score, visitor_score)

schedule21 <- schedule21 %>% 
  mutate(home_win = ifelse(home_score > visitor_score, 1, 0))

#### get 2020 schedule

link <- "https://fixturedownload.com/results/nhl-2020"
table <- read_html(link) %>% 
  html_table()
schedule20 <- table[[1]]

schedule20 <- schedule20 %>% 
  rename("home" = `Home Team`,
         "visitor" = `Away Team`) %>% 
  separate(Result, into = c("home_score", "visitor_score"), sep = " - ")


schedule20 <- drop_na(schedule20)

schedule20$home_score <- as.numeric(schedule20$home_score)
schedule20$visitor_score <- as.numeric(schedule20$visitor_score)

schedule20 <- schedule20 %>% 
  select(home, visitor, home_score, visitor_score)

schedule20 <- schedule20 %>% 
  mutate(home_win = ifelse(home_score > visitor_score, 1, 0))

schedules <- rbind(schedule20, schedule21)

## join data

home_data <- left_join(schedules, stats, by = c("home" = "Team"))
full_data <- left_join(home_data, stats, by = c("visitor" = "Team"))


## save the file

write_rds(full_data, "models/nhl_stats.rds")
