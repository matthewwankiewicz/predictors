remotes::install_github("jthomasmock/espnscrapeR")
library(espnscrapeR)
library(tidyverse)
library(rvest)
library(sqldf)

stats <- nflfastR::load_player_stats(2021)
stats <- stats %>% 
  filter(season == 2021)
stats20 <- nflfastR::load_player_stats(2020)
stats19 <- nflfastR::load_player_stats(2019)

stats <- rbind(stats, stats20, stats19)

### GET OFFENSE ------------------------------------------------------------------------------------------

def_pass_team20 <- scrape_team_stats_nfl(season = 2020, 
                                         stats = "passing",
                                         role = "defense")

### GET DEFENSE ------------------------------------------------------------------------------------------

def_pass_team21 <- scrape_team_stats_nfl(season = 2021, 
                                         stats = "passing",
                                         role = "defense")

def_pass_team19 <- scrape_team_stats_nfl(season = 2019, 
                                         stats = "passing",
                                         role = "defense")

### CLEAN DEFENSE ------------------------------------------------------------------------------------------

for(i in 5:ncol(def_pass_team19)){
  colnames(def_pass_team19)[i] <-  paste0("def", colnames(def_pass_team19)[i], sep = "")
}

def_pass_team19 <- def_pass_team19 %>% 
  select(-c(role, stat))


### CLEAN OFFENSE ------------------------------------------------------------------------------------------

for(i in 5:ncol(def_pass_team20)){
  colnames(def_pass_team20)[i] <-  paste0("def", colnames(def_pass_team20)[i], sep = "")
}

def_pass_team20 <- def_pass_team20 %>% 
  select(-c(role, stat))


for(i in 5:ncol(def_pass_team21)){
  colnames(def_pass_team21)[i] <-  paste0("def", colnames(def_pass_team21)[i], sep = "")
}

def_pass_team21 <- def_pass_team21 %>% 
  select(-c(role, stat))


team_stats21 <- rbind(def_pass_team21, def_pass_team20, def_pass_team19, by = c("team", "season"))

team_stats21[team_stats21 == "Football"] <- "Washington"
team_stats21[team_stats21 == "Redskins"] <- "Washington"
team_stats21 <- team_stats21 %>% 
  filter(team != "season")

#### get schedule --------
link <- "https://fixturedownload.com/results/nfl-2021"
page <- read_html(link)
table <- html_table(page)

schedule2021 <- table[[1]]
schedule2021[schedule2021 == "Washington Football Team"] <- "Washington"

schedule2021 <- schedule2021 %>% 
  rename("home" = `Home Team`,
         "visitor" = `Away Team`) %>% 
  separate(Result, into = c("home_score", "visitor_score"), sep = " - ")

schedule2021$home_score <- as.numeric(schedule2021$home_score)
schedule2021$visitor_score <- as.numeric(schedule2021$visitor_score)

## Format home and away names

h <- schedule2021$home
v <- schedule2021$visitor

format_name <- function(name){
  last_space <- last(gregexpr(" ", name)[[1]]) + 1
  substr(name, start = last_space, stop = nchar(name))
}

for(i in 1:length(h)){
  h[i] <- format_name(h[i])
}

for(i in 1:length(v)){
  v[i] <- format_name(v[i])
}

schedule2021$home <- h
schedule2021$visitor <- v

schedule2021 <- schedule2021 %>% 
  select(home, visitor, home_score, visitor_score)

schedule2021 <- schedule2021 %>% 
  mutate(home_win = ifelse(home_score > visitor_score, 1, 0),
         winner = ifelse(home_score > visitor_score, home, visitor))

schedule.gp <- drop_na(schedule2021)

schedule.gp.home <- schedule.gp %>% 
  group_by(home) %>% 
  summarise(n = n())

schedule.gp.away <- schedule.gp %>% 
  group_by(visitor) %>% 
  summarise(n = n())

schedule.gp <- left_join(schedule.gp.home, schedule.gp.away,
                         by = c("home" = "visitor"))

schedule.gp <- schedule.gp %>% 
  mutate(games_played = n.x + n.y) %>% 
  select("team" = home,
         games_played)

### get average and standard deviation ---------

team_stats20 <- team_stats21 %>% 
  filter(season == 2020 | season == 2019)

gp <- rep(16, 64)
team_stats20$games_played <- gp

team_stats21 <- team_stats21 %>% 
  filter(season == 2021)

team_stats21 <- left_join(team_stats21, schedule.gp,
                          by = "team")

team_stats <- rbind(team_stats20, team_stats21)

team_stats$defrush_att <- as.numeric(team_stats$defpass_yds)
team_stats$defrush_att <- as.numeric(team_stats$defrush_att)

team_stats <- team_stats %>% 
  mutate(avg_td = defrush_att/games_played)


av_pass <- mean(team_stats$avg_td)

sd_pass <- sd(team_stats$avg_td)


qnorm(runif(1), mean = avg_rush, sd = sd_rush)
qnorm(runif(1), mean = avg_pass, sd = sd_pass)


team_passing <- team_stats %>% 
  group_by(team) %>% 
  summarise(avg_td = mean(avg_td))

team_passing <- team_passing %>% 
  mutate(def_strength_pass = round(scale(avg_td, center = 1) -1, digits = 3))



### write function to get player avg stats -----------

avg_receiving <- stats %>% 
  group_by(player_name) %>% 
  summarise(avg_rec = mean(receiving_yards),
            sd_rec = sd(receiving_yards)) %>% 
  filter(avg_rec != 0)





get_prediction <- function(player, team, sims, prop){
  player.df <- avg_receiving %>% 
    filter(player_name == player)
  player_average = as.numeric(player.df[1,2])
  player_sd = as.numeric(player.df[1,3])
  
  team_adj <- sqldf(paste("select def_strength_pass[,1] from team_passing where team is '",
                          team, "'", sep = ""))[1,1]
  
  yards <- c()
  for(i in 1:sims){
    yards[i] <- qnorm(runif(1), mean = player_average, sd = player_sd) + 
      qnorm(runif(1), mean = player_average, sd = player_sd)*team_adj
  }
  hist(yards)
  abline(v = prop, col = "green")
  print(paste(mean(yards), sum(yards > prop)/sims))
}

get_prediction("D.Johnson", "Ravens", 10000, 80.5)



stats <- read.csv("models/sportsref_download.csv")

stats <- stats %>% 
  select(-Rk) %>% 
  drop_na()

booker <- read_csv("models/test.csv")

avg_3pt <- mean(stats$AST)

team_3pt <- stats %>% 
  group_by(Team) %>% 
  summarise(avg_3pters = mean(AST))

team_3pt <- team_3pt %>% 
  mutate(def_3_strength = round(avg_3pters/avg_3pt - 1, digits = 3))

avg_receiving <- booker %>% 
  group_by(Player_ID) %>% 
  summarise(avg_rec = mean(AST),
            sd_rec = sd(AST)) %>% 
  filter(avg_rec != 0)


get_prediction <- function(player, team, sims, prop){
  player.df <- avg_receiving %>% 
    filter(Player_ID == player)
  player_average = as.numeric(player.df[1,2])
  player_sd = as.numeric(player.df[1,3])
  
  team_adj <- sqldf(paste("select def_3_strength from team_3pt where team is '",
                          team, "'", sep = ""))[1,1]
  
  yards <- c()
  for(i in 1:sims){
    yards[i] <- qnorm(runif(1), mean = player_average, sd = player_sd) + 
      qnorm(runif(1), mean = player_average, sd = player_sd)*team_adj
  }
  hist(yards)
  abline(v = prop, col = "green")
  return(mean(yards))
}


get_prediction(1626164, "Dallas Mavericks", 10000, 4.5)


### get gamelogs and team stats

library(tidyverse)

gamelogs <- read_csv("models/gamelogs.csv")
team_stats <- read_csv("models/team_stats.csv")
team_stats <- team_stats %>% 
  select(-Rk) %>% 
  drop_na()

## clean gamelogs
gamelogs$GAME_ID <- as.numeric(gamelogs$GAME_ID)

gamelogs <- gamelogs %>% 
  select(-c(...1, TEAM_ID, TEAM_ABBREVIATION, TEAM_CITY, PLAYER_ID, 
            NICKNAME, START_POSITION, COMMENT, MIN)) %>% 
  arrange(desc(GAME_ID))


write_rds(team_stats, "predictors/team_opp_stats.rds")
write_rds(gamelogs, "predictors/gamelogs.rds")

gamelogs %>% 
  filter(PLAYER_NAME == "Bradley Beal") %>% 
  select(REB) %>% 
  slice(1:10)

teams <- teams %>% 
  mutate(strength = round(avg.stat/avg_stat - 1, digits = 3))

get_prediction <- function(player, team, stat, sims, prop){
  # get player stat
  avgs <- gamelogs %>% 
    group_by(PLAYER_NAME) %>% 
    summarise(avg1 = mean({{stat}}, na.rm = T),
              sd1 = sd({{stat}}, na.rm = T)) %>% 
    filter(avg1 != 0)
  
  
  player.df <- avgs %>% 
    filter(PLAYER_NAME == player)
  player_average = as.numeric(player.df[1,2])
  player_sd = as.numeric(player.df[1,3])
  
  
  teams <- team_stats %>% 
    group_by(Team) %>% 
    summarise(avg.stat = mean({{stat}}))
  
  stat_col <- team_stats %>% select(stat)
  avg_stat <- mean(stat_col)
  
  teams <- teams %>% 
    mutate(strength = round(avg.stat/avg_stat - 1, digits = 3))
  
  team_adj <- sqldf(paste("select strength from teams where team is '",
                          team, "'", sep = ""))[1,1]
  
  simmed <- c()
  for(i in 1:sims){
    simmed[i] <- qnorm(runif(1), mean = player_average, sd = player_sd) + 
      qnorm(runif(1), mean = player_average, sd = player_sd)*team_adj
  }
  hist(simmed)
  abline(v = prop, col = "green")
  print(paste("Estimate:", mean(simmed), ". Prob:", sum(simmed > prop)/sims))
}

get_prediction("Reggie Jackson", "Memphis", "FG3M", 10000, 1.5)
