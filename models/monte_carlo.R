library(tidyverse)
library(rvest)


#### Getting Data #####
  
link <- "https://www.nba.com/stats/teams/traditional/?sort=W_PCT&dir=-1"
page <- read_html(link)
table <- html_table()

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

schedule2021 %>% head()
schedule2021 <- schedule2021 %>% 
  drop_na()

all_teams <- unique(c(schedule2021$visitor, schedule2021$home))

all_team_stats <- all_teams %>% 
  
  map_dfr(function(team, schedule2021){
    
    home_games <- schedule2021$home == team
    away_games <- schedule2021$visitor == team
    
    points_for <- mean(
      c(schedule2021$home_score[home_games],
        schedule2021$visitor_score[away_games])
    )
    
    points_for_sd <- sd(
      c(schedule2021$home_score[home_games],
        schedule2021$visitor_score[away_games])
    )
    
    points_against <- mean(
      c(schedule2021$visitor_score[home_games],
        schedule2021$home_score[away_games])
    )
    
    data.frame(
      team = team,
      pf = points_for,
      pf_sd = points_for_sd,
      pa = points_against,
      n_games = sum(c(home_games,away_games))
    )
  }, schedule2021)

#### Example of the simulation ####
## Get Pts For, Pts Against, and SD of Pts For, for the two teams of interest

team1 <- "Raiders"
team2 <- "Giants"

tm1_pf <- all_team_stats[all_team_stats$team == team1, "pf"]
tm1_pa <- all_team_stats[all_team_stats$team == team1, "pa"]
tm1_sd_pf <- all_team_stats[all_team_stats$team == team1, "pf_sd"]

tm2_pf <- all_team_stats[all_team_stats$team == team2, "pf"]
tm2_pa <- all_team_stats[all_team_stats$team == team2, "pa"]
tm2_sd_pf <- all_team_stats[all_team_stats$team == team2, "pf_sd"]

## adjust points for for both teams based on opponent points against

tm1_adj_pts <- mean(c(tm1_pf, tm2_pa))
tm2_adj_pts <- mean(c(tm2_pf, tm1_pa))

## Let's get some simulated scores for team 1 and team 2

# simulated score for tm1
qnorm(
  runif(1),
  mean = tm1_adj_pts,
  sd = tm1_sd_pf)

# simulated score for tm2
qnorm(
  runif(1),
  mean = tm2_adj_pts,
  sd = tm2_sd_pf)


## Run the simulation 10,000 times to get probability that one team beats the other
N <- 1e4
outcome <- vector("character", length = N)

for(i in 1:N){
  
  d <- qnorm(runif(1), mean = tm1_adj_pts, sd = tm1_sd_pf) - 
    qnorm(runif(1), mean = tm2_adj_pts, sd = tm2_sd_pf)
  
  d <- ifelse(d > 0, team1, team2)
  
  outcome[i] <- d
  
}

table(outcome)
prop.table(table(outcome))
barplot(prop.table(table(outcome)))

## Run simulation and get the estimated point spread of the game

## make a function to simulate a single game

simulate_game <- function( tm1_mean, tm1_sd, tm2_mean, tm2_sd){
  
  tm1 <- qnorm(runif(1), mean = tm1_mean, sd = tm1_sd)
  tm2 <- qnorm(runif(1), mean = tm2_mean, sd = tm2_sd)
  
  data.frame(
    tm1 = tm1,
    tm2 = tm2,
    point_diff = tm1 - tm2,
    winner = ifelse(tm1 > tm2, "tm1", "tm2")
  )
}

## Run sumulation N times (10000)
N <- 1e4

simulated_games <- seq_len(N) %>% 
  map_dfr(
    ~simulate_game(
      tm1_adj_pts, tm1_sd_pf,
      tm2_adj_pts, tm2_sd_pf
    )
  )


head(simulated_games)

hist(simulated_games$point_diff, col = "pale green")

abline(v = mean(simulated_games$point_diff), col = "red", lty = "dashed", lwd = 4)

quantile(simulated_games$point_diff)

mean(simulated_games$point_diff)
mean(simulated_games$tm1)
mean(simulated_games$tm2)
sum(mean(simulated_games$tm1), mean(simulated_games$tm2))



### test --------------------------
link <- "https://cleaningtheglass.com/stats/league/fourfactors/?season=2021&seasontype=regseason&start=10/1/2021&end=10/15/2022&venue=home"
page <- read_html(link)
table <- page %>% html_table(fill = T)
df20 <- table[[1]]

# reformat headers
library(janitor)

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

# have to change numbers to numeric and also remove characters

# change columns with percentages

data_test20 <- as.data.frame(lapply(data20, function(y) gsub("%", "", y)))

# change everything to character

data_test20 <- data_test20 %>% 
  mutate_all(as.character)

# save columns that are numeric
cols.num <- c("pts_poss_off", "efg_percent_off", "tov_percent_off", "orb_percent_off", 
              "ft_rate_off", "pts_poss_def", "efg_percent_def", "tov_percent_def", 
              "orb_percent_def", "ft_rate_def")

# convert to numeric

data_test20[cols.num] <- sapply(data_test20[cols.num],as.numeric)

data_test20 <- data_test20 %>% 
  mutate(Team_name = case_when(
    Team == "Atlanta" ~ "Atlanta Hawks",
    Team == "Brooklyn" ~ "Brooklyn Nets",
    Team == "Boston" ~ "Boston Celtics",
    Team == "New York" ~ "New York Knicks",
    Team == "Philadelphia" ~ "Philadelphia 76ers",
    Team == "Toronto" ~ "Toronto Raptors",
    Team == "Chicago" ~ "Chicago Bulls",
    Team == "Cleveland" ~ "Cleveland Cavaliers",
    Team == "Detroit" ~ "Detroit Pistons",
    Team == "Indiana" ~ "Indiana Pacers",
    Team == "Milwaukee" ~ "Milwaukee Bucks",
    Team == "Charlotte" ~ "Charlotte Hornets",
    Team == "Miami" ~ "Miami Heat",
    Team == "Orlando" ~ "Orlando Magic",
    Team == "Washington" ~ "Washington Wizards",
    Team == "Denver" ~ "Denver Nuggets",
    Team == "Minnesota" ~ "Minnesota Timberwolves",
    Team == "Oklahoma City" ~ "Oklahoma City Thunder",
    Team == "Utah" ~ "Utah Jazz",
    Team == "Golden State" ~ "Golden State Warriors",
    Team == "LA Clippers" ~ "LA Clippers",
    Team == "LA Lakers" ~ "LA Lakers",
    Team == "Phoenix" ~ "Phoenix Suns",
    Team == "Sacramento" ~ "Sacramento Kings",
    Team == "Dallas" ~ "Dallas Mavericks",
    Team == "Houston" ~ "Houston Rockets",
    Team == "Memphis" ~ "Memphis Grizzlies",
    Team == "New Orleans" ~ "New Orleans Pelicans",
    Team == "San Antonio" ~ "San Antonio Spurs",
    Team == "Portland" ~ "Portland Trail Blazers"
  ))

data_test20[data_test20 == "LA Clippers"] <- "Clippers"
data_test20[data_test20 == "LA Lakers"] <- "Lakers"

advanced_data_home <- data_test20 %>% 
  select(Team_name, tov_percent_off, orb_percent_off, ft_rate_off,
         tov_percent_def, ft_rate_def, orb_percent_def)

advanced_data_home[1,1] <- "average"

link <- "https://cleaningtheglass.com/stats/league/fourfactors/?season=2021&seasontype=regseason&start=10/1/2021&end=10/15/2022&venue=away"
page <- read_html(link)
table <- page %>% html_table(fill = T)
df20 <- table[[1]]

# reformat headers
library(janitor)

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

# have to change numbers to numeric and also remove characters

# change columns with percentages

data_test20 <- as.data.frame(lapply(data20, function(y) gsub("%", "", y)))

# change everything to character

data_test20 <- data_test20 %>% 
  mutate_all(as.character)

# save columns that are numeric
cols.num <- c("pts_poss_off", "efg_percent_off", "tov_percent_off", "orb_percent_off", 
              "ft_rate_off", "pts_poss_def", "efg_percent_def", "tov_percent_def", 
              "orb_percent_def", "ft_rate_def")

# convert to numeric

data_test20[cols.num] <- sapply(data_test20[cols.num],as.numeric)

data_test20 <- data_test20 %>% 
  mutate(Team_name = case_when(
    Team == "Atlanta" ~ "Atlanta Hawks",
    Team == "Brooklyn" ~ "Brooklyn Nets",
    Team == "Boston" ~ "Boston Celtics",
    Team == "New York" ~ "New York Knicks",
    Team == "Philadelphia" ~ "Philadelphia 76ers",
    Team == "Toronto" ~ "Toronto Raptors",
    Team == "Chicago" ~ "Chicago Bulls",
    Team == "Cleveland" ~ "Cleveland Cavaliers",
    Team == "Detroit" ~ "Detroit Pistons",
    Team == "Indiana" ~ "Indiana Pacers",
    Team == "Milwaukee" ~ "Milwaukee Bucks",
    Team == "Charlotte" ~ "Charlotte Hornets",
    Team == "Miami" ~ "Miami Heat",
    Team == "Orlando" ~ "Orlando Magic",
    Team == "Washington" ~ "Washington Wizards",
    Team == "Denver" ~ "Denver Nuggets",
    Team == "Minnesota" ~ "Minnesota Timberwolves",
    Team == "Oklahoma City" ~ "Oklahoma City Thunder",
    Team == "Utah" ~ "Utah Jazz",
    Team == "Golden State" ~ "Golden State Warriors",
    Team == "LA Clippers" ~ "LA Clippers",
    Team == "LA Lakers" ~ "LA Lakers",
    Team == "Phoenix" ~ "Phoenix Suns",
    Team == "Sacramento" ~ "Sacramento Kings",
    Team == "Dallas" ~ "Dallas Mavericks",
    Team == "Houston" ~ "Houston Rockets",
    Team == "Memphis" ~ "Memphis Grizzlies",
    Team == "New Orleans" ~ "New Orleans Pelicans",
    Team == "San Antonio" ~ "San Antonio Spurs",
    Team == "Portland" ~ "Portland Trail Blazers"
  ))

data_test20[data_test20 == "LA Clippers"] <- "Clippers"
data_test20[data_test20 == "LA Lakers"] <- "Lakers"

advanced_data_visitor <- data_test20 %>% 
  select(Team_name, tov_percent_off, orb_percent_off, ft_rate_off,
         tov_percent_def, ft_rate_def, orb_percent_def)

advanced_data_visitor[1,1] <- "average"

### offence & defence ----------

url <-  "https://www.espn.com/nba/stats/team"

df <- url %>% 
  read_html() %>% 
  html_nodes("table") %>% 
  html_table(fill = T)

offence_stats <- cbind(df[[1]] %>% select(Team), df[[2]])

colnames(offence_stats)[6] <- "FG_pct"
colnames(offence_stats)[7] <- "FG3_m"
colnames(offence_stats)[8] <- "FG3_a"
colnames(offence_stats)[9] <- "FG3_pct"

offence_stats <- offence_stats %>% 
  select(Team, FGM, FGA, FG_pct, FG3_m, FG3_a, FG3_pct, `FT%`)

teams <- offence_stats$Team
for(i in 1:length(teams)){
  teams[i] = format_name(teams[i])
}

offence_stats$Team <- teams

url <-  "https://www.espn.com/nba/stats/team/_/view/opponent"

df <- url %>% 
  read_html() %>% 
  html_nodes("table") %>% 
  html_table(fill = T)

defence_stats <- cbind(df[[1]] %>% select(Team), df[[2]])

colnames(defence_stats)[6] <- "FG_pct"
colnames(defence_stats)[7] <- "FG3_m"
colnames(defence_stats)[8] <- "FG3_a"
colnames(defence_stats)[9] <- "FG3_pct"

defence_stats <- defence_stats %>% 
  select(Team, FGM, FGA, FG_pct, FG3_m, FG3_a, FG3_pct, `FT%`)

teams <- defence_stats$Team
for(i in 1:length(teams)){
  teams[i] = format_name(teams[i])
}

defence_stats$Team <- teams

offence_stats <- offence_stats %>% 
  mutate(FG2_m = FGM - FG3_m,
         FG2_a = FGA - FG3_a,
         FG2_pct = round(FG2_m/FG2_a, digits = 3)*100,
         FG2_rate = round(FG2_a/FGA, digits = 3)*100,
         FG3_rate = 100 - FG2_rate)

defence_stats <- defence_stats %>% 
  mutate(FG2_m = FGM - FG3_m,
         FG2_a = FGA - FG3_a,
         FG2_pct = round(FG2_m/FG2_a, digits = 3)*100,
         FG2_rate = round(FG2_a/FGA, digits = 3)*100,
         FG3_rate = 100 - FG2_rate)




#### SET UP Monte Carlo ------------------------

simulate_score <- function(probs, p_2pt, p_3pt, p_reb, p_ft, sims){
  
  a <- 1
  pts <- 0
  
  while(a <= sims){
    
    outcome <- sample(c(1:4), 1, prob = probs)
    
    if(outcome == 2){
      end <- 0
      while(end < 1){
        
        shot_prob <- runif(1)
        
        if(shot_prob <= p_2pt){
          
          pts <- pts + 2
          end = 1
          
        }
        
        else{
          
          reb <- runif(1)
          if(reb >= p_reb){
            
            end = 1
            
          }
          
        }
        
      }
    }
    
    if(outcome == 3){
      end <- 0
      while(end < 1){
        
        shot_prob <- runif(1)
        
        if(shot_prob <= p_3pt){
          
          pts <- pts + 3
          end = 1
          
        }
        
        else{
          
          reb <- runif(1)
          if(reb >= p_reb){
            
            end = 1
            
          }
          
        }
        
      }
      
    }
    
    if(outcome == 4){
      end <- 0
      while(end < 1){
        
        shot_prob <- runif(1)
        
        if(shot_prob <= p_ft){
          
          pts <- pts + 1
          end = 1
          
        }
        shot_prob <- runif(1)
        if(shot_prob <= p_ft){
          
          pts <- pts + 1
          end = 1
          
        }
        
        else{
          
          reb <- runif(1)
          if(reb >= p_reb){
            
            end = 1
            
          }
          
        }
        
      }
    }
    a = a+1
  }
  print((pts/sims)*105)
}


### have function, need to automate process



simulate_score(probs = c(.117, .458, .269, .156), p_2pt = .478,
               p_3pt = .342, p_reb = .322, p_ft = .147, 10000)


library(sqldf)
x <- advanced_data_home$Team_name
for(i in 1:31){
  x[i] <- format_name(x[i])
}
advanced_data_home$Team_name <- x

x <- advanced_data_visitor$Team_name
for(i in 1:31){
  x[i] <- format_name(x[i])
}
advanced_data_visitor$Team_name <- x

write_rds(offence_stats, "predictors/offence_stats.rds")
write_rds(defence_stats, "predictors/defence_stats.rds")
write_rds(advanced_data_home, "predictors/advanced_data_home.rds")
write_rds(advanced_data_visitor, "predictors/advanced_data_visitor.rds")

## ACTUAL THING -------------------------------

home_team <- "Warriors"
vis_team <- "Pelicans"

## average ------

avg_3pt_rate_off <- mean(offence_stats$FG3_rate)/100
avg_2pt_rate_off <- mean(offence_stats$FG2_rate)/100
avg_2pt_pct_off <- mean(offence_stats$FG2_pct)/100
avg_3pt_pct_off <- mean(offence_stats$FG3_pct)/100
avg_ft_pct_off <- mean(offence_stats$`FT%`)/100

### home ------------
# get offence numbers
home_3pt_rate_off <- sqldf(paste("SELECT FG3_rate from offence_stats where Team is '", 
                                 home_team, "'", sep = ""))[1, 1]/100
home_2pt_rate_off <- sqldf(paste("SELECT FG2_rate from offence_stats where Team is '", 
                                 home_team, "'", sep = ""))[1, 1]/100
home_2pt_pct_off <- sqldf(paste("SELECT FG2_pct from offence_stats where Team is '", 
                                home_team, "'", sep = ""))[1, 1]/100
home_3pt_pct_off <- sqldf(paste("SELECT FG3_pct from offence_stats where Team is '", 
                                home_team, "'", sep = ""))[1, 1]/100
home_ft_pct_off <- sqldf(paste("SELECT `FT%` from offence_stats where Team is '", 
                               home_team, "'", sep = ""))[1, 1]/100

### rebound, tov, free throw numbers


home_orb_pct_off <- sqldf(paste("SELECT orb_percent_off from advanced_data_home where Team_name is '", 
                                 home_team, "'", sep = ""))[1,1]/100
home_orb_pct_def <- sqldf(paste("SELECT orb_percent_def from advanced_data_home where Team_name is '", 
                                 home_team, "'", sep = ""))[1,1]/100

home_ft_rate_off <- sqldf(paste("SELECT ft_rate_off from advanced_data_home where Team_name is '", 
                                 home_team, "'", sep = ""))[1,1]/100
home_ft_rate_def <- sqldf(paste("SELECT ft_rate_def from advanced_data_home where Team_name is '", 
                                 home_team, "'", sep = ""))[1,1]/100

home_tov_pct_off <- sqldf(paste("SELECT tov_percent_off from advanced_data_home where Team_name is '", 
                               home_team, "'", sep = ""))[1,1]/100
home_tov_pct_def <- sqldf(paste("SELECT tov_percent_def from advanced_data_home where Team_name is '", 
                               home_team, "'", sep = ""))[1,1]/100

## get defense numbers
home_3pt_rate_def <- sqldf(paste("SELECT FG3_rate from defence_stats where Team is '", 
                                 home_team, "'", sep = ""))[1, 1]/100
home_2pt_rate_def <- sqldf(paste("SELECT FG2_rate from defence_stats where Team is '", 
                                 home_team, "'", sep = ""))[1, 1]/100
home_2pt_pct_def <- sqldf(paste("SELECT FG2_pct from defence_stats where Team is '", 
                                home_team, "'", sep = ""))[1, 1]/100
home_3pt_pct_def <- sqldf(paste("SELECT FG3_pct from defence_stats where Team is '", 
                                home_team, "'", sep = ""))[1, 1]/100
home_ft_pct_def <- sqldf(paste("SELECT `FT%` from defence_stats where Team is '", 
                               home_team, "'", sep = ""))[1, 1]/100

### visitor -------
# get offence numbers

vis_3pt_rate_off <- sqldf(paste("SELECT FG3_rate from offence_stats where Team is '", 
                                vis_team, "'", sep = ""))[1, 1]/100
vis_2pt_rate_off <- sqldf(paste("SELECT FG2_rate from offence_stats where Team is '", 
                                vis_team, "'", sep = ""))[1, 1]/100
vis_2pt_pct_off <- sqldf(paste("SELECT FG2_pct from offence_stats where Team is '", 
                               vis_team, "'", sep = ""))[1, 1]/100
vis_3pt_pct_off <- sqldf(paste("SELECT FG3_pct from offence_stats where Team is '", 
                               vis_team, "'", sep = ""))[1, 1]/100
vis_ft_pct_off <- sqldf(paste("SELECT `FT%` from offence_stats where Team is '", 
                              vis_team, "'", sep = ""))[1, 1]/100

### rebound numbers
vis_orb_rate_off <- sqldf(paste("SELECT orb_percent_off from advanced_data_visitor where Team_name is '", 
                                 home_team, "'", sep = ""))[1,1]/100
vis_orb_rate_def <- sqldf(paste("SELECT orb_percent_def from advanced_data_visitor where Team_name is '", 
                                 home_team, "'", sep = ""))[1,1]/100

vis_ft_rate_off <- sqldf(paste("SELECT ft_rate_off from advanced_data_visitor where Team_name is '", 
                               home_team, "'", sep = ""))[1,1]/100
vis_ft_rate_def <- sqldf(paste("SELECT ft_rate_def from advanced_data_visitor where Team_name is '", 
                               home_team, "'", sep = ""))[1,1]/100

vis_tov_pct_off <- sqldf(paste("SELECT tov_percent_off from advanced_data_visitor where Team_name is '", 
                                home_team, "'", sep = ""))[1,1]/100
vis_tov_pct_def <- sqldf(paste("SELECT tov_percent_def from advanced_data_visitor where Team_name is '", 
                                home_team, "'", sep = ""))[1,1]/100


## get defense numbers
vis_3pt_rate_def <- sqldf(paste("SELECT FG3_rate from defence_stats where Team is '", 
                                vis_team, "'", sep = ""))[1, 1]/100
vis_2pt_rate_def <- sqldf(paste("SELECT FG2_rate from defence_stats where Team is '", 
                                vis_team, "'", sep = ""))[1, 1]/100
vis_2pt_pct_def <- sqldf(paste("SELECT FG2_pct from defence_stats where Team is '", 
                               vis_team, "'", sep = ""))[1, 1]/100
vis_3pt_pct_def <- sqldf(paste("SELECT FG3_pct from defence_stats where Team is '", 
                               vis_team, "'", sep = ""))[1, 1]/100
vis_ft_pct_def <- sqldf(paste("SELECT `FT%` from defence_stats where Team is '", 
                              vis_team, "'", sep = ""))[1, 1]/100


## get home difference vs average --------

#offence
home_3pt_rate_off.dif <- home_3pt_rate_off - avg_3pt_rate_off
home_2pt_rate_off.dif <- home_2pt_rate_off - avg_2pt_rate_off
home_2pt_pct_off.dif <- home_2pt_pct_off - avg_2pt_pct_off
home_3pt_pct_off.dif <- home_3pt_pct_off - avg_3pt_pct_off
home_ft_pct_off.dif <- home_ft_pct_off - avg_ft_pct_off

#defence
home_3pt_rate_def.dif <- home_3pt_rate_def - avg_3pt_rate_off
home_2pt_rate_def.dif <- home_2pt_rate_def - avg_2pt_rate_off
home_2pt_pct_def.dif <- home_2pt_pct_def - avg_2pt_pct_off
home_3pt_pct_def.dif <- home_3pt_pct_def - avg_3pt_pct_off
home_ft_pct_def.dif <- home_ft_pct_def - avg_ft_pct_off

#reb
home_orb_pct_off.dif <- sqldf(paste("SELECT orb_percent_off from advanced_data_home where Team_name is '", 
                                home_team, "'", sep = ""))[1,1]/100 - 
  sqldf("SELECT orb_percent_off from advanced_data_home where Team_name is 'average'")[1,1]/100
home_orb_pct_def.dif <- sqldf(paste("SELECT orb_percent_def from advanced_data_home where Team_name is '", 
                                home_team, "'", sep = ""))[1,1]/100 - 
  sqldf("SELECT orb_percent_def from advanced_data_home where Team_name is 'average'")[1,1]/100

#ft
home_ft_rate_off.dif <- sqldf(paste("SELECT ft_rate_off from advanced_data_home where Team_name is '", 
                               home_team, "'", sep = ""))[1,1]/100 - 
  sqldf("SELECT ft_rate_off from advanced_data_home where Team_name is 'average'")[1,1]/100
home_ft_rate_def.dif <- sqldf(paste("SELECT ft_rate_def from advanced_data_home where Team_name is '", 
                               home_team, "'", sep = ""))[1,1]/100 - 
  sqldf("SELECT ft_rate_def from advanced_data_home where Team_name is 'average'")[1,1]/100

#tov
home_tov_pct_off.dif <- sqldf(paste("SELECT tov_percent_off from advanced_data_home where Team_name is '", 
                                home_team, "'", sep = ""))[1,1]/100 - 
  sqldf("SELECT tov_percent_off from advanced_data_home where Team_name is 'average'")[1,1]/100
home_tov_pct_def.dif <- sqldf(paste("SELECT tov_percent_def from advanced_data_home where Team_name is '", 
                                home_team, "'", sep = ""))[1,1]/100 - 
  sqldf("SELECT tov_percent_def from advanced_data_home where Team_name is 'average'")[1,1]/100

## get visitor difference vs average ------

#offence
vis_3pt_rate_off.dif <- vis_3pt_rate_off - avg_3pt_rate_off
vis_2pt_rate_off.dif <- vis_2pt_rate_off - avg_2pt_rate_off
vis_2pt_pct_off.dif <- vis_2pt_pct_off - avg_2pt_pct_off
vis_3pt_pct_off.dif <- vis_3pt_pct_off - avg_3pt_pct_off
vis_ft_pct_off.dif <- vis_ft_pct_off - avg_ft_pct_off

#defence
vis_3pt_rate_def.dif <- vis_3pt_rate_def - avg_3pt_rate_off
vis_2pt_rate_def.dif <- vis_2pt_rate_def - avg_2pt_rate_off
vis_2pt_pct_def.dif <- vis_2pt_pct_def - avg_2pt_pct_off
vis_3pt_pct_def.dif <- vis_3pt_pct_def - avg_3pt_pct_off
vis_ft_pct_def.dif <- vis_ft_pct_def - avg_ft_pct_off

#reb
vis_orb_pct_off.dif <- sqldf(paste("SELECT orb_percent_off from advanced_data_visitor where Team_name is '", 
                                   vis_team, "'", sep = ""))[1,1]/100 - 
  sqldf("SELECT orb_percent_off from advanced_data_visitor where Team_name is 'average'")[1,1]/100
vis_orb_pct_def.dif <- sqldf(paste("SELECT orb_percent_def from advanced_data_visitor where Team_name is '", 
                                   vis_team, "'", sep = ""))[1,1]/100 - 
  sqldf("SELECT orb_percent_def from advanced_data_visitor where Team_name is 'average'")[1,1]/100

#ft
vis_ft_rate_off.dif <- sqldf(paste("SELECT ft_rate_off from advanced_data_visitor where Team_name is '", 
                                  vis_team, "'", sep = ""))[1,1]/100 - 
  sqldf("SELECT ft_rate_off from advanced_data_visitor where Team_name is 'average'")[1,1]/100
vis_ft_rate_def.dif <- sqldf(paste("SELECT ft_rate_def from advanced_data_visitor where Team_name is '", 
                                  vis_team, "'", sep = ""))[1,1]/100 - 
  sqldf("SELECT ft_rate_def from advanced_data_visitor where Team_name is 'average'")[1,1]/100

#tov
vis_tov_pct_off.dif <- sqldf(paste("SELECT tov_percent_off from advanced_data_visitor where Team_name is '", 
                                   vis_team, "'", sep = ""))[1,1]/100 - 
  sqldf("SELECT tov_percent_off from advanced_data_visitor where Team_name is 'average'")[1,1]/100
vis_tov_pct_def.dif <- sqldf(paste("SELECT tov_percent_def from advanced_data_visitor where Team_name is '", 
                                   vis_team, "'", sep = ""))[1,1]/100 - 
  sqldf("SELECT tov_percent_def from advanced_data_visitor where Team_name is 'average'")[1,1]/100


### get difference sums

#home off
diff_sum3pt_rate <- home_3pt_rate_off.dif + vis_3pt_rate_def.dif
diff_sum2pt_rate<- home_2pt_rate_off.dif + vis_2pt_rate_def.dif
diff_sum2pt_pct <- home_2pt_pct_off.dif + vis_2pt_pct_def.dif
diff_sum3pt_pct <- home_3pt_pct_off.dif + vis_3pt_pct_def.dif
diff_sumft_pct <- home_ft_pct_off.dif + vis_ft_pct_def.dif
diff_sumorb_pct <- home_orb_pct_off.dif + vis_orb_pct_def.dif
diff_sumft_rate <- home_ft_rate_off.dif + vis_ft_rate_def.dif
diff_sumtov_rate <- home_tov_pct_off.dif + vis_tov_pct_def.dif

### get adjusted values

adj_3pt_rate <- diff_sum3pt_rate + avg_3pt_rate_off
adj_2pt_rate <- diff_sum2pt_rate + avg_2pt_rate_off
adj_2pt_pct <- diff_sum2pt_pct + avg_2pt_pct_off
adj_3pt_pct <- diff_sum3pt_pct + avg_3pt_pct_off
adj_ft_pct <- diff_sumft_pct + avg_ft_pct_off
adj_orb_pct <- diff_sumorb_pct + sqldf("SELECT tov_percent_def from advanced_data_visitor where Team_name is 'average'")[1,1]/100
adj_ft_rate <- diff_sumft_rate + sqldf("SELECT ft_rate_def from advanced_data_visitor where Team_name is 'average'")[1,1]/100
adj_tov_rate <- diff_sumtov_rate + sqldf("SELECT tov_percent_def from advanced_data_visitor where Team_name is 'average'")[1,1]/100

fga_pct <- 1 - adj_ft_rate - adj_tov_rate
prob_2pt <- adj_2pt_rate * fga_pct
prob_3pt <- adj_3pt_rate * fga_pct


probs <- c(adj_tov_rate, prob_2pt, prob_3pt, adj_ft_rate)
p_2pt <- adj_2pt_pct
p_3pt <- adj_3pt_pct
p_reb <- adj_orb_pct
p_ft <- adj_ft_pct

#home score
home <- simulate_score(probs, p_2pt, p_3pt, 
               p_reb, p_ft, 10000)

#visitor off --------

diff_sum3pt_rate <- vis_3pt_rate_off.dif + home_3pt_rate_def.dif
diff_sum2pt_rate<- vis_2pt_rate_off.dif + home_2pt_rate_def.dif
diff_sum2pt_pct <- vis_2pt_pct_off.dif + home_2pt_pct_def.dif
diff_sum3pt_pct <- vis_3pt_pct_off.dif + home_3pt_pct_def.dif
diff_sumft_pct <- vis_ft_pct_off.dif + home_ft_pct_def.dif
diff_sumorb_pct <- vis_orb_pct_off.dif + home_orb_pct_def.dif
diff_sumft_rate <- vis_ft_rate_off.dif + home_ft_rate_def.dif
diff_sumtov_rate <- vis_tov_pct_off.dif + home_tov_pct_def.dif

adj_3pt_rate <- diff_sum3pt_rate + avg_3pt_rate_off
adj_2pt_rate <- diff_sum2pt_rate + avg_2pt_rate_off
adj_2pt_pct <- diff_sum2pt_pct + avg_2pt_pct_off
adj_3pt_pct <- diff_sum3pt_pct + avg_3pt_pct_off
adj_ft_pct <- diff_sumft_pct + avg_ft_pct_off
adj_orb_pct <- diff_sumorb_pct + sqldf("SELECT tov_percent_def from advanced_data_visitor where Team_name is 'average'")[1,1]/100
adj_ft_rate <- diff_sumft_rate + sqldf("SELECT ft_rate_def from advanced_data_visitor where Team_name is 'average'")[1,1]/100
adj_tov_rate <- diff_sumtov_rate + sqldf("SELECT tov_percent_def from advanced_data_visitor where Team_name is 'average'")[1,1]/100

fga_pct <- 1 - adj_ft_rate - adj_tov_rate
prob_2pt <- adj_2pt_rate * fga_pct
prob_3pt <- adj_3pt_rate * fga_pct


probs <- c(adj_tov_rate, prob_2pt, prob_3pt, adj_ft_rate)
p_2pt <- adj_2pt_pct
p_3pt <- adj_3pt_pct
p_reb <- adj_orb_pct
p_ft <- adj_ft_pct

#visitor score
visitor <- simulate_score(probs, p_2pt, p_3pt, 
               p_reb, p_ft, 10000)

print(paste("Based on 10000 simulations, the predicted score is:", home_team, 
            "-", home, ",", vis_team, "-", visitor))

