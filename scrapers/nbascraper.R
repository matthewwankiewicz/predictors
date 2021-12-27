library(tidyverse)
library(rvest)
library(janitor)
library(rstanarm)
library(gam)
library(lubridate)
library(sqldf)

## player stats
link <- "https://hashtagbasketball.com/nba-defense-vs-position"
page <- read_html(link)
table <- page %>% html_table(fill = T)

stats <- table[[11]]

format_name <- function(name){
  last_space <- last(gregexpr(" ", name)[[1]]) + 1
  substr(name, start = 1, stop = last_space-2)
}

x <- data.frame(stats)
for(i in 2:ncol(x)){
  x[,i] <- format_name(x[,i])
}

stats <- tibble(x)

names <- c("Position", "TEAM_NAME", "OPP_PTS", "OPP_FG_PCT", "OPP_FT_PCT", "OPP_FG3M", "OPP_REB", "OPP_AST",
           "OPP_STL", "OPP_BLK", "OPP_TO")

colnames(stats) <- names


cols.num <- c("OPP_PTS", "OPP_FG_PCT", "OPP_FT_PCT", "OPP_FG3M", "OPP_REB", "OPP_AST",
              "OPP_STL", "OPP_BLK", "OPP_TO")

stats[cols.num] <- sapply(stats[cols.num],as.numeric)

team_stats <- stats %>% 
  mutate(TEAM = case_when(
    TEAM_NAME == "ATL" ~ "Atlanta Hawks",
    TEAM_NAME == "BRO" ~ "Brooklyn Nets",
    TEAM_NAME == "BOS" ~ "Boston Celtics",
    TEAM_NAME == "NYK" ~ "New York Knicks",
    TEAM_NAME == "PHI" ~ "Philadelphia 76ers",
    TEAM_NAME == "TOR" ~ "Toronto Raptors",
    TEAM_NAME == "CHI" ~ "Chicago Bulls",
    TEAM_NAME == "CLE" ~ "Cleveland Cavaliers",
    TEAM_NAME == "DET" ~ "Detroit Pistons",
    TEAM_NAME == "IND" ~ "Indiana Pacers",
    TEAM_NAME == "MIL" ~ "Milwaukee Bucks",
    TEAM_NAME == "CHA" ~ "Charlotte Hornets",
    TEAM_NAME == "MIA" ~ "Miami Heat",
    TEAM_NAME == "ORL" ~ "Orlando Magic",
    TEAM_NAME == "WAS" ~ "Washington Wizards",
    TEAM_NAME == "DEN" ~ "Denver Nuggets",
    TEAM_NAME == "MIN" ~ "Minnesota Timberwolves",
    TEAM_NAME == "OKL" ~ "Oklahoma City Thunder",
    TEAM_NAME == "UTA" ~ "Utah Jazz",
    TEAM_NAME == "GSW" ~ "Golden State Warriors",
    TEAM_NAME == "LAC" ~ "Los Angeles Clippers",
    TEAM_NAME == "LAL" ~ "Los Angeles Lakers",
    TEAM_NAME == "PHX" ~ "Phoenix Suns",
    TEAM_NAME == "SAC" ~ "Sacramento Kings",
    TEAM_NAME == "DAL" ~ "Dallas Mavericks",
    TEAM_NAME == "HOU" ~ "Houston Rockets",
    TEAM_NAME == "MEM" ~ "Memphis Grizzlies",
    TEAM_NAME == "NOP" ~ "New Orleans Pelicans",
    TEAM_NAME == "SAS" ~ "San Antonio Spurs",
    TEAM_NAME == "POR" ~ "Portland Trail Blazers"
  ))



c_stats <- team_stats %>% filter(Position == "C") %>% select(-c(Position, TEAM_NAME))
pf_stats <- team_stats %>% filter(Position == "PF") %>% select(-c(Position, TEAM_NAME))
sf_stats <- team_stats %>% filter(Position == "SF") %>% select(-c(Position, TEAM_NAME))
sg_stats <- team_stats %>% filter(Position == "SG") %>% select(-c(Position, TEAM_NAME))
pg_stats <- team_stats %>% filter(Position == "PG") %>% select(-c(Position, TEAM_NAME))


write_rds(pg_stats, "predictors/pg_stats.rds")
write_rds(sg_stats, "predictors/sg_stats.rds")
write_rds(sf_stats, "predictors/sf_stats.rds")
write_rds(pf_stats, "predictors/pf_stats.rds")
write_rds(c_stats, "predictors/c_stats.rds")


## gamelogs

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



## team stats
home_ff <- read_csv("models/home_four_factors.csv")
home_ff <- home_ff %>% 
  select(TEAM_NAME, FTA_RATE, TM_TOV_PCT, OREB_PCT,
         OPP_FTA_RATE, OPP_TOV_PCT, OPP_OREB_PCT)

advanced_data_home <- rbind(home_ff, summarise_all(home_ff, mean))
advanced_data_home[31, 1] <- "average"

visitor_ff <- read_csv("models/visitor_four_factors.csv")
visitor_ff <- visitor_ff %>% 
  select(TEAM_NAME, FTA_RATE, TM_TOV_PCT, OREB_PCT,
         OPP_FTA_RATE, OPP_TOV_PCT, OPP_OREB_PCT)

advanced_data_visitor <- rbind(visitor_ff, summarise_all(visitor_ff, mean))
advanced_data_visitor[31, 1] <- "average"


### offence & defence ----------

home_stats <- read_csv("models/home_stats.csv")
home_stats <- home_stats %>% 
  select(TEAM_NAME = TEAM_NAME...3,
         FGM, FGA, FG_PCT, FG3M, FG3A, FG3_PCT, FT_PCT,
         OPP_FGM, OPP_FGA, OPP_FG_PCT, OPP_FG3M, OPP_FG3A,
         OPP_FG3_PCT, OPP_FT_PCT)

visitor_stats <- read_csv("models/visitor_stats.csv")
visitor_stats <- visitor_stats %>% 
  select(TEAM_NAME = TEAM_NAME...3,
         FGM, FGA, FG_PCT, FG3M, FG3A, FG3_PCT, FT_PCT,
         OPP_FGM, OPP_FGA, OPP_FG_PCT, OPP_FG3M, OPP_FG3A,
         OPP_FG3_PCT, OPP_FT_PCT)


home_stats <- home_stats %>% 
  mutate(FG2M = FGM - FG3M,
         FG2A = FGA - FG3A,
         FG2_PCT = round(FG2M/FG2A, digits = 3)*100,
         FG2_rate = round(FG2A/FGA, digits = 3)*100,
         FG3_rate = 100 - FG2_rate,
         OPP_FG2M = OPP_FGM - OPP_FG3M,
         OPP_FG2A = OPP_FGA - OPP_FG3A,
         OPP_FG2_PCT = round(OPP_FG2M/OPP_FG2A, digits = 3)*100,
         OPP_FG2_rate = round(OPP_FG2A/OPP_FGA, digits = 3)*100,
         OPP_FG3_rate = 100 - OPP_FG2_rate)

visitor_stats <- visitor_stats %>% 
  mutate(FG2M = FGM - FG3M,
         FG2A = FGA - FG3A,
         FG2_PCT = round(FG2M/FG2A, digits = 3)*100,
         FG2_rate = round(FG2A/FGA, digits = 3)*100,
         FG3_rate = 100 - FG2_rate,
         OPP_FG2M = OPP_FGM - OPP_FG3M,
         OPP_FG2A = OPP_FGA - OPP_FG3A,
         OPP_FG2_PCT = round(OPP_FG2M/OPP_FG2A, digits = 3)*100,
         OPP_FG2_rate = round(OPP_FG2A/OPP_FGA, digits = 3)*100,
         OPP_FG3_rate = 100 - OPP_FG2_rate)

#### SET UP Monte Carlo ------------------------

simulate_score <- function(probs, p_2pt, p_3pt, p_reb, p_ft, pace, sims){
  
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
  return((pts/sims)*pace)
}

##### advanced data ----------------
link <- "https://www.teamrankings.com/nba/stat/possessions-per-game"
table <- read_html(link) %>% 
  html_table()

possessions <- table[[1]]

possessions <- possessions %>% 
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
    Team == "Okla City" ~ "Oklahoma City Thunder",
    Team == "Utah" ~ "Utah Jazz",
    Team == "Golden State" ~ "Golden State Warriors",
    Team == "LA Clippers" ~ "LA Clippers",
    Team == "LA Lakers" ~ "Los Angeles Lakers",
    Team == "Phoenix" ~ "Phoenix Suns",
    Team == "Sacramento" ~ "Sacramento Kings",
    Team == "Dallas" ~ "Dallas Mavericks",
    Team == "Houston" ~ "Houston Rockets",
    Team == "Memphis" ~ "Memphis Grizzlies",
    Team == "New Orleans" ~ "New Orleans Pelicans",
    Team == "San Antonio" ~ "San Antonio Spurs",
    Team == "Portland" ~ "Portland Trail Blazers"
  ))

possessions <- possessions %>% 
  select(Team_name, Home, Away, "last_3" = `Last 3`)

possessions <- possessions %>% 
  mutate(home_weight = sqrt(Home*last_3),
         away_weight = sqrt(Away*last_3))


write_rds(home_stats, "predictors/home_stats.rds")
write_rds(visitor_stats, "predictors/visitor_stats.rds")
write_rds(advanced_data_home, "predictors/advanced_data_home.rds")
write_rds(advanced_data_visitor, "predictors/advanced_data_visitor.rds")
write_rds(possessions, "predictors/possessions.rds")


## function for home, away ----------

avg_3pt_rate_off <- mean(home_stats$FG3_rate)/100
avg_2pt_rate_off <- mean(home_stats$FG2_rate)/100
avg_2pt_pct_off <- mean(home_stats$FG2_PCT)/100
avg_3pt_pct_off <- mean(home_stats$FG3_PCT)
avg_ft_pct_off <- mean(home_stats$FT_PCT)

simulate_game <- function(home_team, vis_team){
  home_3pt_rate_off <- sqldf(paste("SELECT FG3_rate from home_stats where TEAM_NAME is '", 
                                   home_team, "'", sep = ""))[1, 1]/100
  home_2pt_rate_off <- sqldf(paste("SELECT FG2_rate from home_stats where TEAM_NAME is '", 
                                   home_team, "'", sep = ""))[1, 1]/100
  home_2pt_pct_off <- sqldf(paste("SELECT FG2_PCT from home_stats where TEAM_NAME is '", 
                                  home_team, "'", sep = ""))[1, 1]/100
  home_3pt_pct_off <- sqldf(paste("SELECT FG3_PCT from home_stats where TEAM_NAME is '", 
                                  home_team, "'", sep = ""))[1, 1]
  home_ft_pct_off <- sqldf(paste("SELECT FT_PCT from home_stats where TEAM_NAME is '", 
                                 home_team, "'", sep = ""))[1, 1]
  
  ### rebound, tov, free throw numbers
  
  home_orb_pct_off <- sqldf(paste("SELECT OREB_PCT from advanced_data_home where TEAM_NAME is '", 
                                  home_team, "'", sep = ""))[1,1]
  home_orb_pct_def <- sqldf(paste("SELECT OPP_OREB_PCT from advanced_data_home where TEAM_NAME is '", 
                                  home_team, "'", sep = ""))[1,1]
  
  home_ft_rate_off <- sqldf(paste("SELECT FTA_RATE from advanced_data_home where TEAM_NAME is '", 
                                  home_team, "'", sep = ""))[1,1]
  home_ft_rate_def <- sqldf(paste("SELECT OPP_FTA_RATE from advanced_data_home where TEAM_NAME is '", 
                                  home_team, "'", sep = ""))[1,1]
  
  home_tov_pct_off <- sqldf(paste("SELECT TM_TOV_PCT from advanced_data_home where TEAM_NAME is '", 
                                  home_team, "'", sep = ""))[1,1]
  home_tov_pct_def <- sqldf(paste("SELECT OPP_TOV_PCT from advanced_data_home where TEAM_NAME is '", 
                                  home_team, "'", sep = ""))[1,1]
  
  ## get defense numbers
  home_3pt_rate_def <- sqldf(paste("SELECT OPP_FG3_rate from home_stats where TEAM_NAME is '", 
                                   home_team, "'", sep = ""))[1, 1]/100
  home_2pt_rate_def <- sqldf(paste("SELECT OPP_FG2_rate from home_stats where TEAM_NAME is '", 
                                   home_team, "'", sep = ""))[1, 1]/100
  home_2pt_pct_def <- sqldf(paste("SELECT OPP_FG2_PCT from home_stats where TEAM_NAME is '", 
                                  home_team, "'", sep = ""))[1, 1]/100
  home_3pt_pct_def <- sqldf(paste("SELECT OPP_FG3_PCT from home_stats where TEAM_NAME is '", 
                                  home_team, "'", sep = ""))[1, 1]
  home_ft_pct_def <- sqldf(paste("SELECT OPP_FT_PCT from home_stats where TEAM_NAME is '", 
                                 home_team, "'", sep = ""))[1, 1]
  
  ### visitor -------
  # get offence numbers
  
  vis_3pt_rate_off <- sqldf(paste("SELECT FG3_rate from visitor_stats where TEAM_NAME is '", 
                                  vis_team, "'", sep = ""))[1, 1]/100
  vis_2pt_rate_off <- sqldf(paste("SELECT FG2_rate from visitor_stats where TEAM_NAME is '", 
                                  vis_team, "'", sep = ""))[1, 1]/100
  vis_2pt_pct_off <- sqldf(paste("SELECT FG2_PCT from visitor_stats where TEAM_NAME is '", 
                                 vis_team, "'", sep = ""))[1, 1]/100
  vis_3pt_pct_off <- sqldf(paste("SELECT FG3_PCT from visitor_stats where TEAM_NAME is '", 
                                 vis_team, "'", sep = ""))[1, 1]
  vis_ft_pct_off <- sqldf(paste("SELECT FT_PCT from visitor_stats where TEAM_NAME is '", 
                                vis_team, "'", sep = ""))[1, 1]
  
  ### rebound numbers
  vis_orb_rate_off <- sqldf(paste("SELECT OREB_PCT from advanced_data_visitor where TEAM_NAME is '", 
                                  home_team, "'", sep = ""))[1,1]
  vis_orb_rate_def <- sqldf(paste("SELECT OPP_OREB_PCT from advanced_data_visitor where TEAM_NAME is '", 
                                  home_team, "'", sep = ""))[1,1]
  
  vis_ft_rate_off <- sqldf(paste("SELECT FTA_RATE from advanced_data_visitor where TEAM_NAME is '", 
                                 home_team, "'", sep = ""))[1,1]
  vis_ft_rate_def <- sqldf(paste("SELECT OPP_FTA_RATE from advanced_data_visitor where TEAM_NAME is '", 
                                 home_team, "'", sep = ""))[1,1]
  
  vis_tov_pct_off <- sqldf(paste("SELECT TM_TOV_PCT from advanced_data_visitor where TEAM_NAME is '", 
                                 home_team, "'", sep = ""))[1,1]
  vis_tov_pct_def <- sqldf(paste("SELECT OPP_TOV_PCT from advanced_data_visitor where TEAM_NAME is '", 
                                 home_team, "'", sep = ""))[1,1]
  
  
  ## get defense numbers
  vis_3pt_rate_def <- sqldf(paste("SELECT OPP_FG3_rate from visitor_stats where TEAM_NAME is '", 
                                  vis_team, "'", sep = ""))[1, 1]/100
  vis_2pt_rate_def <- sqldf(paste("SELECT OPP_FG2_rate from visitor_stats where TEAM_NAME is '", 
                                  vis_team, "'", sep = ""))[1, 1]/100
  vis_2pt_pct_def <- sqldf(paste("SELECT OPP_FG2_PCT from visitor_stats where TEAM_NAME is '", 
                                 vis_team, "'", sep = ""))[1, 1]/100
  vis_3pt_pct_def <- sqldf(paste("SELECT OPP_FG3_PCT from visitor_stats where TEAM_NAME is '", 
                                 vis_team, "'", sep = ""))[1, 1]
  vis_ft_pct_def <- sqldf(paste("SELECT OPP_FT_PCT from visitor_stats where TEAM_NAME is '", 
                                vis_team, "'", sep = ""))[1, 1]
  
  
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
  home_orb_pct_off.dif <- sqldf(paste("SELECT OREB_PCT from advanced_data_home where TEAM_NAME is '", 
                                      home_team, "'", sep = ""))[1,1] - 
    sqldf("SELECT OREB_PCT from advanced_data_home where TEAM_NAME is 'average'")[1,1]
  home_orb_pct_def.dif <- sqldf(paste("SELECT OPP_OREB_PCT from advanced_data_home where TEAM_NAME is '", 
                                      home_team, "'", sep = ""))[1,1] - 
    sqldf("SELECT OPP_OREB_PCT from advanced_data_home where TEAM_NAME is 'average'")[1,1]
  
  #ft
  home_ft_rate_off.dif <- sqldf(paste("SELECT FTA_RATE from advanced_data_home where TEAM_NAME is '", 
                                      home_team, "'", sep = ""))[1,1] - 
    sqldf("SELECT FTA_RATE from advanced_data_home where TEAM_NAME is 'average'")[1,1]
  home_ft_rate_def.dif <- sqldf(paste("SELECT OPP_FTA_RATE from advanced_data_home where TEAM_NAME is '", 
                                      home_team, "'", sep = ""))[1,1] - 
    sqldf("SELECT OPP_FTA_RATE from advanced_data_home where TEAM_NAME is 'average'")[1,1]
  
  #tov
  home_tov_pct_off.dif <- sqldf(paste("SELECT TM_TOV_PCT from advanced_data_home where TEAM_NAME is '", 
                                      home_team, "'", sep = ""))[1,1] - 
    sqldf("SELECT TM_TOV_PCT from advanced_data_home where TEAM_NAME is 'average'")[1,1]
  home_tov_pct_def.dif <- sqldf(paste("SELECT OPP_TOV_PCT from advanced_data_home where TEAM_NAME is '", 
                                      home_team, "'", sep = ""))[1,1] - 
    sqldf("SELECT OPP_TOV_PCT from advanced_data_home where TEAM_NAME is 'average'")[1,1]
  
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
  vis_orb_pct_off.dif <- sqldf(paste("SELECT OREB_PCT from advanced_data_visitor where TEAM_NAME is '", 
                                     vis_team, "'", sep = ""))[1,1] - 
    sqldf("SELECT OREB_PCT from advanced_data_visitor where TEAM_NAME is 'average'")[1,1]
  vis_orb_pct_def.dif <- sqldf(paste("SELECT OPP_OREB_PCT from advanced_data_visitor where TEAM_NAME is '", 
                                     vis_team, "'", sep = ""))[1,1] - 
    sqldf("SELECT OPP_OREB_PCT from advanced_data_visitor where TEAM_NAME is 'average'")[1,1]
  
  #ft
  vis_ft_rate_off.dif <- sqldf(paste("SELECT FTA_RATE from advanced_data_visitor where TEAM_NAME is '", 
                                     vis_team, "'", sep = ""))[1,1] - 
    sqldf("SELECT FTA_RATE from advanced_data_visitor where TEAM_NAME is 'average'")[1,1]
  vis_ft_rate_def.dif <- sqldf(paste("SELECT OPP_FTA_RATE from advanced_data_visitor where TEAM_NAME is '", 
                                     vis_team, "'", sep = ""))[1,1] - 
    sqldf("SELECT OPP_FTA_RATE from advanced_data_visitor where TEAM_NAME is 'average'")[1,1]
  
  #tov
  vis_tov_pct_off.dif <- sqldf(paste("SELECT TM_TOV_PCT from advanced_data_visitor where TEAM_NAME is '", 
                                     vis_team, "'", sep = ""))[1,1] - 
    sqldf("SELECT TM_TOV_PCT from advanced_data_visitor where TEAM_NAME is 'average'")[1,1]
  vis_tov_pct_def.dif <- sqldf(paste("SELECT OPP_TOV_PCT from advanced_data_visitor where TEAM_NAME is '", 
                                     vis_team, "'", sep = ""))[1,1] - 
    sqldf("SELECT OPP_TOV_PCT from advanced_data_visitor where TEAM_NAME is 'average'")[1,1]
  
  
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
  adj_orb_pct <- diff_sumorb_pct + sqldf("SELECT OPP_OREB_PCT from advanced_data_visitor where TEAM_NAME is 'average'")[1,1]
  adj_ft_rate <- diff_sumft_rate + sqldf("SELECT OPP_FTA_RATE from advanced_data_visitor where TEAM_NAME is 'average'")[1,1]
  adj_tov_rate <- diff_sumtov_rate + sqldf("SELECT OPP_TOV_PCT from advanced_data_visitor where TEAM_NAME is 'average'")[1,1]
  
  fga_pct <- 1 - adj_ft_rate - adj_tov_rate
  prob_2pt <- adj_2pt_rate * fga_pct
  prob_3pt <- adj_3pt_rate * fga_pct
  
  
  probs <- c(adj_tov_rate, prob_2pt, prob_3pt, adj_ft_rate)
  p_2pt <- adj_2pt_pct
  p_3pt <- adj_3pt_pct
  p_reb <- adj_orb_pct
  p_ft <- adj_ft_pct
  
  pace <- sqldf(paste("SELECT last_3 from possessions where Team_name is '",
                      home_team, "'", sep = ""))[1,1]
  #home score
  home_score <- round(simulate_score(probs, p_2pt, p_3pt, 
                                     p_reb, p_ft, pace, 10000), digits = 0)
  
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
  adj_orb_pct <- diff_sumorb_pct + sqldf("SELECT OPP_OREB_PCT from advanced_data_visitor where TEAM_NAME is 'average'")[1,1]
  adj_ft_rate <- diff_sumft_rate + sqldf("SELECT OPP_FTA_RATE from advanced_data_visitor where TEAM_NAME is 'average'")[1,1]
  adj_tov_rate <- diff_sumtov_rate + sqldf("SELECT OPP_TOV_PCT from advanced_data_visitor where TEAM_NAME is 'average'")[1,1]
  
  fga_pct <- 1 - adj_ft_rate - adj_tov_rate
  prob_2pt <- adj_2pt_rate * fga_pct
  prob_3pt <- adj_3pt_rate * fga_pct
  
  
  probs <- c(adj_tov_rate, prob_2pt, prob_3pt, adj_ft_rate)
  p_2pt <- adj_2pt_pct
  p_3pt <- adj_3pt_pct
  p_reb <- adj_orb_pct
  p_ft <- adj_ft_pct
  
  pace <- sqldf(paste("SELECT last_3 from possessions where Team_name is '",
                      vis_team, "'", sep = ""))[1,1]
  #visitor score
  visitor_score <- round(simulate_score(probs, p_2pt, p_3pt,
                                        p_reb, p_ft, pace, 10000), digits = 0)
  
  return((paste(home_score, "-", visitor_score, sep = "")))
}
simulate_game("Houston Rockets", "Chicago Bulls")

schedule <- schedule %>% 
  mutate(home_win = ifelse(home_score > visitor_score, 1, 0))
sked <- schedule %>% 
  select(home, visitor, home_score,
         visitor_score, home_win)

sked[sked == "Los Angeles Clippers"] <- "LA Clippers"

home <- sked$home
visitor <- sked$visitor

library(sqldf)
score <- c()
for(i in 1:length(home)){
  score[i] <- simulate_game(home[i], visitor[i])
}

sked$score <- score

sked <- sked %>% 
  separate(score, into = c("home_score_sim", "visitor_score_sim"), sep = "-")

sked$home_score_sim <- as.numeric(sked$home_score_sim)
sked$visitor_score_sim <- as.numeric(sked$visitor_score_sim)

sked <- sked %>% 
  mutate(home_win_sim = ifelse(home_score_sim > visitor_score_sim,
                               1, 0),
         sim_correct = ifelse(home_win_sim == home_win, 1, 0))

sum(sked$sim_correct)


test <- readxl::read_xlsx("models/testing.xlsx")

home <- na.omit(test$home)
away <- na.omit(test$visitor)
home_score <- na.omit(test$home_score)
away_score <- na.omit(test$visitor_score)
over_under <- na.omit(test$over_under)
home_ml <- na.omit(test$home_ml)
home_spread <- na.omit(test$home_spread)

test <- tibble(home, away, home_score, away_score, over_under, home_ml, home_spread)

test <- test %>% 
  mutate(home_spread_true = ifelse(home_ml < 0, home_spread*-1, home_spread))

sked2 <- sked[1:279,]


test2 <- left_join(sked2, test, by = c("home", "visitor" = "away",
                                       "home_score", "visitor_score" = "away_score"))

test2 <- test2 %>% 
  mutate(total_act = home_score + visitor_score,
         total_sim = home_score_sim + visitor_score_sim,
         over = ifelse(total_act > over_under, 1, 0),
         over_sim = ifelse(total_sim > over_under, 1, 0),
         correct_over = ifelse(over == over_sim, 1, 0),
         home_dog = ifelse(home_ml > 0, 1, 0),
         home_cover = ifelse(home_spread_true < 0,
                             ifelse((home_score - visitor_score) > home_spread, 1, 0),
                             ifelse((visitor_score - home_score) < home_spread, 1, 0)),
         home_cover_sim = ifelse(home_spread_true < 0,
                                 ifelse((home_score_sim - visitor_score_sim) > home_spread, 1, 0),
                                 ifelse((visitor_score_sim - home_score_sim) < home_spread, 1, 0)),
         sim_cover = ifelse(home_cover == home_cover_sim, 1, 0))

x <- test2 %>% 
  select(home_score, visitor_score, home_spread, home_spread_true, home_cover, home_cover_sim)

home_over <- test2 %>% 
  group_by(home) %>% 
  summarise(home_correct = sum(sim_cover),
            home_n = n())

visitor_over <- test2 %>% 
  group_by(visitor) %>% 
  summarise(vis_correct = sum(sim_cover),
            vis_n = n())

pred_results <- left_join(home_over, visitor_over, 
                          by = c("home" = "visitor"))

pred_results <- pred_results %>% 
  mutate(overall_correct = home_correct + vis_correct,
         total_games = home_n + vis_n, 
         home_accuracy = round(home_correct/home_n, 3)*100,
         visitor_accuracy = round(vis_correct/vis_n, 3)*100,
         accuracy = round(overall_correct/total_games, 3)*100)

write_rds(pred_results, "predictors/model_stats.rds")




#### LOGISTIC ----------------



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
    Team == "LA Clippers" ~ "Los Angeles Clippers",
    Team == "LA Lakers" ~ "Los Angeles Lakers",
    Team == "Phoenix" ~ "Phoenix Suns",
    Team == "Sacramento" ~ "Sacramento Kings",
    Team == "Dallas" ~ "Dallas Mavericks",
    Team == "Houston" ~ "Houston Rockets",
    Team == "Memphis" ~ "Memphis Grizzlies",
    Team == "New Orleans" ~ "New Orleans Pelicans",
    Team == "San Antonio" ~ "San Antonio Spurs",
    Team == "Portland" ~ "Portland Trail Blazers"
  ))


home_teams <- data_test20$Team_name
away_teams <- data_test20$Team_name

matchups <- expand_grid(home_teams, away_teams)
matchups <- matchups %>% drop_na()

matchups <- matchups %>% 
  left_join(data_test20, by = c("home_teams" = "Team_name"))
matchups <- matchups %>% 
  left_join(data_test20, by = c("away_teams" = "Team_name"))


# ----------------------------------- GET SCHEDULE ---------------------------------------

# Data from basketball reference


link <- "https://www.basketball-reference.com/leagues/NBA_2022_games.html"
table <- link %>% 
  read_html() %>% 
  html_table()
table <- table[[1]]
table <- table[,-7]
table <- table[,-7]
colnames(table)[4] <- "visitor_score"
colnames(table)[6] <- "home_score"
table <- table %>% 
  select(-c(`Start (ET)`, Attend., Notes))

table <- table %>% 
  select(Date, `Home/Neutral`, `Visitor/Neutral`, home_score, visitor_score)
schedule <- table %>% 
  rename("visitor" = `Visitor/Neutral`,
         "home" = `Home/Neutral`)

schedule_oct <- drop_na(schedule)


### November

link <- "https://www.basketball-reference.com/leagues/NBA_2022_games-november.html"
table <- link %>% 
  read_html() %>% 
  html_table()
table <- table[[1]]
table <- table[,-7]
table <- table[,-7]
colnames(table)[4] <- "visitor_score"
colnames(table)[6] <- "home_score"
table <- table %>% 
  select(-c(`Start (ET)`, Attend., Notes))


table <- table %>% 
  select(Date, `Home/Neutral`, `Visitor/Neutral`, home_score, visitor_score)
schedule <- table %>% 
  rename("visitor" = `Visitor/Neutral`,
         "home" = `Home/Neutral`)

schedule_nov <- drop_na(schedule)

schedule <- rbind(schedule_oct, schedule_nov)

## December schedule


link <- "https://www.basketball-reference.com/leagues/NBA_2022_games-december.html"
table <- link %>% 
  read_html() %>% 
  html_table()
table <- table[[1]]
table <- table[,-7]
table <- table[,-7]
colnames(table)[4] <- "visitor_score"
colnames(table)[6] <- "home_score"
table <- table %>% 
  select(-c(`Start (ET)`, Attend., Notes))


table <- table %>% 
  select(Date, `Home/Neutral`, `Visitor/Neutral`, home_score, visitor_score)
schedule <- table %>% 
  rename("visitor" = `Visitor/Neutral`,
         "home" = `Home/Neutral`)

schedule_dec <- drop_na(schedule)

schedule <- rbind(schedule_oct, schedule_nov, schedule_dec)

#### Find Back to Backs
# reformat dates

dates <- schedule$Date

dates <- lubridate::mdy(dates)

schedule$Date <- dates

home_schedule <- schedule %>% 
  group_by(home, Date) %>% 
  summarise(played = n())

visitor_schedule <- schedule %>% 
  group_by(visitor, Date) %>% 
  summarise(played = n())

team_schedules <- rbind(home_schedule, visitor_schedule)

team_schedules <- team_schedules %>% 
  mutate(team = ifelse(is.na(home), visitor, home)) %>% 
  arrange(team, Date) %>% 
  select(-played)

dates <- team_schedules$Date

b2b <- rep(NA, length(dates))
for(i in 2:length(dates)){
  if(dates[i] == dates[i-1] + 1){
    b2b[i] <- 1
  }
  else{
    b2b[i] <- 0
  }
}

team_schedules$b2b <- b2b
team_schedules[1, 5] = 0

team_schedules <- team_schedules %>% 
  ungroup() %>% 
  select(c(Date, team, b2b))

team_schedules$Date <- as.character(team_schedules$Date)

## bring back to back into schedules
schedule$Date <- as.character(schedule$Date)

schedule_homeb2b <- left_join(schedule, team_schedules, 
                                   by = c("Date", "home" = "team")) %>% 
  rename("b2b_home" = b2b)

schedule_visitorb2b <- left_join(schedule, team_schedules, 
                              by = c("Date", "visitor" = "team")) %>% 
  rename("b2b_visitor" = b2b)

schedule_bothb2b <- left_join(schedule_homeb2b, schedule_visitorb2b)


# ---------------------------------- MODEL BUILD -----------------------------------------------

## original model

nba_schedule <- left_join(schedule, data_test20,
                          by = c("home" = "Team_name"))
nba_schedule <- left_join(nba_schedule, data_test20,
                          by = c("visitor" = "Team_name"))
nba_schedule <- nba_schedule %>% 
  mutate(home_win = ifelse(home_score > visitor_score, 1, 0))

# create variables for differences
nba_schedule <- nba_schedule %>% 
  mutate(efg_diff_off = efg_percent_off.x - efg_percent_off.y,
         efg_diff_def = efg_percent_def.x - efg_percent_def.y,
         tov_diff_off = tov_percent_off.x - tov_percent_off.y,
         tov_diff_def = tov_percent_def.x - tov_percent_def.y,
         orb_diff_off = orb_percent_off.x - orb_percent_off.y,
         orb_diff_def = orb_percent_def.x - orb_percent_def.y,
         ft_diff_off = ft_rate_off.x - ft_rate_off.y,
         ft_diff_def = ft_rate_def.x - ft_rate_def.y,
         pts_poss_off = pts_poss_off.x - pts_poss_off.y,
         pts_poss_def = pts_poss_def.x - pts_poss_def.y)

matchups <- matchups %>% 
  mutate(efg_diff_off = efg_percent_off.x - efg_percent_off.y,
         efg_diff_def = efg_percent_def.x - efg_percent_def.y,
         tov_diff_off = tov_percent_off.x - tov_percent_off.y,
         tov_diff_def = tov_percent_def.x - tov_percent_def.y,
         orb_diff_off = orb_percent_off.x - orb_percent_off.y,
         orb_diff_def = orb_percent_def.x - orb_percent_def.y,
         ft_diff_off = ft_rate_off.x - ft_rate_off.y,
         ft_diff_def = ft_rate_def.x - ft_rate_def.y,
         pts_poss_off = pts_poss_off.x - pts_poss_off.y,
         pts_poss_def = pts_poss_def.x - pts_poss_def.y)

nba_schedule <- nba_schedule %>% 
  select(-c(Team.x, Team.y, Date))


## -------------


nba_schedule[nba_schedule == "Los Angeles Clippers"] <- "LA Clippers"

home <- nba_schedule$home
visitor <- nba_schedule$visitor

library(sqldf)
score <- c()
for(i in 1:length(home)){
  score[i] <- simulate_game(home[i], visitor[i])
}

nba_schedule$score <- score

nba_schedule <- nba_schedule %>% 
  separate(score, into = c("home_score_sim", "visitor_score_sim"), sep = "-")

nba_schedule$home_score_sim <- as.numeric(nba_schedule$home_score_sim)
nba_schedule$visitor_score_sim <- as.numeric(nba_schedule$visitor_score_sim)

nba_schedule <- nba_schedule %>% 
  mutate(home_win_sim = ifelse(home_score_sim > visitor_score_sim,
                               1, 0),
         home_diff = home_score_sim - visitor_score_sim)




matchups[matchups == "Los Angeles Clippers"] <- "LA Clippers"

home <- matchups$home_teams
visitor <- matchups$away_teams

library(sqldf)
score <- c()
for(i in 1:length(home)){
  score[i] <- simulate_game(home[i], visitor[i])
}

matchups$score <- score

matchups <- matchups %>% 
  separate(score, into = c("home_score_sim", "visitor_score_sim"), sep = "-")

matchups$home_score_sim <- as.numeric(matchups$home_score_sim)
matchups$visitor_score_sim <- as.numeric(matchups$visitor_score_sim)

matchups <- matchups %>% 
  mutate(home_win_sim = ifelse(home_score_sim > visitor_score_sim,
                               1, 0),
         home_diff = home_score_sim - visitor_score_sim)


# ----------

model_stan <- stan_glm(home_win ~ . - home - visitor - home_score - visitor_score -
                                 W.x - W.y - L.x - L.y - home_score_sim - visitor_score_sim,
                               data = nba_schedule, family = binomial(),
                               refresh = 0)

model <- gam(home_win ~ . - home - visitor - home_score - visitor_score -
                       W.x - W.y - L.x - L.y - home_score_sim - visitor_score_sim,
                     data = nba_schedule, family = binomial())

## accuracy check

schedule$estimate <- predict(model_stan, newdata = nba_schedule,
                             type = "response")
schedule$estimate_glm <- predict(model, newdata = nba_schedule,
                                 type = "response")
schedule <- schedule %>% 
  mutate(stan_pred = ifelse(estimate >= .5, 1, 0),
         glm_pred = ifelse(estimate_glm >= .5, 1, 0),
         home_win = ifelse(home_score > visitor_score, 1, 0),
         stan_correct = ifelse(stan_pred == home_win, 1, 0),
         glm_correct = ifelse(glm_pred == home_win, 1, 0))


## overall check

nba_acc <- (sum(schedule$glm_correct, na.rm = T)/nrow(schedule) +
              sum(schedule$stan_correct, na.rm = T)/nrow(schedule))/2

### full model

matchups <- matchups %>% 
  rename("home" = home_teams,
         "visitor" = away_teams)

matchups$home_score <- rep(0, nrow(matchups))
matchups$visitor_score <- rep(0, nrow(matchups))

matchups <- matchups %>% 
  select(-c(Team.x, Team.y))


matchups$estimate <- predict(model_stan, newdata = matchups,
                             type = "response")
matchups$estimate_glm <- predict(model, newdata = matchups,
                                 type = "response")
matchups <- matchups %>% 
  mutate(avg_estimate = 0.5*estimate + 0.5*estimate_glm)



### save
write_rds(matchups, "predictors/nba_matchups.rds")
write_rds(matchups_visitorb2b, "predictors/nba_matchups_visitorb2b.rds")
write_rds(matchups_homeb2b, "predictors/nba_matchups_homeb2b.rds")
write_rds(matchups_bothb2b, "predictors/nba_matchups_bothb2b.rds")
write_rds(nba_acc, "predictors/nba_acc.rds")

