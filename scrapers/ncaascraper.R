library(rvest)
library(tidyverse)
library(janitor)
library(sqldf)

## Team Stats  --------------------------------
## Traditional

link <- "https://www.sports-reference.com/cbb/seasons/2022-school-stats.html"
page <- read_html(link)
table <- html_table(page)
ncaa_stats_trad <- table[[1]]

ncaa_stats_trad <- ncaa_stats_trad %>%
  row_to_names(row_number = 1) %>% 
  clean_names()

ncaa_stats_trad <- ncaa_stats_trad %>% 
  select(school, fg, fga, "fg3" = x3p, "fg3a" = x3pa, ft_percent, srs, w, l, 
         "conf_w" = w_2, "conf_l" = l_2) %>% 
  filter(school != "School", fg != "Totals")

cols.num <- c("fg", "fga", "fg3", "fg3a", "ft_percent", "srs", "w", "l", 
              "conf_w", "conf_l")

# convert to numeric

ncaa_stats_trad[cols.num] <- sapply(ncaa_stats_trad[cols.num],as.numeric)

ncaa_stats_trad <- ncaa_stats_trad %>% 
  mutate(fg3_percent = fg3/fg3a,
         fg2_percent = (fg - fg3)/(fga - fg3a)) %>% 
  select(school, fg2_percent, fg3_percent, ft_percent, srs, w, l, conf_w, conf_l)

## Four Factors
link <- "https://www.sports-reference.com/cbb/seasons/2022-advanced-school-stats.html"
page <- read_html(link)
table <- html_table(page)
ncaa_stats <- table[[1]]

ncaa_stats <- ncaa_stats %>%
  row_to_names(row_number = 1) %>% 
  clean_names()

ncaa_stats <- ncaa_stats %>% 
  select(school, "ftr" = f_tr, tov_percent, orb_percent, e_fg_percent, "FG3Ar" = x3p_ar, pace) %>% 
  filter(school != "School", ftr != "School Advanced")

cols.num <- c("ftr", "tov_percent", "orb_percent", "e_fg_percent", "FG3Ar", "pace")

# convert to numeric

ncaa_stats[cols.num] <- sapply(ncaa_stats[cols.num],as.numeric)

ncaa_stats <- ncaa_stats %>% 
  mutate(tov_percent = tov_percent/100,
         orb_percent = orb_percent/100,
         FG2Ar = 1 - FG3Ar)

ncaa_stats <- left_join(ncaa_stats, ncaa_stats_trad, by = "school")
ncaa_stats <- ncaa_stats %>% mutate_all(~gsub("NCAA", "", .))

### Opponent Stats --------------------------
## Traditional
link <- "https://www.sports-reference.com/cbb/seasons/2022-opponent-stats.html"
page <- read_html(link)
table <- html_table(page)
ncaa_opp_stats_trad <- table[[1]]

ncaa_opp_stats_trad <- ncaa_opp_stats_trad %>%
  row_to_names(row_number = 1) %>% 
  clean_names()

ncaa_opp_stats_trad <- ncaa_opp_stats_trad %>% 
  select(school, fg, fga, "fg3" = x3p, "fg3a" = x3pa, ft_percent) %>% 
  filter(school != "School", fg != "Opponent")

cols.num <- c("fg", "fga", "fg3", "fg3a", "ft_percent")

# convert to numeric

ncaa_opp_stats_trad[cols.num] <- sapply(ncaa_opp_stats_trad[cols.num],as.numeric)

ncaa_opp_stats_trad <- ncaa_opp_stats_trad %>% 
  mutate(fg3_percent = fg3/fg3a,
         fg2_percent = (fg - fg3)/(fga - fg3a)) %>% 
  select(school, fg2_percent, fg3_percent, ft_percent)

## Four Factors
link <- "https://www.sports-reference.com/cbb/seasons/2022-advanced-opponent-stats.html"
page <- read_html(link)
table <- html_table(page)
ncaa_opp_stats <- table[[1]]

ncaa_opp_stats <- ncaa_opp_stats %>%
  row_to_names(row_number = 1) %>% 
  clean_names()

ncaa_opp_stats <- ncaa_opp_stats %>% 
  select(school, "ftr" = f_tr, tov_percent, orb_percent, e_fg_percent, "FG3Ar" = x3p_ar, pace) %>% 
  filter(school != "School", ftr != "Opponent Advanced")

cols.num <- c("ftr", "tov_percent", "orb_percent", "e_fg_percent", "FG3Ar", "pace")

# convert to numeric

ncaa_opp_stats[cols.num] <- sapply(ncaa_opp_stats[cols.num],as.numeric)

ncaa_opp_stats <- ncaa_opp_stats %>% 
  mutate(tov_percent = tov_percent/100,
         orb_percent = orb_percent/100,
         FG2Ar = 1 - FG3Ar)

ncaa_opp_stats <- left_join(ncaa_opp_stats, ncaa_opp_stats_trad, by = "school")
ncaa_opp_stats <- ncaa_opp_stats %>% mutate_all(~gsub("NCAA", "", .))

#### SIMULATION ---------------------------

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
          shot_prob <- runif(1)
          if(shot_prob <= p_ft){
            
            pts <- pts + 1
            end = 1
            
          }
          
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


avg_3pt_rate_off <- mean(ncaa_stats$FG3Ar)
avg_2pt_rate_off <- mean(ncaa_stats$FG2Ar)
avg_2pt_pct_off <- mean(ncaa_stats$fg2_percent)
avg_3pt_pct_off <- mean(ncaa_stats$fg3_percent)
avg_ft_pct_off <- mean(ncaa_stats$ft_percent)
avg_orb_rate <- mean(ncaa_stats$orb_percent)
avg_tov_rate <- mean(ncaa_stats$tov_percent)
avg_ft_rate <- mean(ncaa_stats$ftr)

simulate_game <- function(home_team, vis_team){
  home_3pt_rate_off <- sqldf(paste("SELECT FG3Ar from ncaa_stats where school is '", 
                                   home_team, "'", sep = ""))[1, 1]
  home_2pt_rate_off <- sqldf(paste("SELECT FG2Ar from ncaa_stats where school is '", 
                                   home_team, "'", sep = ""))[1, 1]
  home_3pt_pct_off <- sqldf(paste("SELECT fg3_percent from ncaa_stats where school is '", 
                                  home_team, "'", sep = ""))[1, 1]
  home_2pt_pct_off <- sqldf(paste("SELECT fg2_percent from ncaa_stats where school is '", 
                                  home_team, "'", sep = ""))[1, 1]
  home_ft_pct_off <- sqldf(paste("SELECT ft_percent from ncaa_stats where school is '", 
                                  home_team, "'", sep = ""))[1, 1]
  
  ### rebound, tov, free throw numbers
  
  home_orb_pct_off <- sqldf(paste("SELECT orb_percent from ncaa_stats where school is '", 
                                 home_team, "'", sep = ""))[1, 1]
  home_orb_pct_def <- sqldf(paste("SELECT orb_percent from ncaa_opp_stats where school is '", 
                                  home_team, "'", sep = ""))[1, 1]
  
  home_ft_rate_off <- sqldf(paste("SELECT ftr from ncaa_stats where school is '", 
                                  home_team, "'", sep = ""))[1, 1]
  home_ft_rate_off <- sqldf(paste("SELECT ftr from ncaa_opp_stats where school is '", 
                                  home_team, "'", sep = ""))[1, 1]
  
  home_tov_pct_off <- sqldf(paste("SELECT tov_percent from ncaa_stats where school is '", 
                                  home_team, "'", sep = ""))[1, 1]
  home_tov_pct_def <- sqldf(paste("SELECT tov_percent from ncaa_opp_stats where school is '", 
                                  home_team, "'", sep = ""))[1, 1]
  
  ## get defense numbers
  home_3pt_rate_def <- sqldf(paste("SELECT FG3Ar from ncaa_opp_stats where school is '", 
                                   home_team, "'", sep = ""))[1, 1]
  home_2pt_rate_def <- sqldf(paste("SELECT FG2Ar from ncaa_opp_stats where school is '", 
                                   home_team, "'", sep = ""))[1, 1]
  home_3pt_pct_def <- sqldf(paste("SELECT fg3_percent from ncaa_opp_stats where school is '", 
                                  home_team, "'", sep = ""))[1, 1]
  home_2pt_pct_def <- sqldf(paste("SELECT fg2_percent from ncaa_opp_stats where school is '", 
                                  home_team, "'", sep = ""))[1, 1]
  home_ft_pct_def <- sqldf(paste("SELECT ft_percent from ncaa_opp_stats where school is '", 
                                 home_team, "'", sep = ""))[1, 1]
  
  ### visitor -------
  # get offence numbers
  
  vis_3pt_rate_off <- sqldf(paste("SELECT FG3Ar from ncaa_stats where school is '", 
                                  vis_team, "'", sep = ""))[1, 1]
  vis_2pt_rate_off <- sqldf(paste("SELECT FG2Ar from ncaa_stats where school is '", 
                                  vis_team, "'", sep = ""))[1, 1]
  vis_3pt_pct_off <- sqldf(paste("SELECT fg3_percent from ncaa_stats where school is '", 
                                 vis_team, "'", sep = ""))[1, 1]
  vis_2pt_pct_off <- sqldf(paste("SELECT fg2_percent from ncaa_stats where school is '", 
                                 vis_team, "'", sep = ""))[1, 1]
  vis_ft_pct_off <- sqldf(paste("SELECT ft_percent from ncaa_stats where school is '", 
                                vis_team, "'", sep = ""))[1, 1]
  
  ### rebound, tov, free throw numbers
  
  vis_orb_pct_off <- sqldf(paste("SELECT orb_percent from ncaa_stats where school is '", 
                                 vis_team, "'", sep = ""))[1, 1]
  vis_orb_pct_def <- sqldf(paste("SELECT orb_percent from ncaa_opp_stats where school is '", 
                                 vis_team, "'", sep = ""))[1, 1]
  
  vis_ft_rate_off <- sqldf(paste("SELECT ftr from ncaa_stats where school is '", 
                                 vis_team, "'", sep = ""))[1, 1]
  vis_ft_rate_off <- sqldf(paste("SELECT ftr from ncaa_opp_stats where school is '", 
                                 vis_team, "'", sep = ""))[1, 1]
  
  vis_tov_pct_off <- sqldf(paste("SELECT tov_percent from ncaa_stats where school is '", 
                                 vis_team, "'", sep = ""))[1, 1]
  vis_tov_pct_def <- sqldf(paste("SELECT tov_percent from ncaa_opp_stats where school is '", 
                                 vis_team, "'", sep = ""))[1, 1]
  
  ## get defense numbers
  vis_3pt_rate_def <- sqldf(paste("SELECT FG3Ar from ncaa_opp_stats where school is '", 
                                  vis_team, "'", sep = ""))[1, 1]
  vis_2pt_rate_def <- sqldf(paste("SELECT FG2Ar from ncaa_opp_stats where school is '", 
                                  vis_team, "'", sep = ""))[1, 1]
  vis_3pt_pct_def <- sqldf(paste("SELECT fg3_percent from ncaa_opp_stats where school is '", 
                                 vis_team, "'", sep = ""))[1, 1]
  vis_2pt_pct_def <- sqldf(paste("SELECT fg2_percent from ncaa_opp_stats where school is '", 
                                 vis_team, "'", sep = ""))[1, 1]
  vis_ft_pct_def <- sqldf(paste("SELECT ft_percent from ncaa_opp_stats where school is '", 
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
  home_orb_pct_off.dif <- sqldf(paste("SELECT orb_percent from ncaa_stats where school is '", 
                                      home_team, "'", sep = ""))[1, 1] - avg_orb_rate
  home_orb_pct_def.dif <- sqldf(paste("SELECT orb_percent from ncaa_opp_stats where school is '", 
                                  home_team, "'", sep = ""))[1, 1] - avg_orb_rate
  
  #ft
  home_ft_rate_off.dif <- sqldf(paste("SELECT ft_percent from ncaa_stats where school is '", 
                                      home_team, "'", sep = ""))[1, 1] - avg_ft_rate
  home_ft_rate_def.dif <- sqldf(paste("SELECT ftr from ncaa_opp_stats where school is '", 
                                      home_team, "'", sep = ""))[1, 1] - avg_ft_rate
  
  #tov
  home_tov_pct_off.dif <- sqldf(paste("SELECT tov_percent from ncaa_stats where school is '", 
                                                          home_team, "'", sep = ""))[1, 1] - avg_tov_rate
  home_tov_pct_def.dif <- sqldf(paste("SELECT tov_percent from ncaa_opp_stats where school is '", 
                                                          home_team, "'", sep = ""))[1, 1] - avg_tov_rate
  
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
  vis_orb_pct_off.dif <- sqldf(paste("SELECT orb_percent from ncaa_stats where school is '", 
                                     vis_team, "'", sep = ""))[1, 1] - avg_orb_rate
  vis_orb_pct_def.dif <- sqldf(paste("SELECT orb_percent from ncaa_opp_stats where school is '", 
                                     vis_team, "'", sep = ""))[1, 1] - avg_orb_rate
  
  #ft
  vis_ft_rate_off.dif <- sqldf(paste("SELECT ft_percent from ncaa_stats where school is '", 
                                     vis_team, "'", sep = ""))[1, 1] - avg_ft_rate
  vis_ft_rate_def.dif <- sqldf(paste("SELECT ftr from ncaa_opp_stats where school is '", 
                                     vis_team, "'", sep = ""))[1, 1] - avg_ft_rate
  
  #tov
  vis_tov_pct_off.dif <- sqldf(paste("SELECT tov_percent from ncaa_stats where school is '", 
                                     vis_team, "'", sep = ""))[1, 1] - avg_tov_rate
  vis_tov_pct_def.dif <- sqldf(paste("SELECT tov_percent from ncaa_opp_stats where school is '", 
                                     vis_team, "'", sep = ""))[1, 1] - avg_tov_rate
  
  
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
  adj_orb_pct <- diff_sumorb_pct + avg_orb_rate
  adj_ft_rate <- diff_sumft_rate + avg_ft_rate
  adj_tov_rate <- diff_sumtov_rate + avg_tov_rate
  
  fga_pct <- 1 - adj_ft_rate - adj_tov_rate
  prob_2pt <- adj_2pt_rate * fga_pct
  prob_3pt <- adj_3pt_rate * fga_pct
  
  
  probs <- c(adj_tov_rate, prob_2pt, prob_3pt, adj_ft_rate)
  p_2pt <- adj_2pt_pct
  p_3pt <- adj_3pt_pct
  p_reb <- adj_orb_pct
  p_ft <- adj_ft_pct
  
  pace_home <- sqldf(paste("SELECT pace from ncaa_stats where school is '",
                      home_team, "'", sep = ""))[1,1]
  pace_away <- sqldf(paste("SELECT pace from ncaa_stats where school is '",
                           vis_team, "'", sep = ""))[1,1]
  pace <- sqrt(pace_home*pace_away)
  #home score
  home_score <- round(simulate_score(probs, p_2pt, p_3pt, 
                                     p_reb, p_ft, pace, 1000), digits = 0)
  
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
  adj_orb_pct <- diff_sumorb_pct + avg_orb_rate
  adj_ft_rate <- diff_sumft_rate + avg_ft_rate
  adj_tov_rate <- diff_sumtov_rate + avg_tov_rate
  
  fga_pct <- 1 - adj_ft_rate - adj_tov_rate
  prob_2pt <- adj_2pt_rate * fga_pct
  prob_3pt <- adj_3pt_rate * fga_pct
  
  
  probs <- c(adj_tov_rate, prob_2pt, prob_3pt, adj_ft_rate)
  p_2pt <- adj_2pt_pct
  p_3pt <- adj_3pt_pct
  p_reb <- adj_orb_pct
  p_ft <- adj_ft_pct
  
  pace <-sqldf(paste("SELECT pace from ncaa_stats where school is '",
                     vis_team, "'", sep = ""))[1,1]
  #visitor score
  visitor_score <- round(simulate_score(probs, p_2pt, p_3pt,
                                        p_reb, p_ft, pace, 1000), digits = 0)
  
  print((paste(home_score, "-", visitor_score, sep = "")))
}

simulate_game("Manhattan", "Fairfield")

write_rds(ncaa_stats, "predictors/ncaa_stats.rds")
write_rds(ncaa_opp_stats, "predictors/ncaa_opp_stats.rds")


### LIN REG --------------------


# # Fix team names
ncaa_stats[ncaa_stats == "Albany (NY)"] <- "Albany"
ncaa_stats[ncaa_stats == "Brigham Young"] <- "BYU"
ncaa_stats[ncaa_stats == "Central Connecticut State"] <- "Central Connecticut"
ncaa_stats[ncaa_stats == "Detroit Mercy"] <- "Detroit"
ncaa_stats[ncaa_stats == "Long Island University"] <- "LIU"
ncaa_stats[ncaa_stats == "Louisiana State"] <- "LSU"
ncaa_stats[ncaa_stats == "Loyola (MD)"] <- "Loyola"
ncaa_stats[ncaa_stats == "Miami (FL)"] <- "Miami"
ncaa_stats[ncaa_stats == "Pennsylvania"] <- "Penn"
ncaa_stats[ncaa_stats == "Pittsburgh"] <- "Pitt"
ncaa_stats[ncaa_stats == "Saint Francis (PA)"] <- "Saint Francis"
ncaa_stats[ncaa_stats == "Saint Mary's (CA)"] <- "Saint Mary's"
ncaa_stats[ncaa_stats == "SIU Edwardsville"] <- "SIU-Edwardsville"
ncaa_stats[ncaa_stats == "Southern Methodist"] <- "SMU"
ncaa_stats[ncaa_stats == "St. Francis (NY)"] <- "St. Francis"
ncaa_stats[ncaa_stats == "St. Thomas (MN)"] <- "St. Thomas"
ncaa_stats[ncaa_stats == "UC Davis"] <- "UC-Davis"
ncaa_stats[ncaa_stats == "UC Irvine"] <- "UC-Irvine"
ncaa_stats[ncaa_stats == "UC Riverside"] <- "UC-Riverside"
ncaa_stats[ncaa_stats == "UC San Diego"] <- "UC-San Diego"
ncaa_stats[ncaa_stats == "Central Florida"] <- "UCF"
ncaa_stats[ncaa_stats == "Connecticut"] <- "UConn"
ncaa_stats[ncaa_stats == "UC Santa Barbara"] <- "UCSB"
ncaa_stats[ncaa_stats == "Massachusetts"] <- "UMass"
ncaa_stats[ncaa_stats == "Illinois-Chicago"] <- "UIC"
ncaa_stats[ncaa_stats == "Massachusetts-Lowell"] <- "UMass-Lowell"
ncaa_stats[ncaa_stats == "Maryland-Baltimore County"] <- "UMBC"
ncaa_stats[ncaa_stats == "North Carolina"] <- "UNC"
ncaa_stats[ncaa_stats == "Nevada-Las Vegas"] <- "UNLV"
ncaa_stats[ncaa_stats == "South Carolina"] <- "USC"
ncaa_stats[ncaa_stats == "South Carolina Upstate"] <- "USC Upstate"
ncaa_stats[ncaa_stats == "Tennessee-Martin"] <- "UT-Martin"
ncaa_stats[ncaa_stats == "Virginia Commonwealth"] <- "VCU"
ncaa_stats[ncaa_stats == "St. John's (NY)"] <- "St. John's"
ncaa_stats[ncaa_stats == "East Tennessee State"] <- "ETSU"
ncaa_stats[ncaa_stats == "Saint Joseph's"] <- "St. Joseph's"

ncaa_opp_stats[ncaa_opp_stats == "Albany (NY)"] <- "Albany"
ncaa_opp_stats[ncaa_opp_stats == "Brigham Young"] <- "BYU"
ncaa_opp_stats[ncaa_opp_stats == "Central Connecticut State"] <- "Central Connecticut"
ncaa_opp_stats[ncaa_opp_stats == "Detroit Mercy"] <- "Detroit"
ncaa_opp_stats[ncaa_opp_stats == "Long Island University"] <- "LIU"
ncaa_opp_stats[ncaa_opp_stats == "Louisiana State"] <- "LSU"
ncaa_opp_stats[ncaa_opp_stats == "Loyola (MD)"] <- "Loyola"
ncaa_opp_stats[ncaa_opp_stats == "Miami (FL)"] <- "Miami"
ncaa_opp_stats[ncaa_opp_stats == "Pennsylvania"] <- "Penn"
ncaa_opp_stats[ncaa_opp_stats == "Pittsburgh"] <- "Pitt"
ncaa_opp_stats[ncaa_opp_stats == "Saint Francis (PA)"] <- "Saint Francis"
ncaa_opp_stats[ncaa_opp_stats == "Saint Mary's (CA)"] <- "Saint Mary's"
ncaa_opp_stats[ncaa_opp_stats == "SIU Edwardsville"] <- "SIU-Edwardsville"
ncaa_opp_stats[ncaa_opp_stats == "Southern Methodist"] <- "SMU"
ncaa_opp_stats[ncaa_opp_stats == "St. Francis (NY)"] <- "St. Francis"
ncaa_opp_stats[ncaa_opp_stats == "St. Thomas (MN)"] <- "St. Thomas"
ncaa_opp_stats[ncaa_opp_stats == "UC Davis"] <- "UC-Davis"
ncaa_opp_stats[ncaa_opp_stats == "UC Irvine"] <- "UC-Irvine"
ncaa_opp_stats[ncaa_opp_stats == "UC Riverside"] <- "UC-Riverside"
ncaa_opp_stats[ncaa_opp_stats == "UC San Diego"] <- "UC-San Diego"
ncaa_opp_stats[ncaa_opp_stats == "Central Florida"] <- "UCF"
ncaa_opp_stats[ncaa_opp_stats == "Connecticut"] <- "UConn"
ncaa_opp_stats[ncaa_opp_stats == "UC Santa Barbara"] <- "UCSB"
ncaa_opp_stats[ncaa_opp_stats == "Massachusetts"] <- "UMass"
ncaa_opp_stats[ncaa_opp_stats == "Illinois-Chicago"] <- "UIC"
ncaa_opp_stats[ncaa_opp_stats == "Massachusetts-Lowell"] <- "UMass-Lowell"
ncaa_opp_stats[ncaa_opp_stats == "Maryland-Baltimore County"] <- "UMBC"
ncaa_opp_stats[ncaa_opp_stats == "North Carolina"] <- "UNC"
ncaa_opp_stats[ncaa_opp_stats == "Nevada-Las Vegas"] <- "UNLV"
ncaa_opp_stats[ncaa_opp_stats == "South Carolina"] <- "USC"
ncaa_opp_stats[ncaa_opp_stats == "South Carolina Upstate"] <- "USC Upstate"
ncaa_opp_stats[ncaa_opp_stats == "Tennessee-Martin"] <- "UT-Martin"
ncaa_opp_stats[ncaa_opp_stats == "Virginia Commonwealth"] <- "VCU"
ncaa_opp_stats[ncaa_opp_stats == "St. John's (NY)"] <- "St. John's"
ncaa_opp_stats[ncaa_opp_stats == "East Tennessee State"] <- "ETSU"
ncaa_opp_stats[ncaa_opp_stats == "Saint Joseph's"] <- "St. Joseph's"

opp_stats <- ncaa_opp_stats
colnames(opp_stats)[2:11] <- paste("opp", colnames(opp_stats)[2:11], sep = "_")

ncaa_ranks <- left_join(ncaa_stats, opp_stats, by = "school")

ncaa_ranks <- ncaa_ranks %>% 
  mutate(ftr = rank(ftr),
         tov_percent = rank(tov_percent),
         orb_percent = rank(orb_percent),
         e_fg_percent = rank(e_fg_percent),
         FG3Ar = rank(FG3Ar),
         pace = rank(pace),
         FG2Ar = rank(FG2Ar),
         fg2_percent = rank(fg2_percent),
         fg3_percent = rank(fg3_percent),
         ft_percent = rank(ft_percent),
         srs = rank(srs),
         opp_ftr = rank(opp_ftr),
         opp_tov_percent = rank(opp_tov_percent),
         opp_orb_percent = rank(opp_orb_percent),
         opp_e_fg_percent = rank(opp_e_fg_percent),
         opp_FG3Ar = rank(opp_FG3Ar),
         opp_pace = rank(opp_pace),
         opp_FG2Ar = rank(opp_FG2Ar),
         opp_fg2_percent = rank(opp_fg2_percent),
         opp_fg3_percent = rank(opp_fg3_percent),
         opp_ft_percent = rank(opp_ft_percent))

## KENPOM

link <- "https://kenpom.com/"
page <- read_html(link)
table <- html_table(page)
kenpom <- table[[1]]

kenpom <- kenpom %>% 
  row_to_names(row_number = 1) %>% 
  clean_names() %>% 
  select("school" = team, rk, adj_em, "adj_off" = adj_o, "adj_def" = adj_d,
         luck, "sos" = adj_em_2) %>% 
  filter(school != "Team", sos != "Strength of Schedule")

num_cols <- colnames(kenpom)[-1]

kenpom[num_cols] <- sapply(kenpom[num_cols], as.numeric)

kenpom$school <- str_replace(kenpom$school, "St.", "State")

kenpom[kenpom == "Connecticut"] <- "UConn"
kenpom[kenpom == "Bowling Green"] <- "Bowling Green State"
kenpom[kenpom == "Pittsburgh"] <- "Pitt"
kenpom[kenpom == "North Carolina"] <- "UNC"
kenpom[kenpom == "Massachusetts"] <- "UMass"
kenpom[kenpom == "Illinois Chicago"] <- "UIC"
kenpom[kenpom == "East Tennessee St."] <- "ETSU"
kenpom[kenpom == "UC Santa Barbara"] <- "UCSB"
kenpom[kenpom == "UC Davis"] <- "UC-Davis"
kenpom[kenpom == "UC Irvine"] <- "UC-Irvine"
kenpom[kenpom == "UC Riverside"] <- "UC-Riverside"
kenpom[kenpom == "UC San Diego"] <- "UC-San Diego"
kenpom[kenpom == "Arkansas Pine Bluff"] <- "Arkansas Pine-Bluff"
kenpom[kenpom == "Bethune Cookman"] <- "Bethune-Cookman"
kenpom[kenpom == "Cal Baptist"] <- "California Baptist"
kenpom[kenpom == "Charleston"] <- "College of Charleston"
kenpom[kenpom == "Detroit Mercy"] <- "Detroit"
kenpom[kenpom == "Long Beach St."] <- "Cal State Long Beach"
kenpom[kenpom == "Louisiana Monroe"] <- "Louisiana-Monroe"
kenpom[kenpom == "Loyola MD"] <- "Loyola"
kenpom[kenpom == "Loyola Chicago"] <- "Loyola (IL)"
kenpom[kenpom == "Maryland Eastern Shore"] <- "Maryland-Eastern Shore"
kenpom[kenpom == "UMass Lowell"] <- "UMass-Lowell"
kenpom[kenpom == "Miami FL"] <- "Miami"
kenpom[kenpom == "Miami OH"] <- "Miami (OH)"
kenpom[kenpom == "Nebraska Omaha"] <- "Omaha"
kenpom[kenpom == "N.C. State"] <- "NC State"
kenpom[kenpom == "Prairie View A&M"] <- "Prairie View"
kenpom[kenpom == "SIU Edwardsville"] <- "SIU-Edwardsville"
kenpom[kenpom == "Southern Miss"] <- "Southern Mississippi"
kenpom[kenpom == "Tennessee Martin"] <- "UT-Martin"
kenpom[kenpom == "UT Rio Grande Valley"] <- "Texas-Rio Grande Valley"
kenpom[kenpom == "Saint Joseph's"] <- "St. Joseph's"

ncaa_stats <- left_join(ncaa_stats, kenpom, by = "school")

## regression
library(rvest)
library(tidyverse)

link <- "https://www.sports-reference.com/cbb/boxscores/index.cgi?month=01&day=8&year=2022"
page <- read_html(link, "cd")

table <- html_nodes(page, "td")
table <- html_text(table)

remove <- c(table[6], table[12], "\n\t\t\t\tFinal\n\t\t\t\t\n\t\t\t", 
            "OT\n\t\t\t", "2OT\n\t\t\t", "3OT\n\t\t\t", "4OT\n\t\t\t",
            "8:30p\n\t\t\t", "\n\t\t\t\t\n\t\t\t")
table <- table[!table %in% remove]

data <- data.frame(matrix(table, ncol = 4, byrow = T))
colnames(data) <- c("home", "home_score", "visitor", "visitor_score")

games <- list()
for(i in 1:31) {
  
  link <- paste("https://www.sports-reference.com/cbb/boxscores/index.cgi?month=01&day=", i, 
                "&year=2022", sep = "")
  page <- read_html(link, "cd")
  
  table <- html_nodes(page, "td")
  table <- html_text(table)
  
  remove <- c(table[6], table[12], "\n\t\t\t\tFinal\n\t\t\t\t\n\t\t\t", 
              "OT\n\t\t\t", "2OT\n\t\t\t", "3OT\n\t\t\t", "4OT\n\t\t\t",
              "8:30p\n\t\t\t", "\n\t\t\t\t\n\t\t\t", "7:00p\n\t\t\t",
              "8:00p\n\t\t\t", "1:00p\n\t\t\t", "4:00p\n\t\t\t", "2:00p\n\t\t\t")
  table <- table[!table %in% remove]
  
  data <- data.frame(matrix(table, ncol = 4, byrow = T))
  colnames(data) <- c("home", "home_score", "visitor", "visitor_score")
  games[[i]] <- data
}
data_jan = do.call(rbind, games)

games <- list()
for(i in 1:28) {
  
  link <- paste("https://www.sports-reference.com/cbb/boxscores/index.cgi?month=02&day=", i, 
                "&year=2022", sep = "")
  page <- read_html(link, "cd")
  
  table <- html_nodes(page, "td")
  table <- html_text(table)
  
  remove <- c(table[6], table[12], "\n\t\t\t\tFinal\n\t\t\t\t\n\t\t\t", 
              "OT\n\t\t\t", "2OT\n\t\t\t", "3OT\n\t\t\t", "4OT\n\t\t\t",
              "8:30p\n\t\t\t", "\n\t\t\t\t\n\t\t\t", "7:00p\n\t\t\t",
              "8:00p\n\t\t\t", "1:00p\n\t\t\t", "4:00p\n\t\t\t", "2:00p\n\t\t\t",
              "10:00p\n\t\t\t")
  table <- table[!table %in% remove]
  
  data <- data.frame(matrix(table, ncol = 4, byrow = T))
  colnames(data) <- c("home", "home_score", "visitor", "visitor_score")
  games[[i]] <- data
}

data_feb = do.call(rbind, games)

games <- list()
for(i in 1:14) {
  
  link <- paste("https://www.sports-reference.com/cbb/boxscores/index.cgi?month=03&day=", i, 
                "&year=2022", sep = "")
  page <- read_html(link, "cd")
  
  table <- html_nodes(page, "td")
  table <- html_text(table)
  
  remove <- c(table[6], table[12], "\n\t\t\t\tFinal\n\t\t\t\t\n\t\t\t", 
              "OT\n\t\t\t", "2OT\n\t\t\t", "3OT\n\t\t\t", "4OT\n\t\t\t",
              "8:30p\n\t\t\t", "\n\t\t\t\t\n\t\t\t", "7:00p\n\t\t\t",
              "8:00p\n\t\t\t", "1:00p\n\t\t\t", "4:00p\n\t\t\t", "2:00p\n\t\t\t",
              "10:00p\n\t\t\t")
  table <- table[!table %in% remove]
  
  data <- data.frame(matrix(table, ncol = 4, byrow = T))
  colnames(data) <- c("home", "home_score", "visitor", "visitor_score")
  games[[i]] <- data
}

data_mar = do.call(rbind, games)

ncaa_schedule <- rbind(data_jan, data_feb, data_mar)

clean_cols <- c("home", "visitor")
num_cols <- c("home_score", "visitor_score")
ncaa_schedule[num_cols] <- sapply(ncaa_schedule[num_cols],as.character)
ncaa_schedule[num_cols] <- sapply(ncaa_schedule[num_cols],as.numeric)

ncaa_schedule$home <- as.character(trimws(gsub("\\s*\\([^\\)]+\\)","", ncaa_schedule$home)))
ncaa_schedule$visitor <- as.character(trimws(gsub("\\s*\\([^\\)]+\\)","", ncaa_schedule$visitor)))

ncaa_schedule$home <- str_trim(ncaa_schedule$home)
ncaa_schedule$visitor <- str_trim(ncaa_schedule$visitor)

ncaa_schedule <- ncaa_schedule %>% 
  drop_na()

ncaa_schedule <- ncaa_schedule %>% 
  mutate(home_diff = home_score - visitor_score,
         total = home_score + visitor_score)

x <- ncaa_sked %>% group_by(home) %>% summarise(mean_diff_home = mean(home_diff))
x <- left_join(x, ncaa_sked %>% group_by(visitor) %>% summarise(mean_diff_vis = mean(home_diff)), by = c("home" = "visitor"))

# ncaa_schedule <- left_join(ncaa_schedule, ncaa_ranks, by = c("home" = "school"))
# ncaa_schedule <- left_join(ncaa_schedule, ncaa_ranks, by = c("visitor" = "school"))
ncaa_schedule <- left_join(ncaa_schedule, ncaa_stats, by = c("home" = "school"))
ncaa_schedule <- left_join(ncaa_schedule, ncaa_stats, by = c("visitor" = "school"))
ncaa_schedule <- left_join(ncaa_schedule, opp_stats, by = c("home" = "school"))
ncaa_schedule <- left_join(ncaa_schedule, opp_stats, by = c("visitor" = "school"))
ncaa_schedule <- left_join(ncaa_schedule, x, by = "home")
ncaa_schedule <- left_join(ncaa_schedule, x, by = c("visitor" = "home")) 
ncaa_schedule <- ncaa_schedule %>% drop_na()

mod <- lm(home_diff ~ . - home_score - visitor_score - total - home - visitor, data = ncaa_schedule)

total_mod <- lm(total ~ . - home_score - visitor_score - total - home - visitor, data = ncaa_schedule)

mod_teams <- lm(home_diff ~ home + visitor + conf_w.x:conf_w.y + conf_l.x:conf_l.y, data = ncaa_schedule)
total_mod_teams <- lm(total ~ home + visitor + conf_w.x:conf_w.y + conf_l.x:conf_l.y, data = ncaa_schedule)

home <- ncaa_stats$school
visitor  <- ncaa_stats$school

matchups <- expand_grid(home, visitor)
ncaa_matchups <- matchups %>% drop_na()

# ncaa_matchups <- left_join(ncaa_matchups, ncaa_ranks, by = c("home" = "school"))
# ncaa_matchups <- left_join(ncaa_matchups, ncaa_ranks, by = c("visitor" = "school"))
ncaa_matchups <- left_join(ncaa_matchups, ncaa_stats, by = c("home" = "school"))
ncaa_matchups <- left_join(ncaa_matchups, ncaa_stats, by = c("visitor" = "school"))
ncaa_matchups <- left_join(ncaa_matchups, opp_stats, by = c("home" = "school"))
ncaa_matchups <- left_join(ncaa_matchups, opp_stats, by = c("visitor" = "school"))
ncaa_matchups <- left_join(ncaa_matchups, x, by = "home")
ncaa_matchups <- left_join(ncaa_matchups, x, by = c("visitor" = "home")) 
ncaa_matchups$home_score <- rep(0, nrow(ncaa_matchups))
ncaa_matchups$visitor_score <- rep(0, nrow(ncaa_matchups))
ncaa_matchups$total <- rep(0, nrow(ncaa_matchups))
ncaa_matchups$home_diff <- rep(0, nrow(ncaa_matchups))

ncaa_matchups <- ncaa_matchups %>% 
  drop_na(rk.x)


ncaa_matchups = subset(ncaa_matchups, !(home %in% c("Cal State Long Beach", "East Tennessee State",
                                                    "Loyola (IL)", "Miami (OH)", "Mississippi", "Saint Peter's",
                                                    "Southern California", "Southern Mississippi",
                                                    "St. John's (NY)", "St. Joseph's",
                                                    "Arkansas-Pine Bluff", "ETSU", "Florida International", 
                                                    "Gardner-Webb", "Grambling", "Kansas City", "Mount St. Mary's",
                                                    "NC State", "Purdue-Fort Wayne", "Saint Francis", "St. Bonaventure",
                                                    "St. Francis", "St. John's", "St. Thomas", "Stanford", "Stephen F. Austin", 
                                                    "Stetson", "Stony Brook", "Texas A&M-Corpus Christi", "Long Beach State")))

ncaa_matchups = subset(ncaa_matchups, !(visitor %in% c("Cal State Long Beach", "East Tennessee State",
                                                       "Loyola (IL)", "Miami (OH)", "Mississippi", "Saint Peter's",
                                                       "Southern California", "Southern Mississippi",
                                                       "St. John's (NY)", "St. Joseph's",
                                                       "Arkansas-Pine Bluff", "ETSU", "Florida International", 
                                                       "Gardner-Webb", "Grambling", "Kansas City", "Mount St. Mary's",
                                                       "NC State", "Purdue-Fort Wayne", "Saint Francis", "St. Bonaventure",
                                                       "St. Francis", "St. John's", "St. Thomas", "Stanford", "Stephen F. Austin", 
                                                       "Stetson", "Stony Brook", "Texas A&M-Corpus Christi", "Long Beach State")))

ncaa_matchups$estimate_stats <- predict(mod, newdata = ncaa_matchups)
ncaa_matchups$estimate_aic <- predict(aic_mod, newdata = ncaa_matchups)
ncaa_matchups$total_estimate_stats <- predict(total_mod, newdata = ncaa_matchups)
ncaa_matchups$estimate_teams <- predict(mod_teams, newdata = ncaa_matchups)
ncaa_matchups$total_estimate_teams <- predict(total_mod_teams, newdata = ncaa_matchups)

ncaa_matchups <- ncaa_matchups %>% 
  mutate(estimate_stats = ifelse(estimate_stats > 0, estimate_stats + 3.5, estimate_stats),
         estimate_aic = ifelse(estimate_aic > 0, estimate_aic + 3.5, estimate_aic))

ncaa_matchups <- ncaa_matchups %>% 
  mutate(estimate = 0.4*estimate_stats + 0.4*estimate_aic + 0.2*estimate_teams,
         total_estimate = 0.6*total_estimate_stats + 0.4*total_estimate_teams)

ncaa_matchups %>% filter(home == "Illinois", visitor == "Indiana") %>% select(estimate_stats, estimate_aic,
                                                                              estimate, estimate_teams)

write_rds(ncaa_matchups, "predictors/ncaa_matchups.rds")

