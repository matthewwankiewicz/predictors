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