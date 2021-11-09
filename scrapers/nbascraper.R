library(tidyverse)
library(rvest)
library(janitor)
library(rstanarm)
library(gam)
library(lubridate)

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

model_stan <- stan_glm(home_win ~ . - home - visitor - home_score - visitor_score -
                                 W.x - W.y - L.x - L.y,
                               data = nba_schedule, family = binomial(),
                               refresh = 0)

model <- gam(home_win ~ . - home - visitor - home_score - visitor_score -
                       W.x - W.y - L.x - L.y,
                     data = nba_schedule, family = binomial())

## home_b2b

nba_schedule_homeb2b <- left_join(schedule_homeb2b, data_test20,
                          by = c("home" = "Team_name"))
nba_schedule_homeb2b <- left_join(nba_schedule_homeb2b, data_test20,
                          by = c("visitor" = "Team_name"))
nba_schedule_homeb2b <- nba_schedule_homeb2b %>% 
  mutate(home_win = ifelse(home_score > visitor_score, 1, 0))

# create variables for differences
nba_schedule_homeb2b <- nba_schedule_homeb2b %>% 
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

nba_schedule_homeb2b <- nba_schedule_homeb2b %>% 
  select(-c(Team.x, Team.y, Date))

model_stan_homeb2b <- stan_glm(home_win ~ . - home - visitor - home_score - visitor_score -
                         W.x - W.y - L.x - L.y,
                       data = nba_schedule_homeb2b, family = binomial(),
                       refresh = 0)

model_homeb2b <- gam(home_win ~ . - home - visitor - home_score - visitor_score -
               W.x - W.y - L.x - L.y,
             data = nba_schedule_homeb2b, family = binomial())

## visitor_b2b

nba_schedule_homeb2b <- left_join(schedule_homeb2b, data_test20,
                                  by = c("home" = "Team_name"))
nba_schedule_homeb2b <- left_join(nba_schedule_homeb2b, data_test20,
                                  by = c("visitor" = "Team_name"))
nba_schedule_homeb2b <- nba_schedule_homeb2b %>% 
  mutate(home_win = ifelse(home_score > visitor_score, 1, 0))

# create variables for differences
nba_schedule_visitorb2b <- left_join(schedule_visitorb2b, data_test20,
                                  by = c("home" = "Team_name"))
nba_schedule_visitorb2b <- left_join(nba_schedule_visitorb2b, data_test20,
                                  by = c("visitor" = "Team_name"))
nba_schedule_visitorb2b <- nba_schedule_visitorb2b %>% 
  mutate(home_win = ifelse(home_score > visitor_score, 1, 0))

nba_schedule_visitorb2b <- nba_schedule_visitorb2b %>% 
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

nba_schedule_visitorb2b <- nba_schedule_visitorb2b %>% 
  select(-c(Team.x, Team.y, Date))

model_stan_visitorb2b <- stan_glm(home_win ~ . - home - visitor - home_score - visitor_score -
                         W.x - W.y - L.x - L.y,
                       data = nba_schedule_visitorb2b, family = binomial(),
                       refresh = 0)

model_visitorb2b <- gam(home_win ~ . - home - visitor - home_score - visitor_score -
               W.x - W.y - L.x - L.y,
             data = nba_schedule_visitorb2b, family = binomial())

## both_b2b

nba_schedule_bothb2b <- left_join(schedule_bothb2b, data_test20,
                                  by = c("home" = "Team_name"))
nba_schedule_bothb2b <- left_join(nba_schedule_bothb2b, data_test20,
                                  by = c("visitor" = "Team_name"))
nba_schedule_bothb2b <- nba_schedule_bothb2b %>% 
  mutate(home_win = ifelse(home_score > visitor_score, 1, 0))

# create variables for differences
nba_schedule_bothb2b <- left_join(schedule_bothb2b, data_test20,
                                     by = c("home" = "Team_name"))
nba_schedule_bothb2b <- left_join(nba_schedule_bothb2b, data_test20,
                                     by = c("visitor" = "Team_name"))
nba_schedule_bothb2b <- nba_schedule_bothb2b %>% 
  mutate(home_win = ifelse(home_score > visitor_score, 1, 0))

nba_schedule_bothb2b <- nba_schedule_bothb2b %>% 
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

nba_schedule_bothb2b <- nba_schedule_bothb2b %>% 
  select(-c(Team.x, Team.y, Date))

model_stan_bothb2b <- stan_glm(home_win ~ . - home - visitor - home_score - visitor_score -
                         W.x - W.y - L.x - L.y,
                       data = nba_schedule_bothb2b, family = binomial(),
                       refresh = 0)

model_bothb2b <- gam(home_win ~ . - home - visitor - home_score - visitor_score -
               W.x - W.y - L.x - L.y,
             data = nba_schedule_bothb2b, family = binomial())

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

matchups_homeb2b <- matchups
matchups_visitorb2b <- matchups
matchups_bothb2b <- matchups

matchups$estimate <- predict(model_stan, newdata = matchups,
                             type = "response")
matchups$estimate_glm <- predict(model, newdata = matchups,
                                 type = "response")
matchups <- matchups %>% 
  mutate(avg_estimate = 0.5*estimate + 0.5*estimate_glm)


### homeb2b

home_b2b <- rep(1, nrow(matchups_homeb2b))

matchups_homeb2b$b2b_home <- home_b2b

matchups_homeb2b$estimate <- predict(model_stan_homeb2b, newdata = matchups_homeb2b,
                             type = "response")
matchups_homeb2b$estimate_glm <- predict(model, newdata = matchups_homeb2b,
                                 type = "response")
matchups_homeb2b <- matchups_homeb2b %>% 
  mutate(avg_estimate = 0.5*estimate + 0.5*estimate_glm)

### visitorb2b

visitor_b2b <- rep(1, nrow(matchups_visitorb2b))

matchups_visitorb2b$b2b_visitor <- visitor_b2b

matchups_visitorb2b$home_score <- rep(0, nrow(matchups_visitorb2b))
matchups_visitorb2b$visitor_score <- rep(0, nrow(matchups_visitorb2b))

matchups_visitorb2b$estimate <- predict(model_stan_visitorb2b, newdata = matchups_visitorb2b,
                                     type = "response")
matchups_visitorb2b$estimate_glm <- predict(model_visitorb2b, newdata = matchups_visitorb2b,
                                         type = "response")
matchups_visitorb2b <- matchups_visitorb2b %>% 
  mutate(avg_estimate = 0.5*estimate + 0.5*estimate_glm)

### bothb2b

home_b2b <- rep(1, nrow(matchups_bothb2b))
visitor_b2b <- rep(1, nrow(matchups_bothb2b))

matchups_bothb2b$b2b_home <- home_b2b
matchups_bothb2b$b2b_visitor <- visitor_b2b

matchups_bothb2b$home_score <- rep(0, nrow(matchups_bothb2b))
matchups_bothb2b$visitor_score <- rep(0, nrow(matchups_bothb2b))

matchups_bothb2b$estimate <- predict(model_stan_bothb2b, newdata = matchups_bothb2b,
                                        type = "response")
matchups_bothb2b$estimate_glm <- predict(model_bothb2b, newdata = matchups_bothb2b,
                                            type = "response")
matchups_bothb2b <- matchups_bothb2b %>% 
  mutate(avg_estimate = 0.5*estimate + 0.5*estimate_glm)

### save
write_rds(matchups, "predictors/nba_matchups.rds")
write_rds(matchups_visitorb2b, "predictors/nba_matchups_visitorb2b.rds")
write_rds(matchups_homeb2b, "predictors/nba_matchups_homeb2b.rds")
write_rds(matchups_bothb2b, "predictors/nba_matchups_bothb2b.rds")
write_rds(nba_acc, "predictors/nba_acc.rds")

