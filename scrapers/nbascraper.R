library(tidyverse)
library(rvest)
library(janitor)
library(rstanarm)

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

# Data from https://fixturedownload.com

link <- "https://fixturedownload.com/results/nba-2021"
page <- read_html(link)
table <- page %>% html_table(fill = T)
schedule <- table[[1]]

schedule <- schedule %>% 
  drop_na() %>% 
  filter(Result != "-")

schedule <- schedule %>% 
  rename("home" = `Home Team`,
         "visitor" = `Away Team`) %>% 
  separate(Result, into = c("home_score", "visitor_score"), sep = " - ")

schedule$home_score <- as.numeric(schedule$home_score)
schedule$visitor_score <- as.numeric(schedule$visitor_score)

schedule$home[schedule$home == "LA Clippers"] <- "Los Angeles Clippers"
schedule$visitor[schedule$visitor == "LA Clippers"] <- "Los Angeles Clippers"


# ---------------------------------- MODEL BUILD -----------------------------------------------

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
  select(-c(`Round Number`, Date, Location, Team.x, Team.y))

model <- glm(home_win ~ . - home - visitor - home_score - visitor_score -
               W.x - W.y - L.x - L.y,
             data = nba_schedule, family = binomial())

model_stan <- stan_glm(home_win ~ . - home - visitor - home_score - visitor_score -
                         W.x - W.y - L.x - L.y,
                       data = nba_schedule, family = binomial(),
                       refresh = 0)

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

nba_acc <- sum(schedule$glm_correct, na.rm = T)/64

matchups <- matchups %>% 
  rename("home" = home_teams,
         "visitor" = away_teams)

matchups$home_score <- rep(0, nrow(matchups))
matchups$visitor_score <- rep(0, nrow(matchups))

matchups$estimate <- predict(model_stan, newdata = matchups,
                             type = "response")
matchups$estimate_glm <- predict(model, newdata = matchups,
                                 type = "response")
matchups <- matchups %>% 
  mutate(avg_estimate = 0.5*estimate + 0.5*estimate_glm)

write_rds(matchups, "predictors/nba_matchups.rds")
write_rds(nba_acc, "predictors/nba_acc.rds")
