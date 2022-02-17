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



c_stats <- team_stats %>% filter(Position == "C") %>% select(-c(Position, TEAM_NAME)) %>% 
  mutate(PTS_REB_AST = OPP_PTS + OPP_REB + OPP_AST,
         PTS_REB = OPP_PTS + OPP_REB,
         PTS_AST = OPP_PTS + OPP_AST,
         REB_AST = OPP_REB + OPP_AST,
         F_PTS = OPP_PTS + .5*OPP_FG3M + 1.25*OPP_REB + 1.5*OPP_AST + 2*OPP_STL +
           2*OPP_BLK -.5*OPP_TO) %>% 
  relocate(TEAM)
pf_stats <- team_stats %>% filter(Position == "PF") %>% select(-c(Position, TEAM_NAME)) %>% 
  mutate(PTS_REB_AST = OPP_PTS + OPP_REB + OPP_AST,
         PTS_REB = OPP_PTS + OPP_REB,
         PTS_AST = OPP_PTS + OPP_AST,
         REB_AST = OPP_REB + OPP_AST,
         F_PTS = OPP_PTS + .5*OPP_FG3M + 1.25*OPP_REB + 1.5*OPP_AST + 2*OPP_STL +
           2*OPP_BLK -.5*OPP_TO) %>% 
  relocate(TEAM)
sf_stats <- team_stats %>% filter(Position == "SF") %>% select(-c(Position, TEAM_NAME)) %>% 
  mutate(PTS_REB_AST = OPP_PTS + OPP_REB + OPP_AST,
         PTS_REB = OPP_PTS + OPP_REB,
         PTS_AST = OPP_PTS + OPP_AST,
         REB_AST = OPP_REB + OPP_AST,
         F_PTS = OPP_PTS + .5*OPP_FG3M + 1.25*OPP_REB + 1.5*OPP_AST + 2*OPP_STL +
           2*OPP_BLK -.5*OPP_TO) %>% 
  relocate(TEAM)
sg_stats <- team_stats %>% filter(Position == "SG") %>% select(-c(Position, TEAM_NAME)) %>% 
  mutate(PTS_REB_AST = OPP_PTS + OPP_REB + OPP_AST,
         PTS_REB = OPP_PTS + OPP_REB,
         PTS_AST = OPP_PTS + OPP_AST,
         REB_AST = OPP_REB + OPP_AST,
         F_PTS = OPP_PTS + .5*OPP_FG3M + 1.25*OPP_REB + 1.5*OPP_AST + 2*OPP_STL +
           2*OPP_BLK -.5*OPP_TO) %>% 
  relocate(TEAM)
pg_stats <- team_stats %>% filter(Position == "PG") %>% select(-c(Position, TEAM_NAME)) %>% 
  mutate(PTS_REB_AST = OPP_PTS + OPP_REB + OPP_AST,
         PTS_REB = OPP_PTS + OPP_REB,
         PTS_AST = OPP_PTS + OPP_AST,
         REB_AST = OPP_REB + OPP_AST,
         F_PTS = OPP_PTS + .5*OPP_FG3M + 1.25*OPP_REB + 1.5*OPP_AST + 2*OPP_STL +
           2*OPP_BLK -.5*OPP_TO) %>% 
  relocate(TEAM)


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
  mutate(PTS_REB_AST = PTS + REB + AST, 
         PTS_AST = PTS + AST,
         PTS_REB = PTS + REB,
         REB_AST = REB + AST,
         F_PTS = PTS + .5*FG3M + 1.25*REB + 1.5*AST + 2*BLK + 2*STL - 0.5*TO) %>% 
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
  select("team_name" = Team_name, "pace21" = `2021`, "pace20" = `2020`)

possessions[possessions == "LA Clippers"] <- "Los Angeles Clippers"


link <- "https://www.teamrankings.com/nba/trends/ou_trends/"
page <- read_html(link)
table <- page %>% 
  html_table()
table <- table[[1]]

table <- table %>% 
  separate(`Over Record`, c("over", "under", "push"), sep = "-")

over_unders_22 <- table %>% 
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
  )) %>% 
  select("team_name" = Team_name, over, under, "net_diff" = `Total +/-`)


link <- "https://www.teamrankings.com/nba/trends/ou_trends/?range=yearly_2020_2021"
page <- read_html(link)
table <- page %>% 
  html_table()
table <- table[[1]]

table <- table %>% 
  separate(`Over Record`, c("over", "under", "push"), sep = "-")

over_unders_21 <- table %>% 
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
  )) %>% 
   select("team_name" = Team_name, over, under, "net_diff" = `Total +/-`)


write_rds(home_stats, "predictors/home_stats.rds")
write_rds(visitor_stats, "predictors/visitor_stats.rds")
write_rds(advanced_data_home, "predictors/advanced_data_home.rds")
write_rds(advanced_data_visitor, "predictors/advanced_data_visitor.rds")
write_rds(possessions, "predictors/possessions.rds")

#### LOGISTIC ----------------



## 2020

link <- "https://cleaningtheglass.com/stats/league/fourfactors/?season=2020&seasontype=regseason&start=10/1/2020&end=10/15/2021"
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

data_test20[cols.num] <- sapply(data_test20[cols.num],scale)

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

##### schedule

link <- "https://fixturedownload.com/results/nba-2020"
page <- read_html(link)
table <- html_table(page)

schedule2021 <- table[[1]]

schedule2021 <- schedule2021 %>% 
  rename("home" = `Home Team`,
         "visitor" = `Away Team`) %>% 
  separate(Result, into = c("home_score", "visitor_score"), sep = " - ")

schedule2021$home_score <- as.numeric(schedule2021$home_score)
schedule2021$visitor_score <- as.numeric(schedule2021$visitor_score)
schedule2021[schedule2021 == "LA Clippers"] <- "Los Angeles Clippers"
## Format home and away names

schedule2021 <- schedule2021 %>% 
  select(home, visitor, home_score, visitor_score)

nba_schedule <- left_join(schedule2021, data_test20,
                          by = c("home" = "Team_name"))
nba_schedule <- left_join(nba_schedule, data_test20,
                          by = c("visitor" = "Team_name"))
p1 <- possessions %>% 
  select(team_name, "pace" = pace20)

nba_schedule <- left_join(nba_schedule, p1,
                          by = c("home" = "team_name"))

nba_schedule <- left_join(nba_schedule, p1,
                          by = c("visitor" = "team_name"))

nba_schedule <- left_join(nba_schedule, over_unders_21,
                          by = c("home" = "team_name"))

nba_schedule <- left_join(nba_schedule, over_unders_21,
                          by = c("visitor" = "team_name"))

nba_schedule <- nba_schedule %>% 
  mutate(home_win = ifelse(home_score > visitor_score, 1, 0),
         home_diff = home_score - visitor_score, 
         total_score = home_score + visitor_score)

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

matchups20 <- matchups %>% 
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

nba_schedule20 <- nba_schedule %>% 
  select(-c(Team.x, Team.y))

## 2021 --------------------------

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

data_test20[cols.num] <- sapply(data_test20[cols.num],scale)

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

## jan

link <- "https://www.basketball-reference.com/leagues/NBA_2022_games-january.html"
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

schedule_jan <- schedule


## feb
link <- "https://www.basketball-reference.com/leagues/NBA_2022_games-february.html"
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

schedule_feb <- schedule

schedule <- rbind(schedule_oct, schedule_nov, schedule_dec, schedule_jan, schedule_feb)

nba_schedule <- left_join(schedule, data_test20,
                          by = c("home" = "Team_name"))
nba_schedule <- left_join(nba_schedule, data_test20,
                          by = c("visitor" = "Team_name"))

p2 <- possessions %>% 
  select(team_name, "pace" = pace21)

nba_schedule <- left_join(nba_schedule, p2, 
                          by = c("home" = "team_name"))

nba_schedule <- left_join(nba_schedule, p2, 
                          by = c("visitor" = "team_name"))

nba_schedule <- left_join(nba_schedule, over_unders_22,
                          by = c("home" = "team_name"))

nba_schedule <- left_join(nba_schedule, over_unders_22,
                          by = c("visitor" = "team_name"))

nba_schedule <- nba_schedule %>% 
  mutate(home_win = ifelse(home_score > visitor_score, 1, 0),
         home_diff = home_score - visitor_score,
         total_score = home_score + visitor_score)

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
  select(-c(Team.x, Team.y, Date)) %>% 
  filter(home_score > 50, visitor_score > 50)


# combine


total_schedule <- rbind(nba_schedule20, nba_schedule)


total_schedule <- total_schedule %>% 
  mutate(home_diff = home_score - visitor_score,
         home_win = ifelse(home_score > visitor_score, 1, 0),
         total_score = home_score + visitor_score)


model_stan <- gam(home_win ~ . - home_score - visitor_score -
                                 W.x - W.y - L.x - L.y - home_diff,
                               data = total_schedule, family = binomial())

model_spread <- lm(home_diff ~ efg_diff_off*efg_diff_def + efg_diff_off + efg_diff_def +
                   tov_diff_off*tov_diff_def + tov_diff_off + tov_diff_def + 
                     orb_diff_off*orb_diff_def + orb_diff_off + orb_diff_def + home + visitor +
                     pace.x*pace.y + pace.x + pace.y, data = total_schedule)

model_total <- lm(total_score ~ efg_diff_off*efg_diff_def + efg_diff_off + efg_diff_def +
                  tov_diff_off*tov_diff_def + tov_diff_off + tov_diff_def + 
                    orb_diff_off*orb_diff_def + orb_diff_off + orb_diff_def + home + visitor +
                    pace.x*pace.y + pace.x + pace.y +
                    efg_percent_off.x + efg_percent_off.y+efg_percent_def.x + efg_percent_def.y+ 
                    tov_percent_off.x + tov_percent_off.y+ tov_percent_def.x + tov_percent_def.y+
                    orb_percent_off.x + orb_percent_off.y+ orb_percent_def.x + orb_percent_def.y+ 
                    ft_rate_off.x + ft_rate_off.y+ ft_rate_def.x + ft_rate_def.y+ 
                    pts_poss_off.x + pts_poss_off.y+ pts_poss_def.x + pts_poss_def.y +
                    over.x + over.y + under.x + under.y + net_diff.x +
                    net_diff.y, data = total_schedule)

## accuracy check

total_schedule$estimate <- predict(model_stan, newdata = total_schedule,
                             type = "response")
total_schedule$estimate_spread <- predict(model_spread, newdata = total_schedule,
                                 type = "response")
total_schedule$estimate_total <- predict(model_total, newdata = total_schedule,
                                          type = "response")
total_schedule <- total_schedule %>% 
  mutate(stan_pred = ifelse(estimate >= .5, 1, 0),
         home_win = ifelse(home_score > visitor_score, 1, 0),
         stan_correct = ifelse(stan_pred == home_win, 1, 0))


## overall check

nba_acc <- sum(total_schedule$stan_correct, na.rm = T)/nrow(total_schedule %>% drop_na())

### full model

matchups <- matchups %>% 
  rename("home" = home_teams,
         "visitor" = away_teams)

matchups$home_score <- rep(0, nrow(matchups))
matchups$visitor_score <- rep(0, nrow(matchups))
matchups$home_diff <- rep(0, nrow(matchups))
matchups$total_score <- rep(0, nrow(matchups))


matchups <- matchups %>% 
  select(-c(Team.x, Team.y))

matchups <- left_join(matchups, p2, by = c("home" = "team_name"))
matchups <- left_join(matchups, p2, by = c("visitor" = "team_name"))
matchups <- left_join(matchups, over_unders_22, by = c("home" = "team_name"))
matchups <- left_join(matchups, over_unders_22, by = c("visitor" = "team_name"))

matchups$estimate <- predict(model_stan, newdata = matchups,
                             type = "response")
matchups$estimate_spread <- round(predict(model_spread, newdata = matchups,
                                 type = "response"))
matchups$estimate_total <- round(predict(model_total, newdata = matchups,
                                          type = "response"))



### save
write_rds(matchups, "predictors/nba_matchups.rds")
write_rds(matchups_visitorb2b, "predictors/nba_matchups_visitorb2b.rds")
write_rds(matchups_homeb2b, "predictors/nba_matchups_homeb2b.rds")
write_rds(matchups_bothb2b, "predictors/nba_matchups_bothb2b.rds")
write_rds(nba_acc, "predictors/nba_acc.rds")

