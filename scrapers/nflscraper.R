library(espnscrapeR)
library(tidyverse)
library(rvest)

### GET OFFENSE ------------------------------------------------------------------------------------------

off_pass_team20 <- scrape_team_stats_nfl(season = 2020, 
                                         stats = "passing",
                                         role = "offense")

### GET DEFENSE ------------------------------------------------------------------------------------------

def_pass_team20 <- scrape_team_stats_nfl(season = 2020, 
                                         stats = "passing",
                                         role = "defense")

### CLEAN DEFENSE ------------------------------------------------------------------------------------------

for(i in 5:ncol(def_pass_team20)){
  colnames(def_pass_team20)[i] <-  paste0("def", colnames(def_pass_team20)[i], sep = "")
}

def_pass_team20 <- def_pass_team20 %>% 
  select(-c(role, stat))

### CLEAN OFFENSE ------------------------------------------------------------------------------------------

for(i in 5:ncol(off_pass_team20)){
  colnames(off_pass_team20)[i] <-  paste0("off", colnames(off_pass_team20)[i], sep = "")
}

off_pass_team20 <- off_pass_team20 %>% 
  select(-c(role, stat))


team_stats20 <- left_join(def_pass_team20, off_pass_team20, by = c("team", "season"))

team_stats20[team_stats20 == "Redskins"] <- "Washington"

team_stats20 <- team_stats20 %>% 
  select(-season)

# turn character columns into numeric

def_pass <- team_stats20$defpass_long

for(i in 1:length(def_pass)){
  t <- str_detect(def_pass[i], "T")
  if(t == T){
    def_pass[i] <- substr(def_pass[i], start = 1, stop = 2)
  }
}

def_pass <- as.numeric(def_pass)

team_stats20$defpass_long <- def_pass

#### offense

off_pass <- team_stats20$offpass_long

for(i in 1:length(off_pass)){
  t <- str_detect(off_pass[i], "T")
  if(t == T){
    off_pass[i] <- substr(off_pass[i], start = 1, stop = 2)
  }
}

team_stats20[team_stats20 == "Football"] <- "Washington"

team_stats20$offpass_long <- off_pass

### ADD SoS 

link <- "https://www.pro-football-reference.com/years/2020/"
table <- read_html(link) %>% 
  html_table()

AFC <- table[[1]]
AFC <- AFC %>% 
  filter(Tm != "AFC East",
         Tm != "AFC North",
         Tm != "AFC South", 
         Tm != "AFC West")

NFC <- table[[2]]
NFC <- NFC %>% 
  filter(Tm != "NFC East",
         Tm != "NFC North",
         Tm != "NFC South", 
         Tm != "NFC West")

nfl <- rbind(AFC, NFC)

nfl$Tm <- stringr::str_replace(nfl$Tm, "\\*", "")
nfl$Tm <- stringr::str_replace(nfl$Tm, "\\+", "")

format_name <- function(name){
  last_space <- last(gregexpr(" ", name)[[1]]) + 1
  substr(name, start = last_space, stop = nchar(name))
}

teams <- nfl$Tm
for(i in 1:length(teams)){
  teams[i] <- format_name(teams[i])
}

teams[17] <- "Washington"

nfl$Tm <- teams

nfl <- nfl %>% 
  select(Tm, SoS)

nfl <- nfl %>% 
  rename("team" = Tm)
nfl$SoS <- as.numeric(nfl$SoS)

team_stats20 <- left_join(team_stats20, nfl, by = "team")

### SCRAPE 2020 SCHEDULE -----------------------------------------------------------------------------------

link <- "https://fixturedownload.com/results/nfl-2020"
page <- read_html(link)
table <- html_table(page)

schedule2020 <- table[[1]]


schedule2020 <- schedule2020 %>% 
  rename("home" = `Home Team`,
         "visitor" = `Away Team`) %>% 
  separate(Result, into = c("home_score", "visitor_score"), sep = " - ")

schedule2020$home_score <- as.numeric(schedule2020$home_score)
schedule2020$visitor_score <- as.numeric(schedule2020$visitor_score)

## Format home and away names

h <- schedule2020$home
v <- schedule2020$visitor

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

schedule2020$home <- h
schedule2020$visitor <- v

schedule2020 <- schedule2020 %>% 
  select(home, visitor, home_score, visitor_score)

schedule2020 <- schedule2020 %>% 
  mutate(home_win = ifelse(home_score > visitor_score, 1, 0),
         winner = ifelse(home_score > visitor_score, home, visitor))

## COMBINE THE DATASET --------------------------------------------------------

home_data <- left_join(schedule2020, team_stats20, by = c("home" = "team"))
full_data <- left_join(home_data, team_stats20, by = c("visitor" = "team"))

## CREATE THE MODEL ----------------------------------------------------------


##### REPEAT FOR 2021 ---------------------------------------------------

### GET OFFENSE ------------------------------------------------------------------------------------------

off_pass_team21 <- scrape_team_stats_nfl(season = 2021, 
                                         stats = "passing",
                                         role = "offense")

### GET DEFENSE ------------------------------------------------------------------------------------------

def_pass_team21 <- scrape_team_stats_nfl(season = 2021, 
                                         stats = "passing",
                                         role = "defense")

### CLEAN DEFENSE ------------------------------------------------------------------------------------------

for(i in 5:ncol(def_pass_team21)){
  colnames(def_pass_team21)[i] <-  paste0("def", colnames(def_pass_team21)[i], sep = "")
}

def_pass_team21 <- def_pass_team21 %>% 
  select(-c(role, stat))

### CLEAN OFFENSE ------------------------------------------------------------------------------------------

for(i in 5:ncol(off_pass_team21)){
  colnames(off_pass_team21)[i] <-  paste0("off", colnames(off_pass_team21)[i], sep = "")
}

off_pass_team21 <- off_pass_team21 %>% 
  select(-c(role, stat))


team_stats21 <- left_join(def_pass_team21, off_pass_team21, by = c("team", "season"))

team_stats21[team_stats21 == "Football"] <- "Washington"

team_stats21 <- team_stats21 %>% 
  select(-season)

## char cols into num cols

def_pass <- team_stats21$defpass_long

for(i in 1:length(def_pass)){
  t <- str_detect(def_pass[i], "T")
  if(t == T){
    def_pass[i] <- substr(def_pass[i], start = 1, stop = 2)
  }
}

def_pass <- as.numeric(def_pass)

team_stats21$defpass_long <- def_pass

#### offense

off_pass <- team_stats21$offpass_long

for(i in 1:length(off_pass)){
  t <- str_detect(off_pass[i], "T")
  if(t == T){
    off_pass[i] <- substr(off_pass[i], start = 1, stop = 2)
  }
}

off_pass <- as.numeric(off_pass)

team_stats21$offpass_long <- off_pass

### ADD SoS

link <- "https://www.pro-football-reference.com/years/2021/"
table <- read_html(link) %>% 
  html_table()

AFC <- table[[1]]
AFC <- AFC %>% 
  filter(Tm != "AFC East",
         Tm != "AFC North",
         Tm != "AFC South", 
         Tm != "AFC West")

NFC <- table[[2]]
NFC <- NFC %>% 
  filter(Tm != "NFC East",
         Tm != "NFC North",
         Tm != "NFC South", 
         Tm != "NFC West")

nfl <- rbind(AFC, NFC)

nfl$Tm <- stringr::str_replace(nfl$Tm, "\\*", "")
nfl$Tm <- stringr::str_replace(nfl$Tm, "\\+", "")

format_name <- function(name){
  last_space <- last(gregexpr(" ", name)[[1]]) + 1
  substr(name, start = last_space, stop = nchar(name))
}

teams <- nfl$Tm
for(i in 1:length(teams)){
  teams[i] <- format_name(teams[i])
}

teams[19] <- "Washington"

nfl$Tm <- teams

nfl <- nfl %>% 
  select(Tm, SoS)

nfl <- nfl %>% 
  rename("team" = Tm)
nfl$SoS <- as.numeric(nfl$SoS)

team_stats21 <- left_join(team_stats21, nfl, by = "team")

### SCRAPE 2020 SCHEDULE -----------------------------------------------------------------------------------

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


## COMBINE THE DATASET --------------------------------------------------------

home_data <- left_join(schedule2021, team_stats21, by = c("home" = "team"))
full_data2 <- left_join(home_data, team_stats21, by = c("visitor" = "team"))

complete_data <- rbind(full_data, full_data2)

complete_data$offpass_long.x <- as.numeric(complete_data$offpass_long.x)
complete_data$offpass_long.y <- as.numeric(complete_data$offpass_long.y)

write_rds(complete_data, "models/complete_data.rds")
write_rds(schedule2020, "models/nfl_schedule2020.rds")
write_rds(schedule2021, "models/nfl_schedule2021.rds")
write_rds(team_stats21, "models/team_stats.rds")

