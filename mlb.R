library(tidyverse)
library(rvest)
library(baseballr)

link <- "https://rotogrinders.com/pages/mlb-team-statistics-team-batting-splits-left-right-133574"
table <- read_html(link) %>% 
  html_table()

vs_left <- table[[2]]
vs_right <- table[[3]]

### VS_LEFT --------------------
k_rate <- vs_left$`K%`
bb_rate <- vs_left$`BB%`
hr_rate <- vs_left$`HR%`

for(i in 1:length(k_rate)){
  k_rate[i] <- as.numeric(str_sub(k_rate[i], start = 1,
                                  end = nchar(k_rate[i]) - 1))
}

for(i in 1:length(bb_rate)){
  bb_rate[i] <- as.numeric(str_sub(bb_rate[i], start = 1,
                                  end = nchar(bb_rate[i]) - 1))
}

for(i in 1:length(hr_rate)){
  hr_rate[i] <- as.numeric(str_sub(hr_rate[i], start = 1,
                                   end = nchar(hr_rate[i]) - 1))
}

k_rate <- as.numeric(k_rate)
bb_rate <- as.numeric(bb_rate)
hr_rate <- as.numeric(hr_rate)

vs_left$`HR%` <- hr_rate/100
vs_left$`BB%` <- bb_rate/100
vs_left$`K%` <- k_rate/100

### VS_RIGHT ----------------

k_rate <- vs_right$`K%`
bb_rate <- vs_right$`BB%`
hr_rate <- vs_right$`HR%`

for(i in 1:length(k_rate)){
  k_rate[i] <- as.numeric(str_sub(k_rate[i], start = 1,
                                  end = nchar(k_rate[i]) - 1))
}

for(i in 1:length(bb_rate)){
  bb_rate[i] <- as.numeric(str_sub(bb_rate[i], start = 1,
                                   end = nchar(bb_rate[i]) - 1))
}

for(i in 1:length(hr_rate)){
  hr_rate[i] <- as.numeric(str_sub(hr_rate[i], start = 1,
                                   end = nchar(hr_rate[i]) - 1))
}

k_rate <- as.numeric(k_rate)
bb_rate <- as.numeric(bb_rate)
hr_rate <- as.numeric(hr_rate)

vs_right$`HR%` <- hr_rate/100
vs_right$`BB%` <- bb_rate/100
vs_right$`K%` <- k_rate/100


### mlb schedule

library(retrosheet)

schedule <- retrosheet::get_retrosheet("schedule", 2021)

vs_left <- vs_left %>% 
  mutate(name = case_when(
    TEAM == "LA Angels" ~ "ANA",
    TEAM == "Baltimore" ~ "BAL",
    TEAM == "Boston" ~ "BOS",
    TEAM == "Chicago Cubs" ~ "CHN",
    TEAM == "Chicago White Sox" ~ "CHA",
    TEAM == "Cleveland" ~ "CLE",
    TEAM == "Detroit" ~ "DET",
    TEAM == "Houston" ~ "HOU",
    TEAM == "Kansas City" ~ "KCA",
    TEAM == "Minnesota" ~ "MIN",
    TEAM == "NY Yankees" ~ "NYA",
    TEAM == "NY Mets" ~ "NYN",
    TEAM == "Oakland" ~ "OAK",
    TEAM == "Seattle" ~ "SEA",
    TEAM == "Tampa Bay" ~ "TBA",
    TEAM == "Texas" ~ "TEX",
    TEAM == "Toronto" ~ "TOR",
    TEAM == "Arizona" ~ "ARI",
    TEAM == "Atlanta" ~ "ATL",
    TEAM == "Cincinnati" ~ "CIN",
    TEAM == "Colorado" ~ "COL",
    TEAM == "LA Dodgers" ~ "LAN",
    TEAM == "Miami" ~ "MIA",
    TEAM == "Milwaukee" ~ "MIL",
    TEAM == "Philadelphia" ~ "PHI",
    TEAM == "Pittsburgh" ~ "PIT",
    TEAM == "San Diego" ~ "SDN",
    TEAM == "San Francisco" ~ "SFN",
    TEAM == "St. Louis" ~ "SLN",
    TEAM == "Washington" ~ "WAS"
  ))

vs_right <- vs_right %>% 
  mutate(name = case_when(
    TEAM == "LA Angels" ~ "ANA",
    TEAM == "Baltimore" ~ "BAL",
    TEAM == "Boston" ~ "BOS",
    TEAM == "Chicago Cubs" ~ "CHN",
    TEAM == "Chicago White Sox" ~ "CHA",
    TEAM == "Cleveland" ~ "CLE",
    TEAM == "Detroit" ~ "DET",
    TEAM == "Houston" ~ "HOU",
    TEAM == "Kansas City" ~ "KCA",
    TEAM == "Minnesota" ~ "MIN",
    TEAM == "NY Yankees" ~ "NYA",
    TEAM == "NY Mets" ~ "NYN",
    TEAM == "Oakland" ~ "OAK",
    TEAM == "Seattle" ~ "SEA",
    TEAM == "Tampa Bay" ~ "TBA",
    TEAM == "Texas" ~ "TEX",
    TEAM == "Toronto" ~ "TOR",
    TEAM == "Arizona" ~ "ARI",
    TEAM == "Atlanta" ~ "ATL",
    TEAM == "Cincinnati" ~ "CIN",
    TEAM == "Colorado" ~ "COL",
    TEAM == "LA Dodgers" ~ "LAN",
    TEAM == "Miami" ~ "MIA",
    TEAM == "Milwaukee" ~ "MIL",
    TEAM == "Philadelphia" ~ "PHI",
    TEAM == "Pittsburgh" ~ "PIT",
    TEAM == "San Diego" ~ "SDN",
    TEAM == "San Francisco" ~ "SFN",
    TEAM == "St. Louis" ~ "SLN",
    TEAM == "Washington" ~ "WAS"
  ))

## merge schedules with stats

# left vs left

teams_20 <- Teams %>% 
  filter(yearID == 2019 | yearID == 2020)

teams_20 <- teams_20 %>% 
  mutate(X1B = H - X2B - X3B - HR,
         OBP = round(((H+BB+HBP)/(AB+BB+HBP+SF)), digits = 3),
         SLG = round(((X1B+2*X2B+3*X3B+4*HR)/AB), digits = 3),
         wOBA = round((OBP*2 + SLG)/3, digits = 3),
         FIP = round((13*HRA + 3*BBA - 2*SOA)/((IPouts/3)) + 3, digits = 3),
         BA = round(H/AB, digits = 3),
         BABIP = round((HA - HRA)/(IPouts - SOA - HRA), digits = 3),
         Pythag = round((R^2)/(R^2 + RA^2), digits = 3),
         SO_rate = round(SO/AB, digits = 3),
         WHIP = round((BBA + HA)/(IPouts/3), digits = 3),
         ISO = round(SLG - BA, digits = 3),
         BB_rate_o = round(BB/AB, digits = 3),
         BB_rate_p = round(BBA/IPouts, digits = 3)
  )

teams_20 <- teams_20 %>% 
  select(teamID, name, R, AB, H, X2B, X3B, HR, BB, SO, SB, CS, HBP, SF,
         RA, ER, ERA, CG, SHO, SV, IPouts, HA, HRA, BBA, SOA, E, DP, FP,
         name, X1B, OBP, SLG, wOBA, FIP, BA, BABIP, Pythag, SO_rate, WHIP, 
         ISO, BB_rate_o, BB_rate_p)

## get schedule

link <- "https://fixturedownload.com/results/mlb-2020"
table <- read_html(link) %>% 
  html_table()

schedule2020 <- table[[1]]

schedule2020 <- schedule2020 %>% 
  rename("home" = `Home Team`,
         "visitor" = `Away Team`) %>% 
  separate(Result, into = c("home_score", "visitor_score"), sep = " - ")

schedule2020$home_score <- as.numeric(schedule2020$home_score)
schedule2020$visitor_score <- as.numeric(schedule2020$visitor_score)

## Format home and away names

schedule2020 <- schedule2020 %>% 
  select(home, visitor, home_score, visitor_score) 


names <- teams_20$name
names[13] <- "Los Angeles Angels"
names[43] <- "Los Angeles Angels"

teams_20$name <- names

teams_20 <- teams_20 %>% 
  select(-teamID)

stats <- aggregate(.~name, FUN = mean, teams_20)


## combine data with schedule

home_data <- left_join(schedule2020, stats, by = c("home" = "name"))
full_data <- left_join(home_data, stats, by = c("visitor" = "name"))


full_data <- full_data %>% 
  mutate(home_win = ifelse(home_score > visitor_score, 1, 0))

model_stan <- rstanarm::stan_glm(home_win ~ . - home - visitor - visitor_score -
                    home_score, data = full_data, family = binomial())

model_glm <- glm(home_win ~ . - home - visitor - visitor_score -
                   home_score, data = full_data, family = binomial())

model_gam <- gam::gam(home_win ~ . - home - visitor - visitor_score -
                   home_score, data = full_data, family = binomial())

model_aic <- step(model_glm)
model_bic <- step(model_glm, k = log(nrow(full_data)))

#model <- step(model, k = log(nrow(full_data)))

full_data$stan_estimate <- predict(model_stan, newdata = full_data,
                              type = "response")
full_data$glm_estimate <- predict(model_glm, newdata = full_data,
                                   type = "response")
full_data$gam_estimate <- predict(model_gam, newdata = full_data,
                                   type = "response")
full_data$aic_estimate <- predict(model_aic, newdata = full_data,
                                   type = "response")
full_data$bic_estimate <- predict(model_bic, newdata = full_data,
                                   type = "response")

full_data <- full_data %>% 
  mutate(prediction = ifelse(estimate >= 0.5, 1, 0),
         correct = ifelse(prediction == home_win, 1, 0))



sum(full_data$correct)/nrow(full_data)


