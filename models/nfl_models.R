library(elo)
library(rstanarm)
library(gam)
library(tidyverse)

schedule2020 <- read_rds("models/nfl_schedule2020.rds")
schedule2021 <- read_rds("models/nfl_schedule2021.rds")
complete_data <- read_rds("models/complete_data.rds")
team_stats <- read_rds("models/team_stats.rds")

complete_data <- complete_data[1:553,]


## Make ELO Model --------------------------------------------------
results <- elo.run(score(home_score, visitor_score) ~ adjust(home, 5) + visitor,
                   data = schedule2020, k = 100)

schedule2021$estimate <- predict(results, newdata = schedule2021,
                                    type = "response")

schedule2021 <- schedule2021 %>% 
  mutate(pred = ifelse(estimate > 0.49, 1, 0),
         correct = ifelse(home_win == pred, 1, 0),
         home_diff = home_score - visitor_score,
         total = home_score + visitor_score)
sum(schedule2021$correct, na.rm = T)/256

complete_data <- complete_data %>% 
  mutate(home_diff = home_score - visitor_score,
         total = home_score + visitor_score)

## Make GLM Model --------------------------------------------------

model <- lm(home_diff ~ defpass_att.x + defpass_att.y + defpass_comp.x + defpass_comp.y +
               defpass_comp_pct.x + defpass_comp_pct.y + defyds_att.x + defyds_att.y +
               defpass_yds.x + defpass_yds.y + defpass_td.x + defpass_td.y + 
               defint.x + defint.y + defpass_rating.x + defpass_rating.y + 
               deffirst_downs.x + deffirst_downs.y + defpass_first_pct.x +
               defpass_first_pct.y + defpass_20plus.x + defpass_20plus.y +
               defpass_40plus.x + defpass_40plus.y + defpass_long.x + defpass_long.y +
               defsacks.x + defsacks.y + offpass_att.x + offpass_att.y + offpass_comp.x +
               offpass_comp.y + offpass_comp.x + offpass_comp.y + offpass_comp_pct.x +
               offpass_comp_pct.y + offyds_att.x + offyds_att.y + offpass_yds.x +
               offpass_yds.y + offpass_td.x + offpass_td.y + offint.x + offint.y +
               offpass_rating.x + offpass_rating.y + offfirst_downs.x + offfirst_downs.y + 
               offpass_first_pct.x + offpass_first_pct.y + offpass_20plus.x + offpass_20plus.y +
               offpass_40plus.x + offpass_40plus.y + offpass_long.x + offpass_long.y +
               offsacks.x + offsacks.y + offsack_yds.x + offsack_yds.y + SoS.x + SoS.y, 
                 data = complete_data)

stan_model <- glm(home_win ~ defpass_att.x + defpass_att.y + defpass_comp.x + defpass_comp.y +
                         defpass_comp_pct.x + defpass_comp_pct.y + defyds_att.x + defyds_att.y +
                         defpass_yds.x + defpass_yds.y + defpass_td.x + defpass_td.y + 
                         defint.x + defint.y + defpass_rating.x + defpass_rating.y + 
                         deffirst_downs.x + deffirst_downs.y + defpass_first_pct.x +
                         defpass_first_pct.y + defpass_20plus.x + defpass_20plus.y +
                         defpass_40plus.x + defpass_40plus.y + defpass_long.x + defpass_long.y +
                         defsacks.x + defsacks.y + offpass_att.x + offpass_att.y + offpass_comp.x +
                         offpass_comp.y + offpass_comp.x + offpass_comp.y + offpass_comp_pct.x +
                         offpass_comp_pct.y + offyds_att.x + offyds_att.y + offpass_yds.x +
                         offpass_yds.y + offpass_td.x + offpass_td.y + offint.x + offint.y +
                         offpass_rating.x + offpass_rating.y + offfirst_downs.x + offfirst_downs.y + 
                         offpass_first_pct.x + offpass_first_pct.y + offpass_20plus.x + offpass_20plus.y +
                         offpass_40plus.x + offpass_40plus.y + offpass_long.x + offpass_long.y +
                         offsacks.x + offsacks.y + offsack_yds.x + offsack_yds.y + SoS.x + SoS.y + home + visitor, 
                       data = complete_data, family = binomial())

mod_total <- lm(total ~ defpass_att.x + defpass_att.y + defpass_comp.x + defpass_comp.y +
              defpass_comp_pct.x + defpass_comp_pct.y + defyds_att.x + defyds_att.y +
              defpass_yds.x + defpass_yds.y + defpass_td.x + defpass_td.y + 
              defint.x + defint.y + defpass_rating.x + defpass_rating.y + 
              deffirst_downs.x + deffirst_downs.y + defpass_first_pct.x +
              defpass_first_pct.y + defpass_20plus.x + defpass_20plus.y +
              defpass_40plus.x + defpass_40plus.y + defpass_long.x + defpass_long.y +
              defsacks.x + defsacks.y + offpass_att.x + offpass_att.y + offpass_comp.x +
              offpass_comp.y + offpass_comp.x + offpass_comp.y + offpass_comp_pct.x +
              offpass_comp_pct.y + offyds_att.x + offyds_att.y + offpass_yds.x +
              offpass_yds.y + offpass_td.x + offpass_td.y + offint.x + offint.y +
              offpass_rating.x + offpass_rating.y + offfirst_downs.x + offfirst_downs.y + 
              offpass_first_pct.x + offpass_first_pct.y + offpass_20plus.x + offpass_20plus.y +
              offpass_40plus.x + offpass_40plus.y + offpass_long.x + offpass_long.y +
              offsacks.x + offsacks.y + offsack_yds.x + offsack_yds.y + SoS.x + SoS.y + home + visitor, 
            data = complete_data)

###

model2 <- lm(home_diff ~ defpass_comp.x + defpass_comp_pct.x + defyds_att.x +
               defyds_att.y + defpass_yds.x + defint.y +
               offsack_yds.x + SoS.x + SoS.y + home + visitor, 
             data = complete_data)

complete_data$estimate <- predict(model2, newdata = complete_data)

complete_data <- complete_data %>% 
  mutate(correct = ifelse(sign(home_diff) == sign(estimate), 1, 0),
         mean_diff = round((home_diff - estimate)^2, digits = 2))

sum(complete_data$correct, na.rm = T)/nrow(complete_data %>% drop_na())
sum(complete_data$mean_diff, na.rm = T)

### Get predictions

home_teams <- unique(complete_data$home)
away_teams <- unique(complete_data$visitor)

matchups <- expand_grid(home_teams, away_teams)
matchups <- matchups %>% drop_na()

matchups <- matchups %>% 
  rename("home" = home_teams,
         "visitor" = away_teams)

matchups <- matchups %>% 
  left_join(team_stats, by = c("home" = "team"))
matchups <- matchups %>% 
  left_join(team_stats, by = c("visitor" = "team"))

matchups$model_spread <- predict(model2, newdata = matchups,
                                       type = "response")

matchups$stan_estimate <- predict(stan_model, newdata = matchups,
                                      type = "response")

matchups$total <- predict(mod_total, newdata = matchups,
                                  type = "response")

# elo_estimate <- c()
# home <- matchups$home
# visitor <- matchups$visitor
# for(i in 1:length(home)){
#   prediction <- predict(results, data.frame("home" = home[i],
#                                             "visitor" = visitor[i]))
#   elo_estimate <- c(elo_estimate, prediction)
# }
# 
# matchups$elo_estimate <- elo_estimate

## Accuracy check

complete_data$stan_estimate <- predict(stan_model, newdata = complete_data,
                                      type = "response")

complete_data <- complete_data %>% 
  mutate(glm_pred = ifelse(stan_estimate > 0.49, 1, 0),
         stan_correct = ifelse(home_win == glm_pred, 1, 0))

nfl_acc <- sum(complete_data$stan_correct, na.rm = T)/nrow(complete_data %>% drop_na())


matchups <- matchups %>% 
  mutate(avg_estimate = stan_estimate)

sqldf(paste("SELECT total from matchups WHERE home = '",
            "Rams", "' AND visitor = '", "Cardinals", "'", sep = ""))

write_rds(matchups, "predictors/matchups.rds")
write_rds(nfl_acc, "predictors/nfl_acc.rds")
