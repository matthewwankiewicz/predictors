library(elo)
library(rstanarm)
library(gam)

schedule2020 <- read_rds("models/nfl_schedule2020.rds")
schedule2021 <- read_rds("models/nfl_schedule2021.rds")
complete_data <- read_rds("models/complete_data.rds")
team_stats <- read_rds("models/team_stats.rds")

schedule2020_21 <- rbind(schedule2020, schedule2021)
sked <- schedule2020_21

## Make ELO Model --------------------------------------------------
results <- elo.run(score(home_score, visitor_score) ~ adjust(home, 10) + visitor,
                   data = schedule2020_21, k = 100)

schedule2020_21$estimate <- predict(results, newdata = schedule2020_21,
                                    type = "response")

schedule2020_21 <- schedule2020_21 %>% 
  mutate(pred = ifelse(estimate > 0.49, 1, 0),
         correct = ifelse(home_win == pred, 1, 0))
sum(schedule2020_21$correct, na.rm = T)/376

## Make GLM Model --------------------------------------------------

model <- gam(home_win ~ defpass_att.x + defpass_att.y + defpass_comp.x + defpass_comp.y +
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
                 data = complete_data, family = binomial())

stan_model <- stan_glm(home_win ~ defpass_att.x + defpass_att.y + defpass_comp.x + defpass_comp.y +
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
                       data = complete_data, family = binomial(), refresh = 0)


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

matchups$model_estimate <- predict.glm(model, newdata = matchups,
                                       type = "response")

matchups$stan_estimate <- predict(stan_model, newdata = matchups,
                                      type = "response")

elo_estimate <- c()
home <- matchups$home
visitor <- matchups$visitor
for(i in 1:length(home)){
  prediction <- predict(results, data.frame("home" = home[i],
                                            "visitor" = visitor[i]))
  elo_estimate <- c(elo_estimate, prediction)
}

matchups$elo_estimate <- elo_estimate

## Accuracy check

complete_data$glm_estimate <- predict(model, newdata = complete_data,
                                     type = "response")

complete_data$stan_estimate <- predict(stan_model, newdata = complete_data,
                                      type = "response")

complete_data <- complete_data %>% 
  mutate(glm_pred = ifelse(glm_estimate > 0.49, 1, 0),
         glm_correct = ifelse(home_win == glm_pred, 1, 0))

nfl_acc <- sum(complete_data$glm_correct, na.rm = T)/390


matchups <- matchups %>% 
  mutate(avg_estimate = 0.5*stan_estimate + 0.5*model_estimate)



write_rds(matchups, "predictors/matchups.rds")
write_rds(nfl_acc, "predictors/nfl_acc.rds")
