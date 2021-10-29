library(elo)

stats <- read_rds("models/nhl_stats.rds")

## Make ELO Model --------------------------------------------------
results <- elo.run(score(home_score, visitor_score) ~ adjust(home, 5) + visitor,
                   data = stats, k = 25)

model <- glm(home_win ~ . - home - visitor - home_score -
               visitor_score, data = stats)

mod <- step(model, k = log(956))

stats$estimate <- predict.glm(mod, newdata = stats, type = "response")

stats <- stats %>% 
  mutate(prediction = ifelse(estimate >= 0.5, 1, 0),
         correct = ifelse(prediction == home_win, 1, 0))

sum(stats$correct, na.rm = T)/956
