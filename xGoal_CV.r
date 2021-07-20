
shots_10 <- read.csv("shots_2010.csv")
shots_11 <- read.csv("shots_2011.csv")
shots_12 <- read.csv("shots_2012.csv")
shots_13 <- read.csv("shots_2013.csv")
shots_14 <- read.csv("shots_2014.csv")
shots_15 <- read.csv("shots_2015.csv")
shots_16 <- read.csv("shots_2016.csv")
shots_17 <- read.csv("shots_2017.csv")
shots_18 <- read.csv("shots_2018.csv")
shots_19 <- read.csv("shots_2019.csv")


shots <- rbind(shots_10, shots_11, shots_12, shots_13,
               shots_14, shots_15, shots_16, shots_17,
               shots_18, shots_19)

attach(shots)


# logistic regression model -----------------------------------------------
shots %>% 
  filter(homeSkatersOnIce == 5,
         awaySkatersOnIce == 5)

init_logit_goal <- glm(goal ~ arenaAdjustedShotDistance + shotAngleAdjusted,
                           data = shots,
                           family = "binomial")

pred_goal_outcome <-
  ifelse(init_logit_goal$fitted.values >= 0.3, "Goal", "No Goal")
head(pred_goal_outcome)
table(pred_goal_outcome)

shots %>%
  ggplot(aes(init_logit_goal$fitted.values)) +
  geom_histogram() +
  theme_bw()


nhl_goal_loyo_cv_preds <-
  map_dfr(unique(shots$season),
          function(test_season) {
            
            # Separate out the test and training data:
            test_data <- shots %>%
              filter(season == test_season)
            
            train_data <- shots %>%
              filter(season != test_season)
            
            rebound_model <- glm(goal ~ arenaAdjustedShotDistance + shotAngleAdjusted,
                                 data = train_data,
                                 family = "binomial")
            
            tibble(test_pred_probs = predict(rebound_model, 
                                             newdata = test_data,
                                             type = "response"),
                   test_actual = test_data$goal,
                   test_season = test_season) %>%
              return()
            
          })



nhl_goal_loyo_cv_preds %>%
  mutate(test_pred = as.numeric(test_pred_probs >= 0.3)) %>%
  group_by(test_season) %>%
  summarize(mcr = mean(test_pred != test_actual))

nhl_goal_loyo_cv_preds %>%
  summarize(brier_score = mean((test_actual - test_pred_probs)^2))


nhl_goal_loyo_cv_preds %>%
  mutate(test_pred = as.numeric(test_pred_probs >= 0.3)) %>%
  group_by(test_season) %>%
  summarize(mcr = mean(test_pred != test_actual)) %>%
  ggplot(aes(x = test_season, y = mcr)) +
  geom_bar(stat = "identity", width = .1) +
  geom_point(size = 5) +
  theme_bw() +
  scale_x_continuous(breaks = unique(nhl_goal_loyo_cv_preds$test_season))

summary(init_logit_goal)








