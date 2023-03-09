library(dplyr)
library(lubridate)
library(tidyverse)

df <- read.csv('w_stats/2000.csv')
for (i in 2001:2022){
  str_temp <- paste("w_stats/",i,'.csv',sep="")
  df_temp <- read.csv(str_temp)
  df <- rbind(df,df_temp)
}

df <- df %>%
  mutate(tournament_date = as.Date(as.character(tourney_date),"%Y%m%d"),
         year = lubridate::year(tournament_date))


print("OK 1")


df_subset <- df %>%
  select(tourney_id,tourney_name,surface,round,tourney_level,tourney_date,winner_id,winner_seed,winner_hand,winner_age,winner_ht,
         loser_id,loser_seed,loser_hand,loser_age,loser_ht,winner_rank,loser_rank) %>%
  mutate( #diff_seed = winner_seed - loser_seed,
         age_diff = winner_age - loser_age,
         ht_diff = winner_ht - loser_ht,
         rank_diff = winner_rank - loser_rank)

print("OK 2")


# tournament_date


df_win_experience <- df %>% 
  #filter(tourney_level=='G') %>%
  mutate(round_factor = factor(round,levels = c("R128","R64","R32","R16",'Q1','Q2','Q3',"QF",'SF','F'))) %>%
  arrange(winner_id,tourney_id,tournament_date) %>% 
  mutate(year = lubridate::year(tournament_date)) %>%
  group_by(winner_id) %>%
  mutate(min_year = min(year)) %>%
  mutate(experience = year - min_year) %>%
  ungroup() %>%
  group_by(tourney_id,winner_id) %>%
  arrange(tournament_date) %>%
  mutate(max_round = last(round_factor)) %>%
  select(winner_id,tourney_id,tournament_date,year,min_year,experience,max_round)

print("OK 3")

df_lost_experience <- df %>% 
  mutate(round_factor = factor(round,levels = c("R128","R64","R32","R16",'Q1','Q2','Q3',"QF",'SF','F'))) %>%
  arrange(loser_id,tourney_id,tournament_date) %>% 
  mutate(year = lubridate::year(tournament_date)) %>%
  group_by(loser_id) %>%
  mutate(min_year = min(year)) %>%
  mutate(experience = year - min_year) %>%
  ungroup() %>%
  group_by(tourney_id,loser_id) %>%
  arrange(tournament_date) %>%
  mutate(max_round = last(round_factor)) %>%
  select(loser_id,tourney_id,tournament_date,year,min_year,experience,max_round)

print("OK 4")
df_win_experience_last3 <- df_win_experience %>%
  group_by(winner_id,tourney_id) %>%
  dplyr::summarise(date = max(tournament_date),
            result = last(max_round)) %>%
  ungroup() %>%
  group_by(winner_id) %>%
  arrange(date) %>%
  mutate(last_result = lag(result),
         last_2_result = lag(result,2),
         last_3_result = lag(result,3))

print("OK 5")
df_lost_experience_last3 <- df_lost_experience %>%
  group_by(loser_id,tourney_id) %>%
  dplyr::summarise(date = max(tournament_date),
            result = last(max_round)) %>%
  ungroup() %>%
  group_by(loser_id) %>%
  arrange(date) %>%
  mutate(last_result = lag(result),
         last_2_result = lag(result,2),
         last_3_result = lag(result,3))

print("OK 6")

df_subset_winner <- df_subset %>%
  left_join(df_win_experience %>% select(winner_id,tourney_id,experience)) %>%
  left_join(df_win_experience_last3 %>% select(winner_id,tourney_id,last_result)) %>%
  select(surface,round,tourney_level,age_diff,ht_diff,rank_diff,experience,last_result) %>%
  drop_na() %>%
  mutate(win_lose = 1) 



print("OK 7")

df_subset_loser <- df_subset %>%
  left_join(df_lost_experience %>% select(loser_id,tourney_id,experience)) %>%
  left_join(df_lost_experience_last3 %>% select(loser_id,tourney_id,last_result)) %>%
  select(surface,round,tourney_level,age_diff,ht_diff,rank_diff,experience,last_result) %>%
  drop_na() %>%
  mutate(win_lose = -1) 

print("OK 8")


df_model <- df_subset_winner  %>%
  rbind(df_subset_loser)

print("OK 9")

#### Statistical tests

# library(ggplot2)
# library(ggpubr)
# library(tidyverse)
# library(broom)
# library(AICcmodavg)




set.seed(as.numeric(Sys.time(), units = "secs"))

train_indices <- sample(nrow(df_model), size = round(0.8 * nrow(df_model)), replace = FALSE)
train <- df_model[train_indices, ]
test <- df_model[-train_indices, ]

# lm = lm(win_lose~surface+age_diff+ht_diff+rank_diff+experience+last_result+tourney_level, data = train) #Create the linear regression
# summary(lm)
# pp<-predict(lm, test)

# lmmm = lm(win_lose~surface+tourney_level+age_diff+ht_diff+rank_diff+experience+last_result, data=train) #Create the linear regression
# pp<-predict(lmmm, test)

glmmm = glm(win_lose~surface+tourney_level+age_diff+ht_diff+rank_diff+experience+last_result, data=train,family = binomial,control = list(maxit = 2, epsilon=1)) #Create the logistic regression
summary(glmmm)
pp<-predict(glmmm, test)

# lmm2 <- lm(win_lose~surface+tourney_level+age_diff+ht_diff+rank_diff+experience, data=train) #Create the linear regression
# pp<-predict(lmm2, test)

# lmmm = lm(win_lose~surface+tourney_level+age_diff+ht_diff+rank_diff+experience+last_result, data=train) #Create the linear regression
# pp<-predict(glmmm, test)



actuals_preds <- data.frame(cbind(actuals=test$win_lose, predicteds=pp))  # make actuals_predicteds dataframe.
cor(actuals_preds)  # 82.7%


min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max))  
# => 58.42%, min_max accuracy
mape <- mean(abs((actuals_preds$predicteds - actuals_preds$actuals))/actuals_preds$actuals)  
# => 48.38%, mean absolute percentage deviation