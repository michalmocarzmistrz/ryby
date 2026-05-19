library(tidyverse)
library(corrplot)
library(GGally)

tips <- read.csv("data/dataset_cleaned.csv", stringsAsFactors = TRUE)
tips <- as_tibble(tips)

#podzial danych

n <- nrow(tips)
set.seed(1)
idx <- sample(1:n)

train <- tips[idx[1:floor(0.70 * n)], ]
val   <- tips[idx[(floor(0.70 * n) + 1):floor(0.85 * n)], ]
test  <- tips[idx[(floor(0.85 * n) + 1):n], ]

nrow(train) / n
nrow(val)   / n
nrow(test)  / n

summary(train)

#####
#liczenie korelacji dla pierwszego zestawu zmiennych

# Q-Q plot dla wybranej zmiennej
#ggplot(train, aes(y = tip)) +
#  geom_boxplot() +
#  theme_minimal()

#train %>%
#  select(tip,distance,geo_distance,duration_min,fare,speed,geo_speed) %>%
#  ggpairs()

#transformacja, ze wzgledu na prawostronny rozklad

train <- train %>%
  mutate(
    log_tip = log(tip + 1),
    log_distance = log(distance + 1),
  )

nrow(train)

#usuwanie outlierów

train <- train %>%
  filter(
    abs(log_tip - mean(log_tip))         < 3 * sd(log_tip),
    abs(log_distance - mean(log_distance)) < 3 * sd(log_distance),
    abs(duration_min - mean(duration_min)) < 3 * sd(duration_min)
  )

nrow(train)

train <- train %>%
  mutate(
    dist_dur = log_distance * duration_min
  )

train %>%
  select(log_tip,log_distance,dist_dur,duration_min) %>%
  ggpairs()

macierz <- train %>%
  select(log_tip,distance,dist_dur,duration_min) %>%
  cor(use = "complete.obs")

corrplot(macierz, method = "color", type = "upper",
         addCoef.col = "black", number.cex = 0.7,
         tl.cex = 0.8)

train %>%
  select(log_tip,fare,speed,geo_speed) %>%
  ggpairs()

#####

#####
#korelacje drugiego zestawu zmiennych

train %>%
  select(log_tip,time,weekday,month,time_sin,time_cos) %>%
  ggpairs()

ggplot(train, aes(x = factor(weekday), y = log_tip)) +
  geom_boxplot(fill = "yellow", alpha = 0.6)

train <- train %>%
  mutate(is_weekday = if_else(weekday >= 3, 1, 0))

ggplot(train, aes(x = factor(month), y = log_tip)) +
  geom_boxplot(fill = "navy", alpha = 0.6)

train <- train %>%
  mutate(is_summer = if_else(month >= 5 & month <= 9, 1, 0))

ggplot(train, aes(x = time, y = log_tip)) +
  geom_point(alpha = 0.3)

train %>%
  select(log_tip,time) %>%
  ggpairs()

train <- train %>%
  mutate(time_of_day = case_when(
    time >= 21 | time < 5  ~ "night",
    time >= 5  & time < 16 ~ "afternoon",
    time >= 16 & time < 21 ~ "evening"
  ) %>% factor())

peak_hour <- 20 

train <- train %>%
  mutate(time_cos = cos(2 * pi * (time - peak_hour) / 24))

####

######
#trzeci zestaw
train %>%
  select(log_tip, points, deliveries_count) %>%
  ggpairs()

# deliveries_count
ggplot(train, aes(x = factor(deliveries_count), y = log_tip)) +
  geom_boxplot(fill = "blue", alpha = 0.6) +
  labs(x = "deliveries_count", title = "log_tip ~ deliveries_count")

train <- train %>%
  mutate(
    log_points = log(points + 1),
    poin_deli = points * deliveries_count
  )

train %>%
  select(log_tip, points, log_points, poin_deli) %>%
  ggpairs()

vars <- train %>% select(log_tip, points, log_points, poin_deli, deliveries_count)

cor_matrix <- cor(vars, method = "spearman", use = "complete.obs")
corrplot(cor_matrix, method = "number", type = "upper")

train$deliveries_f <- as.factor(train$deliveries_count)

####

#transformacje zbioru walidacyjnego i testowego
#1
val <- val %>%
  mutate(
    log_tip = log(tip + 1),
    log_distance = log(distance + 1),
    dist_dur = log_distance * duration_min
  )
test <- test %>%
  mutate(
    log_tip = log(tip + 1),
    log_distance = log(distance + 1),
    dist_dur = log_distance * duration_min
  )
#2
val <- val %>% mutate(is_weekday = if_else(weekday >= 3, 1, 0))
test <- test %>% mutate(is_weekday = if_else(weekday >= 3, 1, 0))
val  <- val  %>% mutate(is_summer = if_else(month >= 5 & month <= 9, 1, 0))
test <- test %>% mutate(is_summer = if_else(month >= 5 & month <= 9, 1, 0))

val <- val %>%
  mutate(time_of_day = case_when(
    time >= 21 | time < 5  ~ "night",
    time >= 5  & time < 16 ~ "afternoon",
    time >= 16 & time < 21 ~ "evening"
  ) %>% factor())

test <- test %>%
  mutate(time_of_day = case_when(
    time >= 21 | time < 5  ~ "night",
    time >= 5  & time < 16 ~ "afternoon",
    time >= 16 & time < 21 ~ "evening"
  ) %>% factor())

val <- val %>%
  mutate(time_cos = cos(2 * pi * (time - peak_hour) / 24))

test <- test %>%
  mutate(time_cos = cos(2 * pi * (time - peak_hour) / 24))

#3
val <- val %>%
  mutate(
    log_points = log(points + 1),
    poin_deli = points * deliveries_count
  )
test <- test %>%
  mutate(
    log_points = log(points + 1),
    poin_deli = points * deliveries_count
  )
val$deliveries_f <- as.factor(val$deliveries_count)
test$deliveries_f <- as.factor(test$deliveries_count)
#

summary(train)

write.csv(train, "data/train.csv", row.names = FALSE)
write.csv(val, "data/val.csv", row.names = FALSE)
write.csv(test, "data/test.csv", row.names = FALSE)