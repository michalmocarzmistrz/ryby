library(tidyverse)
library(corrplot)

ryby <- read.csv("data/dataset_cleaned.csv", stringsAsFactors = TRUE)
ryby <- as_tibble(ryby)

summary(ryby)
n <- nrow(ryby)

ryby[is.na(ryby)] <- 0

#podzial danych

test  <- ryby %>% filter(year >= 15)
train_val <- ryby %>% filter(year < 15)

set.seed(1)

val_idx <- sample(1:n, size = floor(0.15 / 0.85 * nrow(train_val)))

val   <- train_val[val_idx,]
train <- train_val[-val_idx,]

nrow(train) / n
nrow(val)   / n
nrow(test)  / n


summary(train)

#liczymy korelacje itd 

train %>%
  select(log_total_weight, nao_index,surf_temp,y_month,year) %>%
  ggpairs()

train %>%
  select(log_total_weight, n_malych, n_duzych, mean_engine_age_maly, mean_engine_age_duzy, surf_temp, nao_index) %>%
  ggpairs()

train %>%
  select(log_total_weight, month_sin, month_cos, y_month, year, surf_temp,) %>%
  ggpairs()

train %>%
  select(log_total_weight, n_malych, n_duzych, mean_engine_age_maly, mean_length_duzy, mean_length_maly, month_sin, month_cos, y_month, year, nao_index, surf_temp) %>%
  pivot_longer(everything(), names_to = "zmienna", values_to = "wartosc") %>%
  ggplot(aes(x = wartosc)) +
  geom_histogram(bins = 50) +
  facet_wrap(~ zmienna, scales = "free")

library(corrplot)

macierz <- train %>%
  select(total_weight, n_malych, n_duzych,
         mean_length_maly, mean_length_duzy,
         mean_engine_age_maly, mean_engine_age_duzy,
         surf_temp, nao_index, month_sin, month_cos) %>%
  cor(use = "complete.obs")

corrplot(macierz, method = "color", type = "upper",
         addCoef.col = "black", number.cex = 0.7,
         tl.cex = 0.8)

train <- train %>%
  mutate(
    log_n_malych = log(n_malych + 1),
    log_n_duzych = log(n_duzych + 1)
  )

train %>%
  select(log_total_weight, log_n_malych, log_n_duzych) %>%
  ggpairs()

test <- test %>%
  mutate(
    log_n_malych = log(n_malych + 1),
    log_n_duzych = log(n_duzych + 1)
  )

summary(test)
summary(train)

write.csv(ryby, "data/train.csv", row.names = FALSE)
write.csv(ryby, "data/test.csv", row.names = FALSE)