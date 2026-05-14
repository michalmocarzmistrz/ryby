library(tidyverse)
library(corrplot)
library(performance)

train <- read.csv("data/train.csv", stringsAsFactors = TRUE)
train <- as_tibble(train)

test <- read.csv("data/test.csv", stringsAsFactors = TRUE)
test <- as_tibble(test)

summary(train)

model <- lm(
  log_total_weight ~ n_malych + n_duzych + month_cos,
  data = train
)

summary(model)
check_model(model)

# predykcja
test$predicted <- predict(model, newdata = test)

# reszty
test$residuals <- test$log_total_weight - test$predicted

rmse <- sqrt(mean(test$residuals^2))
mae  <- mean(abs(test$residuals))
r2   <- cor(test$log_total_weight, test$predicted)^2

cat("RMSE:", rmse, "\nMAE:", mae, "\nR²:", r2, "\n")

# 4. Wykresy na danych testowych
ggplot(test, aes(x = predicted, y = residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(x = "obserwacje", y = "reszty") +
  ggtitle("scatter plot (test)")


ggplot(test, aes(x = residuals)) +
  geom_histogram(bins = 30, fill = "blue", alpha = 0.7) +
  labs(x = "reszty", y = "czestosc") +
  ggtitle("histogram reszt (test)")

# Q-Q plot
ggplot(test, aes(sample = residuals)) +
  stat_qq() +
  stat_qq_line() +
  labs(x = "kwantyle", y = "reszty") +
  ggtitle("Q-Q plot reszt (test)")