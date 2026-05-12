library(tidyverse)
library(corrplot)

ryby <- read.csv("data/dataset_prepared.csv", stringsAsFactors = TRUE)
ryby <- as_tibble(ryby)

summary(ryby)

model <- lm(
  log_total_weight ~ surf_temp + month_sin,
  data = ryby
)

summary(model)

library(performance)

check_model(model)

# Wykresy reszt
ggplot(model, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(x = "Wartości przewidywane", y = "Reszty") +
  ggtitle("Residuals vs. Fitted")

# Histogram reszt
ggplot(model, aes(x = .resid)) +
  geom_histogram(bins = 30, fill = "blue", alpha = 0.7) +
  labs(x = "Reszty", y = "Częstość") +
  ggtitle("Histogram reszt")

# Q-Q plot
ggplot(model, aes(sample = .resid)) +
  stat_qq() +
  stat_qq_line() +
  labs(x = "Teoretyczne kwantyle", y = "Reszty") +
  ggtitle("Wykres Q-Q reszt")
