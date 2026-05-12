library(tidyverse)
library(corrplot)

ryby <- read.csv("data/dataset_cleaned.csv", stringsAsFactors = TRUE)
ryby <- as_tibble(ryby)

library(GGally)

summary(ryby)

ryby[is.na(ryby)] <- 0

# 1. Oblicz średnią surf_temp dla każdego roku
mean_temp_per_year <- ryby %>%
  group_by(year) %>%
  summarise(mean_surf_temp = mean(surf_temp, na.rm = TRUE))

# 2. Dodaj kolumnę z średnią temperaturą z poprzedniego roku
ryby <- ryby %>%
  left_join(
    mean_temp_per_year %>%
      mutate(prev_year = year - 2) %>%
      rename(prev_year_mean_temp = mean_surf_temp),
    by = c("year" = "prev_year")
  )

library(zoo)

# 2. Stwórz zmienne opóźnione
ryby <- ryby %>%
  mutate(
    surf_temp_lag1 = lag(surf_temp, n = 1),
    surf_temp_lag12 = lag(surf_temp, n = 12),
    surf_temp_rolling3 = rollmean(surf_temp, k = 3, fill = NA, align = "right"),
    surf_temp_rolling12 = rollmean(surf_temp, k = 12, fill = NA, align = "right")
  )

# 3. Sprawdź korelacje
cor.test(ryby$total_weight, ryby$surf_temp, method = "pearson")
cor.test(ryby$total_weight, ryby$surf_temp_lag1, method = "pearson")
cor.test(ryby$total_weight, ryby$surf_temp_lag12, method = "pearson")
cor.test(ryby$total_weight, ryby$surf_temp_rolling12, method = "pearson")

# 4. Wizualizacja
ggplot(ryby, aes(x = surf_temp_rolling12, y = log_total_weight)) +
  geom_point() +
  geom_smooth(method = "lm", color = "blue") +
  labs(x = "Temperatura (poprzedni rok)", y = "Całkowita waga połowów")

summary(ryby)

ryby %>%
  select(log_total_weight, nao_index,surf_temp,y_month,year,prev_year_mean_temp) %>%
  ggpairs()

ryby %>%
  select(log_total_weight, n_malych, n_duzych, mean_engine_age_maly, mean_engine_age_duzy, surf_temp, nao_index) %>%
  ggpairs()

ryby %>%
  select(log_total_weight, month_sin, month_cos, y_month, year, surf_temp,) %>%
  ggpairs()

ryby %>%
  select(log_total_weight, n_malych, n_duzych, mean_engine_age_maly, mean_length_duzy, mean_length_maly, month_sin, month_cos, y_month, year, nao_index, surf_temp) %>%
  pivot_longer(everything(), names_to = "zmienna", values_to = "wartosc") %>%
  ggplot(aes(x = wartosc)) +
  geom_histogram(bins = 50) +
  facet_wrap(~ zmienna, scales = "free")

library(corrplot)

macierz <- ryby %>%
  select(total_weight, n_malych, n_duzych,
         mean_length_maly, mean_length_duzy,
         mean_engine_age_maly, mean_engine_age_duzy,
         surf_temp, nao_index, month_sin, month_cos) %>%
  cor(use = "complete.obs")

corrplot(macierz, method = "color", type = "upper",
         addCoef.col = "black", number.cex = 0.7,
         tl.cex = 0.8)

ryby <- ryby %>%
  mutate(
    log_n_malych = log(n_malych + 1),
    log_n_duzych = log(n_duzych + 1)
  )

ryby %>%
  select(log_total_weight, log_n_malych, log_n_duzych) %>%
  ggpairs()

write.csv(ryby, "data/dataset_prepared.csv", row.names = FALSE)