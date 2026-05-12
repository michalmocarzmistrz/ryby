library(tidyverse)
library(corrplot)

ryby <- read.csv("data/dataset_cleaned.csv", stringsAsFactors = TRUE)
ryby <- as_tibble(ryby)

library(GGally)

ryby %>%
  select(log_weight, length, power, surf_temp, nao_index) %>%
  ggpairs()

ryby %>%
  select(log_weight, engine_age, length, power, month_sin, month_cos, y_month, year, nao_index, surf_temp) %>%
  pivot_longer(everything(), names_to = "zmienna", values_to = "wartosc") %>%
  ggplot(aes(x = wartosc)) +
  geom_histogram(bins = 50) +
  facet_wrap(~ zmienna, scales = "free")

ryby <- ryby %>%
  mutate(
    power = log(power + 1),
    engine_age = log(engine_age + 1)
  )

ryby <- ryby %>%
  mutate(
    lenpow = power * length
  )

ryby <- ryby %>%
  mutate(
    log_lenpow = log(lenpow + 1)
  )


ryby %>%
  select(log_weight, length, power, engine_age) %>%
  ggpairs()

ryby %>%
  select(log_weight, lenpow, log_lenpow) %>%
  ggpairs()

ryby %>%
  select(log_weight, month_sin, month_cos, y_month, year) %>%
  ggpairs()

ryby %>%
  select(log_weight, nao_index,surf_temp,y_month,year) %>%
  ggpairs()

ryby %>%
  select(log_weight, engine_age, length, power, month_sin, month_cos, y_month, year, nao_index, surf_temp) %>%
  pivot_longer(everything(), names_to = "zmienna", values_to = "wartosc") %>%
  ggplot(aes(x = wartosc)) +
  geom_histogram(bins = 50) +
  facet_wrap(~ zmienna, scales = "free")

# macierz korelacji
zmienne <- ryby %>%
  select(log_weight, engine_age, length, power, month_sin, month_cos, y_month, year, nao_index, surf_temp)
macierz_cor <- cor(zmienne, use = "complete.obs")

# korelogram
corrplot(macierz_cor, method = "color", type = "upper", 
         addCoef.col = "black", number.cex = 0.7,
         tl.cex = 0.8)

write.csv(ryby_tibble, "data/dataset_prepared.csv", row.names = FALSE)