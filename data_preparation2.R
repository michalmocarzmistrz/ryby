library(tidyverse)
library(corrplot)

ryby <- read.csv("data/dataset_cleaned.csv", stringsAsFactors = TRUE)
ryby_tibble <- as_tibble(ryby)

library(GGally)

ryby_tibble %>%
  select(log_weight, length, power, surf_temp, nao_index) %>%
  ggpairs()

ryby_tibble %>%
  select(log_weight, engine_age, length, power, month_sin, month_cos, y_month, year, nao_index, surf_temp) %>%
  pivot_longer(everything(), names_to = "zmienna", values_to = "wartosc") %>%
  ggplot(aes(x = wartosc)) +
  geom_histogram(bins = 50) +
  facet_wrap(~ zmienna, scales = "free")

ryby_tibble <- ryby_tibble %>%
  mutate(
    power = log(power + 1),
    engine_age = log(engine_age + 1)
  )

ryby_tibble %>%
  select(log_weight, length, power, engine_age) %>%
  ggpairs()

ryby_tibble %>%
  select(log_weight, month_sin, month_cos, y_month, year) %>%
  ggpairs()

ryby_tibble %>%
  select(log_weight, nao_index,surf_temp,y_month,year) %>%
  ggpairs()

ryby_tibble %>%
  select(log_weight, engine_age, length, power, month_sin, month_cos, y_month, year, nao_index, surf_temp) %>%
  pivot_longer(everything(), names_to = "zmienna", values_to = "wartosc") %>%
  ggplot(aes(x = wartosc)) +
  geom_histogram(bins = 50) +
  facet_wrap(~ zmienna, scales = "free")

# macierz korelacji
zmienne <- ryby_tibble %>%
  select(log_weight, engine_age, length, power, month_sin, month_cos, y_month, year, nao_index, surf_temp)
macierz_cor <- cor(zmienne, use = "complete.obs")

# korelogram
corrplot(macierz_cor, method = "color", type = "upper", 
         addCoef.col = "black", number.cex = 0.7,
         tl.cex = 0.8)

