library(tidyverse)
library(corrplot)

ryby <- read.csv("data/dataset_cleaned.csv", stringsAsFactors = TRUE)
ryby_tibble <- as_tibble(ryby)

library(GGally)

ryby_tibble %>%
  select(log_total_weight, n_malych, n_duzych, mean_engine_age_maly, mean_engine_age_duzy, surf_temp, nao_index) %>%
  ggpairs()

ryby_tibble %>%
  select(log_total_weight, n_malych, n_duzych, mean_engine_age_maly, mean_length_duzy, mean_length_maly, month_sin, month_cos, y_month, year, nao_index, surf_temp) %>%
  pivot_longer(everything(), names_to = "zmienna", values_to = "wartosc") %>%
  ggplot(aes(x = wartosc)) +
  geom_histogram(bins = 50) +
  facet_wrap(~ zmienna, scales = "free")

ryby_tibble %>%
  select(log_total_weight, n_malych, n_duzych, mean_length_maly, mean_length_duzy) %>%
  ggpairs()

ryby_tibble %>%
  select(log_total_weight, month_sin, month_cos, y_month, year) %>%
  ggpairs()

ryby_tibble %>%
  select(log_total_weight, nao_index,surf_temp,y_month,year) %>%
  ggpairs()

library(corrplot)

macierz <- ryby_agg %>%
  select(total_weight, n_malych, n_duzych,
         mean_length_maly, mean_length_duzy,
         mean_engine_age_maly, mean_engine_age_duzy,
         surf_temp, nao_index, month_sin, month_cos) %>%
  cor(use = "complete.obs")

corrplot(macierz, method = "color", type = "upper",
         addCoef.col = "black", number.cex = 0.7,
         tl.cex = 0.8)