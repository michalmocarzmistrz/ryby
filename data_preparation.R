library(tidyverse)

ryby <- read.csv("data/full_dataset.csv", stringsAsFactors = TRUE)
ryby_tibble <- as_tibble(ryby)

# data preparation

#sprawdzam jakie mam zmienne
names(ryby_tibble)
#sprawdzam ile mam obserwacji
nrow(ryby_tibble)

# wyciagam tylko te rekordy gdzie był połów i które były w wybranych patchach
ryby_tibble <- ryby_tibble %>%
  filter(y_ == 1 & str_detect(patch, "^12-"))

nrow(ryby_tibble)
table(ryby_tibble$patch)

#usuwam niepotrzebne zmienne
ryby_tibble <- ryby_tibble %>%
  select(weight,engine_age,length,power,month,y_month,year,patch,nao_index,surf_temp)

write.csv(ryby_tibble, "data/dataset_reduced.csv", row.names = FALSE)

#szukanie brakow

# ile braków w każdej zmiennej
colSums(is.na(ryby_tibble))

#wypisuje wszystkie dane z brakami - okazuje sie ze to z pierwszych 2 lat
ryby_braki <- ryby_tibble %>%
  filter(if_any(everything(), is.na)) %>%
  print(n = Inf)

#wszystkie dane z roku 2 maja braki
ryby_rok <- ryby_tibble %>%
  filter(year == 2) %>%
  print(n = Inf)

#imputacja brakujacych temperatur
ryby_tibble <- ryby_tibble %>%
  group_by(patch, y_month) %>%
  mutate(surf_temp = if_else(
    is.na(surf_temp),
    mean(surf_temp, na.rm = TRUE),
    surf_temp
  )) %>%
  ungroup()

colSums(is.na(ryby_tibble))

#imputacja brakujacych temperatur - dla patchy, ktorych temperatura w danym miesiacu nie byla nigdy zbierana
# korzystamy ze sredniej temperatury w danym miesiacy wsrod wszyskich patchy
ryby_tibble <- ryby_tibble %>%
  group_by(month) %>%
  mutate(surf_temp = if_else(
    is.na(surf_temp),
    mean(surf_temp, na.rm = TRUE),
    surf_temp
  )) %>%
  ungroup()

colSums(is.na(ryby_tibble))

#analiza zmiennej wyjsciowej
summary(ryby_tibble$weight)

library(gridExtra)

ggplot(ryby_tibble, aes(x = weight)) +
  geom_histogram(bins = 100) +
  ggtitle("Oryginalny")
ggsave("wykresy/histogram_weigth.png")

ggplot(ryby_tibble, aes(x = log(weight + 1))) +
  geom_histogram(bins = 100) +
  ggtitle("Log")
ggsave("wykresy/histogram_weigth_log.png")

ggplot(ryby_tibble, aes(y = weight)) +
  geom_boxplot() +
  scale_y_log10()
ggsave("wykresy/boxplot_weigth_log.png")

#dodanie zlogarytmowanej masy do bazy
ryby_tibble <- ryby_tibble %>%
  mutate(log_weight = log(weight + 1))

summary(ryby_tibble$log_weight)

#transformacja miesiecy na wspolrzedne katowe
ryby_tibble <- ryby_tibble %>%
  mutate(
    month_sin = sin(2 * pi * month / 12),
    month_cos = cos(2 * pi * month / 12)
  )

#zamiana kolejnosci - formating
ryby_tibble <- ryby_tibble %>%
  select(log_weight, weight, engine_age, length, power, month, month_sin, month_cos, y_month, year, nao_index, surf_temp, patch, everything())

write.csv(ryby_tibble, "data/dataset_cleaned.csv", row.names = FALSE)