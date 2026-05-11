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
  filter(str_detect(patch, "^28-"))

ryby_tibble %>%
  filter(y_month == 1) %>%
  count(patch)

#usuwam niepotrzebne zmienne
ryby_tibble <- ryby_tibble %>%
  select(ID,weight,engine_age,length,power,month,y_month,year,patch,nao_index,surf_temp)

#szukanie brakow

# ile braków w każdej zmiennej
colSums(is.na(ryby_tibble))

#wypisuje wszystkie dane z brakami - okazuje sie ze to z pierwszych 2 lat
ryby_braki <- ryby_tibble %>%
  filter(if_any(everything(), is.na)) %>%
  print(n = 100)

#wszystkie dane z roku 2 maja braki
ryby_rok <- ryby_tibble %>%
  filter(year == 2) %>%
  print(n = 100)

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

#transformacja miesiecy na wspolrzedne katowe
ryby_tibble <- ryby_tibble %>%
  mutate(
    month_sin = sin(2 * pi * month / 12),
    month_cos = cos(2 * pi * month / 12)
  )

ryby_tibble %>%
  select(weight, length, power, engine_age) %>%
  pivot_longer(everything(), names_to = "zmienna", values_to = "wartosc") %>%
  ggplot(aes(x = wartosc)) +
  geom_histogram(bins = 50) +
  facet_wrap(~ zmienna, scales = "free")

ryby_tibble %>%
  select(length, power, engine_age) %>%
  cor()

#agregacja

# podział na małe i duże statki
ryby_tibble <- ryby_tibble %>%
  mutate(rozmiar_statku = if_else(length < 25, "maly", "duzy"))

# agregacja, sumowanie i tworzenie zmiennych
ryby_agg <- ryby_tibble %>%
  group_by(patch, year, month) %>%
  summarise(
    # zmienna objaśniana
    total_weight = sum(weight),
    
    # flota
    n_malych = sum(rozmiar_statku == "maly"),
    n_duzych = sum(rozmiar_statku == "duzy"),
    
    # wiek silników
    # średnia długość dla małych i dużych
    mean_length_maly = mean(length[rozmiar_statku == "maly"]),
    mean_length_duzy = mean(length[rozmiar_statku == "duzy"]),
    
    # średni wiek silnika dla małych i dużych
    mean_engine_age_maly = mean(engine_age[rozmiar_statku == "maly"]),
    mean_engine_age_duzy = mean(engine_age[rozmiar_statku == "duzy"]),
    
    # zmienne wspólne dla patcha i miesiąca
    surf_temp = first(surf_temp),
    nao_index = first(nao_index),
    month_sin = first(month_sin),
    month_cos = first(month_cos),
    y_month = first(y_month),
    year = first(year),
    
    .groups = "drop"
  ) %>%
print(n = 200)

ryby_agg %>%
  summarise(
    n_zer = sum(total_weight == 0),
    procent_zer = mean(total_weight == 0) * 100
  )

#UWAGA ! trzeba zbadac i przypadki, w ktorych nic nie zlowiono. bowiem skad mamy wiedziec, ze danego miesiaca
#w danym patchu nic nie lowiono do predykcji?
ryby_agg <- ryby_agg %>%
  filter(total_weight > 0)

library(gridExtra)

ggplot(ryby_agg, aes(x = total_weight)) +
  geom_histogram(bins = 100) +
  ggtitle("Oryginalny")
ggsave("wykresy/histogram_weigth.png")

ggplot(ryby_agg, aes(x = log(total_weight + 1))) +
  geom_histogram(bins = 100) +
  ggtitle("Log")
ggsave("wykresy/histogram_weigth_log.png")

ggplot(ryby_agg, aes(y = log(total_weight + 1))) +
  geom_boxplot() +
  scale_y_log10()
ggsave("wykresy/boxplot_weigth_log.png")

ryby_agg <- ryby_agg %>%
  mutate(log_total_weight = log(total_weight + 1)
  )

write.csv(ryby_agg, "data/dataset_cleaned.csv", row.names = FALSE)