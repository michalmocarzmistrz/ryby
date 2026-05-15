library(tidyverse)

#wspl geograficzne na kilometry

distance_miles <- function(lat1, lon1, lat2, lon2)
{
  lat_mid <- (lat1 + lat2) / 2 * pi / 180
  
  dx <- (lon2 - lon1) * 111.32 * cos(lat_mid)
  dy <- (lat2 - lat1) * 111.32
  
  sqrt(dx^2 + dy^2) * 0.621371
}

tips <- read.csv("data/uber_delivery_tips_dataset.csv", stringsAsFactors = TRUE)
tips_tibble <- as_tibble(tips)

# data preparation

#sprawdzam jakie mam zmienne
names(tips_tibble)
#sprawdzam ile mam obserwacji
nrow(tips_tibble)

summary(tips_tibble)

#usuwam niepotrzebne zmienne
tips_tibble <- tips_tibble %>%
  filter(tip > 0)

nrow(tips_tibble)

tips_tibble <- tips_tibble %>%
  select(tip, date, time, weekday, distance ,dropoff_latitude, dropoff_longitude,
         restaurant_latitude, restaurant_longitude, duration_min, points, deliveries_count, fare)

#poprawki odstających obserwacji

tips_tibble <- tips_tibble %>%
  filter(dropoff_longitude < -119.3) 

tips_tibble <- tips_tibble %>%
  filter(tip < 50) 

#dodanie nowych zmiennych

tips_tibble <- tips_tibble %>%
  mutate(month = month(as.Date(date)))

tips_tibble <- tips_tibble %>%
  mutate(
    time_sin = sin(2 * pi * time / 12),
    time_cos = cos(2 * pi * time / 12)
  )

tips_tibble <- tips_tibble %>%
  mutate(geo_distance = distance_miles(restaurant_latitude, restaurant_longitude,
                                     dropoff_latitude, dropoff_longitude))

tips_tibble <- tips_tibble %>%
  mutate(speed = distance / duration_min)

tips_tibble <- tips_tibble %>%
  mutate(geo_speed = geo_distance / duration_min)


summary(tips_tibble)

tips_tibble <- tips_tibble %>%
  select(-date, -dropoff_latitude, -dropoff_longitude, -restaurant_latitude, -restaurant_longitude)

write.csv(tips_tibble, "data/dataset_cleaned.csv", row.names = FALSE)