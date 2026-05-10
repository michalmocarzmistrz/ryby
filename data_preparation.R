library(tidyverse)

ryby <- read.csv("data/full_dataset.csv", stringsAsFactors = TRUE)
ryby_tibble <- as_tibble(ryby)

ryby_tibble
# data preparation
# wyciagam tylko te rekordy gdzie był połów i które były w wybranych patchach
ryby_tibble <- ryby_tibble %>%
  filter(y_ == 1 & patch %in% c("12-07", "12-08", "12-09"))

nrow(ryby_tibble)
table(ryby_tibble$patch)

write.csv(ryby_tibble, "data/full_dataset_07-09.csv", row.names = FALSE)