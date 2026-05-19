# 1. ŁADOWANIE BIBLIOTEK I DANYCH
library(tidyverse)
library(car)
library(lmtest)

# Wczytanie zbiorów zapisanych przez kolegę
train <- read.csv("data/train.csv", stringsAsFactors = TRUE)
val   <- read.csv("data/val.csv", stringsAsFactors = TRUE)
test  <- read.csv("data/test.csv", stringsAsFactors = TRUE)

# stworzenie faktorow (zmienne jakosciowe)
prepare_datasets <- function(df) {

  df %>% mutate(

    deliveries_f = factor(deliveries_count, levels = sort(unique(train$deliveries_count))),

    time_of_day = factor(time_of_day, levels = c("afternoon", "evening", "night")),
  )
}

train <- prepare_datasets(train)
val   <- prepare_datasets(val)
test  <- prepare_datasets(test)


# 2. BUDOWA MODELI REGRESJI LINIOWEJ (MODELING) --------------------------------

print("--- ROZPOCZĘCIE ETAPU MODELOWANIA ---")

# --- Model 1: Bazowy (tylko podstawowe zmienne) ---
model_base <- lm(log_tip ~ distance + duration_min + fare + points + speed + weekday, data = train)
print("=== PODSUMOWANIE MODELU BAZOWEGO ===")
print(summary(model_base))

# --- Model 2: Rozszerzony (nowe zmienne od kolegi) ---
model_extended <- lm(log_tip ~ log_distance + duration_min + dist_dur + fare +
                       speed + log_points + poin_deli + deliveries_f + weekday +
                       is_summer + is_weekday + time_of_day, data = train)
print("=== PODSUMOWANIE MODELU ROZSZERZONEGO ===")
print(summary(model_extended))

# --- Model 3: Selekcja krokowa (Stepwise Regression oparta o AIC) ---
model_step <- lm(log_tip ~ log_distance + fare +
                   log_points + deliveries_f +
                   is_summer + is_weekday + time_of_day, data = train)
print("=== PODSUMOWANIE MODELU PO SELEKCJI KROKOWEJ ===")
print(summary(model_step))


# 3. DIAGNOSTYKA MODELU (WERYFIKACJA ZAŁOŻEŃ)

print("--- ROZPOCZĘCIE DIAGNOSTYKI MODELI ---")


plot(model_step)

# a) Współliniowość (Multicollinearity) - wskaźnik VIF
print("=== ANALIZA WSPÓŁLINIOWOŚCI (VIF) ===")

tryCatch({
  vif_extended <- vif(model_extended)
  print("Wskaźniki VIF dla modelu rozszerzonego (model_extended):")
  print(vif_extended)
}, error = function(e) {
  cat("UWAGA: Nie można obliczyć VIF dla model_extended:", conditionMessage(e), "\n")
})

tryCatch({
  vif_step <- vif(model_step)
  print("Wskaźniki VIF dla modelu po selekcji (model_step):")
  print(vif_step)
}, error = function(e) {
  cat("UWAGA: Nie można obliczyć VIF dla model_step:", conditionMessage(e), "\n")
})

vif_vals <- vif(model_step)

# dla modeli z faktorami vif() zwraca macierz - bierzemy trzecią kolumnę i podnosimy do kwadratu
gvif_adj <- vif_vals[, 3]^2

barplot(gvif_adj,
        main = "Współliniowość (GVIF adj.)",
        ylab = "GVIF^(1/(2*Df))²",
        las = 2,          # etykiety pionowo
        col = ifelse(gvif_adj > 10, "red",
                     ifelse(gvif_adj > 5, "orange", "steelblue")))

abline(h = 5,  col = "orange", lty = 2)
abline(h = 10, col = "red",    lty = 2)

# Test Breusha-Pagana (homoskedastyczność)
bp_test <- bptest(model_step)
print(bp_test)

par(mfrow = c(1, 1))

# wykresy
residuals_step <- residuals(model_step)
plot(fitted(model_step), residuals_step,
     main = "Residuals vs Fitted",
     xlab = "Wartości dopasowane",
     ylab = "Reszty",
     pch = 20, col = "steelblue", alpha = 0.3)
abline(h = 0, col = "red", lty = 2)

# Q-Q plot - normalność
qqnorm(residuals_step, main = "Q-Q Plot reszt", pch = 20, col = "steelblue")
qqline(residuals_step, col = "red", lty = 2)

# 4. WALIDACJA I PORÓWNANIE MODELI (VALIDATION)

print("--- ROZPOCZĘCIE WALIDACJI MODELI ---")

# funkcja pomocnicza do wyliczania miar jakości prognoz: R2, RMSE i MAE
evaluate_model <- function(model, data_set, label) {
  res <- tryCatch({

    pred_log <- predict(model, newdata = data_set)

    actual_tip <- data_set$tip
    pred_tip <- exp(pred_log) - 1


    rmse <- sqrt(mean((actual_tip - pred_tip)^2, na.rm = TRUE))
    mae <- mean(abs(actual_tip - pred_tip), na.rm = TRUE)
    r_squared <- summary(model)$r.squared

    data.frame(Model = label, R2 = r_squared, RMSE = rmse, MAE = mae)
  }, error = function(e) {

    data.frame(Model = paste(label, "(BŁĄD DOPASOWANIA)"), R2 = NA, RMSE = NA, MAE = NA)
  })
  return(res)
}

# Porównanie wszystkich trzech modeli na zbiorze walidacyjnym
results_val <- rbind(
  evaluate_model(model_base, val, "Model Bazowy"),
  evaluate_model(model_extended, val, "Model Rozszerzony"),
  evaluate_model(model_step, val, "Model po selekcji")
)

print("=== WYNIKI PORÓWNANIA NA ZBIORZE WALIDACYJNYM ===")
print(results_val)


# 5. predykcja na zbiorze testowym

print("--- OSTATECZNA TESTOWA WALIDACJA ---")
final_test_performance <- evaluate_model(model_step, test, "Model (Test)")

print("=== OSTATECZNY WYNIK NA ZBIORZE TESTOWYM ===")
print(final_test_performance)

