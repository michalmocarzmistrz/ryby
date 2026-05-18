
# 1. ŁADOWANIE BIBLIOTEK I DANYCH 
library(tidyverse)
library(car)       
library(lmtest)    

# Wczytanie zbiorów zapisanych przez kolegę
train <- read.csv("data/train.csv", stringsAsFactors = TRUE)
val   <- read.csv("data/val.csv", stringsAsFactors = TRUE)
test  <- read.csv("data/test.csv", stringsAsFactors = TRUE)

# UNIFORMIZACJA I NAPRAWA STRUKTURY DANYCH 
prepare_datasets <- function(df) {

  if("hour" %in% names(df) & !("time" %in% names(df))) {
    df$time <- df$hour
  }
  if("time" %in% names(df) & !("hour" %in% names(df))) {
    df$hour <- df$time
  }
  
  df %>% mutate(
    
    deliveries_f = factor(deliveries_count, levels = sort(unique(train$deliveries_count))),
    
    time_of_day = factor(time_of_day, levels = c("afternoon", "evening", "night")),
   
    is_weekday_3_6 = if_else(weekday >= 3 & weekday <= 6, 1, 0)
  )
}

train <- prepare_datasets(train)
val   <- prepare_datasets(val)
test  <- prepare_datasets(test)


# 2. BUDOWA MODELI REGRESJI LINIOWEJ (MODELING) --------------------------------

print("--- ROZPOCZĘCIE ETAPU MODELOWANIA ---")

# --- Model 1: Bazowy (tylko podstawowe zmienne) ---
model_base <- lm(log_tip ~ distance + duration_min + fare + speed + points + weekday, data = train)
print("=== PODSUMOWANIE MODELU BAZOWEGO ===")
print(summary(model_base))

# --- Model 2: Rozszerzony (nowe zmienne od kolegi) ---
model_extended <- lm(log_tip ~ log_distance + duration_min + dist_dur + fare + 
                       speed + log_points + poin_deli + deliveries_f + 
                       is_summer + is_weekday_3_6 + time_of_day + time_cos, data = train)
print("=== PODSUMOWANIE MODELU ROZSZERZONEGO ===")
print(summary(model_extended))

# --- Model 3: Selekcja krokowa (Stepwise Regression oparta o AIC) ---
model_step <- step(model_extended, direction = "backward", trace = FALSE)
print("=== PODSUMOWANIE MODELU PO SELEKCJI KROKOWEJ ===")
print(summary(model_step))


# 3. DIAGNOSTYKA MODELU (WERYFIKACJA ZAŁOŻEŃ)

print("--- ROZPOCZĘCIE DIAGNOSTYKI MODELI ---")

# Wykresy diagnostyczne (klikaj Enter w konsoli RStudio, aby zobaczyć kolejne)
plot(model_step)

# a) Współliniowość (Multicollinearity) - wskaźnik VIF
print("=== ANALIZA WSPÓŁLINIOWOŚCI (VIF) ===")
vif_extended <- vif(model_extended)
print("Wskaźniki VIF dla pełnego modelu rozszerzonego (model_extended):")
print(vif_extended)

tryCatch({
  vif_values <- vif(model_step)
  print("Wskaźniki VIF dla modelu po selekcji krokowej (model_step):")
  print(vif_values)
}, error = function(e) {
  cat("\nUWAGA: Nie można obliczyć VIF dla model_step.\n")
})

# 4. WALIDACJA I PORÓWNANIE MODELI (VALIDATION)

print("--- ROZPOCZĘCIE WALIDACJI MODELI ---")

# Bezpieczna funkcja pomocnicza do wyliczania miar jakości prognoz: R2, RMSE i MAE
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

# Porównanie wszystkich trzech modeli na zbiorze WALIDACYJNYM
results_val <- rbind(
  evaluate_model(model_base, val, "Model Bazowy"),
  evaluate_model(model_extended, val, "Model Rozszerzony"),
  evaluate_model(model_step, val, "Model po selekcji Stepwise")
)

print("=== WYNIKI PORÓWNANIA NA ZBIORZE WALIDACYJNYM ===")
print(results_val)


# 5. OSTATECZNY SPRAWDZIAN (ZBIÓR TESTOWY) 

print("--- OSTATECZNA TESTOWA WALIDACJA ---")
final_test_performance <- evaluate_model(model_step, test, "Model Ostateczny (Test)")

print("=== OSTATECZNY WYNIK NA ZBIORZE TESTOWYM ===")
print(final_test_performance)

