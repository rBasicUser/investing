# ===================================================================
# PRONÓSTICOS CON SMA (20, 50, 200)
# Modelo de predicción simple basado en medias móviles
# ===================================================================

library(tidyverse)
library(glue)
library(yaml)
library(TTR)
library(scales)
library(zoo)

params <- read_yaml("params.yaml")
stocks <- params$stocks
days_simulated <- params$days_simulated

# Crear directorios necesarios
dir.create("output/plots", recursive = TRUE, showWarnings = FALSE)
dir.create("models/SMA", recursive = TRUE, showWarnings = FALSE)

# Función para calcular predicciones basadas en SMA
predict_sma <- function(sma20, sma50, sma200, current_price, days_ahead) {
  # Tendencia basada en la convergencia/divergencia de SMAs
  trend_short <- (sma20 - sma50) / sma50
  trend_long <- (sma50 - sma200) / sma200
  
  # Peso combinado de tendencias
  combined_trend <- 0.6 * trend_short + 0.4 * trend_long
  
  # Factor de momentum basado en posición relativa al SMA
  momentum <- (current_price - sma20) / sma20
  
  # Predicción diaria (drift ajustado)
  daily_drift <- combined_trend * 0.001 + momentum * 0.0005
  
  # Generar predicciones
  predictions <- numeric(days_ahead)
  predictions[1] <- current_price * (1 + daily_drift)
  
  for (i in 2:days_ahead) {
    # Decaimiento del momentum
    decay_factor <- exp(-0.05 * i)
    adjusted_drift <- daily_drift * decay_factor
    predictions[i] <- predictions[i - 1] * (1 + adjusted_drift)
  }
  
  return(predictions)
}

# Bucle principal para cada stock
for (s in stocks) {
  cat(glue("\n=== Procesando {s} ===\n"))
  
  # Crear directorio específico para el stock
  dir.create(glue("output/plots/{s}"), recursive = TRUE, showWarnings = FALSE)
  
  # Cargar datos
  train_data <- readRDS(glue("data/processed/returns/{s}_returns.rds"))
  
  # Definir columna de precio ajustado
  col_precio <- glue("{s}.Adjusted")
  
  # Renombrar columna de precio para simplificar
  train_data <- train_data %>%
    rename(close = !!sym(col_precio)) %>%
    arrange(date)
  
  # Calcular SMAs y métricas
  train_data <- train_data %>%
    mutate(
      SMA20 = TTR::SMA(close, n = 20),
      SMA50 = TTR::SMA(close, n = 50),
      SMA200 = TTR::SMA(close, n = 200),
      
      # Bandas de Bollinger (±1 desviación estándar)
      SD20 = zoo::rollapply(
        close,
        width = 20,
        FUN = sd,
        fill = NA,
        align = "right"
      ),
      Lower20 = SMA20 - SD20,
      Higher20 = SMA20 + SD20,
      
      # Señales de trading
      Above_SMA20 = close > SMA20,
      Above_SMA50 = close > SMA50,
      Above_SMA200 = close > SMA200,
      
      # Convergencia/divergencia
      SMA_convergence = abs(SMA20 - SMA50) / SMA50,
      Long_trend = (SMA50 - SMA200) / SMA200
    ) %>%
    filter(!is.na(SMA200)) # Eliminar primeros 200 días sin SMA200
  
  # Obtener valores actuales para predicción
  current_price <- last(train_data$close)
  current_sma20 <- last(train_data$SMA20)
  current_sma50 <- last(train_data$SMA50)
  current_sma200 <- last(train_data$SMA200)
  
  cat(glue("Precio actual: ${round(current_price, 2)}\n"))
  cat(glue("SMA20: ${round(current_sma20, 2)}\n"))
  cat(glue("SMA50: ${round(current_sma50, 2)}\n"))
  cat(glue("SMA200: ${round(current_sma200, 2)}\n"))
  
  # Generar predicciones
  sma_predictions <- predict_sma(
    current_sma20,
    current_sma50,
    current_sma200,
    current_price,
    days_simulated
  )
  
  # Calcular bandas de confianza (basadas en volatilidad histórica de 20 días)
  recent_returns <- diff(log(tail(train_data$close, 21)))
  volatility <- sd(recent_returns, na.rm = TRUE)
  
  # Bandas de confianza (±1.96 * volatilidad acumulativa)
  cumulative_vol <- volatility * sqrt(1:days_simulated)
  upper_band <- sma_predictions * exp(1.96 * cumulative_vol)
  lower_band <- sma_predictions * exp(-1.96 * cumulative_vol)
  
  # Generar fechas de predicción
  last_date <- max(as.Date(train_data$date))
  pred_dates <- seq(last_date + 1, by = "day", length.out = days_simulated)
  
  # GRÁFICAS EN BASE R
  png(glue("output/plots/{s}/SMA_{s}.png"), width = 800, height = 800)
  par(mfrow = c(2, 2), mar = c(4, 4, 3, 2))
  
  # 1. Serie histórica con SMAs
  n_hist <- min(252, nrow(train_data))
  hist_data <- tail(train_data, n_hist)
  
  plot(
    as.Date(hist_data$date),
    hist_data$close,
    type = "l",
    lwd = 2,
    main = glue("Serie Histórica y SMAs - {s}"),
    xlab = "Fecha",
    ylab = "Precio ($)",
    ylim = range(
      c(hist_data$close, hist_data$SMA20, hist_data$SMA50, hist_data$SMA200),
      na.rm = TRUE
    )
  )
  
  lines(as.Date(hist_data$date), hist_data$SMA20, col = "red", lwd = 1.5)
  lines(as.Date(hist_data$date), hist_data$SMA50, col = "blue", lwd = 1.5)
  lines(as.Date(hist_data$date), hist_data$SMA200, col = "darkgreen", lwd = 1.5)
  
  legend(
    "topleft",
    c("Precio", "SMA20", "SMA50", "SMA200"),
    col = c("black", "red", "blue", "darkgreen"),
    lwd = c(2, 1.5, 1.5, 1.5)
  )
  
  # 2. Predicciones SMA con bandas
  all_dates <- c(tail(as.Date(hist_data$date), 30), pred_dates)
  all_prices <- c(tail(hist_data$close, 30), rep(NA, days_simulated))
  
  plot(
    all_dates,
    all_prices,
    type = "l",
    lwd = 2,
    main = glue("Predicciones SMA - {s}"),
    xlab = "Fecha",
    ylab = "Precio ($)",
    ylim = range(
      c(all_prices, sma_predictions, upper_band, lower_band),
      na.rm = TRUE
    )
  )
  
  # Línea vertical separando histórico de predicción
  abline(v = last_date, col = "red", lty = 2)
  
  # Predicciones y bandas
  lines(pred_dates, sma_predictions, col = "blue", lwd = 2)
  polygon(
    c(pred_dates, rev(pred_dates)),
    c(upper_band, rev(lower_band)),
    col = alpha("lightblue", 0.3),
    border = NA
  )
  lines(pred_dates, upper_band, col = "gray", lty = 3)
  lines(pred_dates, lower_band, col = "gray", lty = 3)
  
  legend(
    "topleft",
    c("Histórico", "Predicción SMA", "IC 95%"),
    col = c("black", "blue", "lightblue"),
    lwd = c(2, 2, 5)
  )
  
  # 3. Análisis de convergencia SMA
  plot(
    as.Date(hist_data$date),
    hist_data$SMA_convergence,
    type = "l",
    lwd = 2,
    main = glue("Convergencia SMA20-SMA50 - {s}"),
    xlab = "Fecha",
    ylab = "Convergencia (%)",
    col = "purple"
  )
  abline(h = 0.05, col = "red", lty = 2)
  abline(h = -0.05, col = "red", lty = 2)
  
  # Colorear fondo según convergencia/divergencia
  convergent_periods <- which(abs(hist_data$SMA_convergence) < 0.05)
  if (length(convergent_periods) > 0) {
    rect(
      as.Date(hist_data$date)[min(convergent_periods)],
      par("usr")[3],
      as.Date(hist_data$date)[max(convergent_periods)],
      par("usr")[4],
      col = alpha("green", 0.1),
      border = NA
    )
  }
  
  # 4. Bandas de Bollinger
  plot(
    as.Date(hist_data$date),
    hist_data$close,
    type = "l",
    lwd = 2,
    main = glue("Bandas de Bollinger - {s}"),
    xlab = "Fecha",
    ylab = "Precio ($)",
    ylim = range(
      c(hist_data$close, hist_data$Higher20, hist_data$Lower20),
      na.rm = TRUE
    )
  )
  
  lines(as.Date(hist_data$date), hist_data$SMA20, col = "blue", lwd = 1.5)
  lines(as.Date(hist_data$date), hist_data$Higher20, col = "red", lty = 2)
  lines(as.Date(hist_data$date), hist_data$Lower20, col = "red", lty = 2)
  
  # Área entre bandas
  polygon(
    c(as.Date(hist_data$date), rev(as.Date(hist_data$date))),
    c(hist_data$Higher20, rev(hist_data$Lower20)),
    col = alpha("lightblue", 0.2),
    border = NA
  )
  
  legend(
    "topleft",
    c("Precio", "SMA20", "Bandas ±1σ"),
    col = c("black", "blue", "red"),
    lwd = c(2, 1.5, 1),
    lty = c(1, 1, 2)
  )
  
  dev.off()
  
  # Métricas del modelo
  trend_signal <- ifelse(
    current_price > current_sma20 &
      current_sma20 > current_sma50 &
      current_sma50 > current_sma200,
    "ALCISTA",
    ifelse(
      current_price < current_sma20 &
        current_sma20 < current_sma50 &
        current_sma50 < current_sma200,
      "BAJISTA",
      "NEUTRAL"
    )
  )
  
  cat(glue("\nSeñal de tendencia: {trend_signal}\n"))
  
  # Verificar que tengamos suficientes predicciones
  target_day <- min(30, days_simulated)
  cat(glue("Precio objetivo ({target_day} días): ${round(sma_predictions[target_day], 2)}\n"))
  cat(glue("Rango esperado: ${round(lower_band[target_day], 2)} - ${round(upper_band[target_day], 2)}\n"))
  cat(glue("Volatilidad estimada: {round(volatility * sqrt(252) * 100, 2)}% anual\n"))
  
  # Guardar resultados
  output <- tibble(
    date = pred_dates,
    sma_prediction = sma_predictions,
    upper_95 = upper_band,
    lower_95 = lower_band,
    current_sma20 = current_sma20,
    current_sma50 = current_sma50,
    current_sma200 = current_sma200,
    trend_signal = trend_signal
  )
  
  saveRDS(output, glue("models/SMA/model_SMA_{s}_{Sys.Date()}.rds"))
  
  cat(glue("\n✅ Modelo SMA completado para {s}\n"))
  cat(rep("=", 50), "\n")
}

cat(glue("\n🎯 Análisis SMA completado!\n"))
cat(glue("📁 Gráficos guardados en: output/plots/\n"))
cat(glue("💾 Modelos guardados en: models/SMA/\n"))