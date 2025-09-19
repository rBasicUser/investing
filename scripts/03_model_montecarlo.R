# ===================================================================
# SIMULACIÓN MONTE CARLO PARA PRONÓSTICO DE ACCIONES - MEJORADO
# Modelo para posición de Quant Analyst
# ===================================================================

library(tidyverse)
library(glue)
library(yaml)
library(scales)
library(moments)  # Para tests de normalidad y estadísticas
library(tseries)  # Para tests de autocorrelación

# Cargar parámetros
params <- read_yaml("params.yaml")
stocks <- params$stocks
days_simulated <- params$days_simulated

# Función principal
for (s in stocks) {
  cat(glue("=== PROCESANDO {s} ===\n"))
  # Cargar datos
  train_data <- readRDS(glue("data/processed/returns/{s}_returns.rds"))|>
    tail(252*4)  
  
  # 1) Construyes el nombre de la columna:
  col_precio <- paste0(s, ".Adjusted")

# 2) Extraes ese vector y le aplicas last():
  price_inicial <- last(train_data[[col_precio]])
  
  mu <- mean(train_data$daily.returns, na.rm = TRUE)
  sigma <- sd(train_data$daily.returns, na.rm = TRUE)
  n_obs <- nrow(train_data)

  # DIAGNÓSTICO DE SUPUESTOS DEL MODELO
  cat(glue("\n--- DIAGNÓSTICO DE SUPUESTOS ---\n"))
  cat(glue("Precio inicial: {round(price_inicial, 2)}\n"))
  cat(glue("Drift anualizado: {round(mu * 252 * 100, 2)}% anual\n"))
  cat(glue("Volatilidad anualizada: {round(sigma * sqrt(252) * 100, 2)}% anual\n"))

  # Test de normalidad (usando muestra si hay muchos datos)
  sample_size <- min(5000, length(train_data$daily.returns))
  sample_returns <- sample(train_data$daily.returns, sample_size)
  
  shapiro_result <- shapiro.test(sample_returns)
  jb_result <- jarque.test(train_data$daily.returns)
  
  cat(glue("\nTest de Normalidad:\n"))
  cat(glue("  Shapiro-Wilk p-valor: {round(shapiro_result$p.value, 4)}\n"))
  cat(glue("  Jarque-Bera p-valor: {round(jb_result$p.value, 4)}\n"))
  cat(glue("  Skewness: {round(skewness(train_data$daily.returns), 4)}\n"))
  cat(glue("  Kurtosis: {round(kurtosis(train_data$daily.returns), 4)}\n"))
  cat(glue("  Los retornos son normales: {ifelse(shapiro_result$p.value > 0.05 & jb_result$p.value > 0.05, 'SÍ', 'NO')}\n"))

  # Test de autocorrelación
  lb_test <- Box.test(train_data$daily.returns, lag = 10, type = "Ljung-Box")
  cat(glue("\nTest de Independencia:\n"))
  cat(glue("  Ljung-Box p-valor: {round(lb_test$p.value, 4)}\n"))
  cat(glue("  Los retornos son independientes: {ifelse(lb_test$p.value > 0.05, 'SÍ', 'NO')}\n"))

  # SIMULACIONES MONTE CARLO MEJORADAS
  cat(glue("\n--- SIMULACIONES MONTE CARLO ---\n"))
  escenarios <- 10000  # Incrementado para mayor precisión
  dt <- 1/252  # Fracción de año (días de trading)
  
  # Ajuste temporal para el drift
  mu_adjusted <- mu * dt
  sigma_adjusted <- sigma * sqrt(dt)

  price_paths <- matrix(NA_real_, nrow = days_simulated, ncol = escenarios)
  price_paths[1, ] <- price_inicial

  cat(glue("Generando {escenarios} escenarios para {days_simulated} días...\n"))

  # Simulaciones GBM mejoradas
  
  for (i in seq_len(escenarios)) {
    for (t in 2:days_simulated) {
      dW <- rnorm(1)
      price_paths[t, i] <- price_paths[t - 1, i] * 
        exp(mu_adjusted - sigma^2 * dt / 2 + sigma_adjusted * dW)
    }
  }

  # ANÁLISIS DE RESULTADOS
  cat(glue("\n--- ANÁLISIS DE RESULTADOS ---\n"))
  
  # Estadísticas de precios finales
  finales <- price_paths[days_simulated, ]
  
  # Métricas de riesgo
  var_95 <- quantile(finales, 0.05)
  var_99 <- quantile(finales, 0.01)
  es_95 <- mean(finales[finales <= var_95])
  es_99 <- mean(finales[finales <= var_99])
  
  # Probabilidad de pérdida
  prob_loss <- mean(finales < price_inicial)
  
  # Máximo drawdown promedio
  max_drawdowns <- apply(price_paths, 2, calculate_max_drawdown)
  avg_max_drawdown <- mean(max_drawdowns)
  
  cat(glue("Precio esperado final: ${round(mean(finales), 2)}\n"))
  cat(glue("VaR 95%: ${round(var_95, 2)}\n"))
  cat(glue("VaR 99%: ${round(var_99, 2)}\n"))
  cat(glue("Expected Shortfall 95%: ${round(es_95, 2)}\n"))
  cat(glue("Probabilidad de pérdida: {round(prob_loss * 100, 1)}%\n"))
  cat(glue("Máximo drawdown promedio: {round(avg_max_drawdown * 100, 1)}%\n"))

  # VISUALIZACIONES MEJORADAS
  png(glue("output/plots/{s}/montecarlo_{s}.png"), width = 1200, height = 1000)
  par(mfrow = c(2, 2), mar = c(4, 4, 3, 2))

  # 1. Histograma retornos logarítmicos históricos
  hist(
    train_data$daily.returns,
    main = glue("Distribución Log-Returns - {s}"),
    xlab = "Log Returns",
    ylab = "Frecuencia",
    col = "lightblue",
    border = "white",
    breaks = 50,
    freq = FALSE
  )
  # Superimponer distribución normal teórica
  x_norm <- seq(min(train_data$daily.returns), max(train_data$daily.returns), length.out = 100)
  y_norm <- dnorm(x_norm, mean = mu, sd = sigma)
  lines(x_norm, y_norm, col = "red", lwd = 2)
  legend("topright", c("Datos", "Normal Teórica"), 
         col = c("lightblue", "red"), lwd = c(10, 2))

  # 3. Caminos Monte Carlo (muestra)
  sample_idx <- sample(escenarios, 200)
  plot(
    1:days_simulated,
    price_paths[, sample_idx[1]],
    type = "n",
    main = glue("Caminos MC - {s}"),
    xlab = "Días",
    ylab = "Precio ($)",
    ylim = range(price_paths, na.rm = TRUE)
  )
  for (j in sample_idx) {
    lines(price_paths[, j], col = alpha("gray", 0.1))
  }
  
  # Percentiles y media
  pct <- apply(price_paths, 1, quantile, probs = c(0.01, 0.05, 0.25, 0.75, 0.95, 0.99))
  mu_sim <- rowMeans(price_paths)
  
  lines(pct[1, ], col = "red", lty = 3, lwd = 2)      # 1%
  lines(pct[2, ], col = "orange", lty = 2, lwd = 2)   # 5%
  lines(pct[3, ], col = "blue", lty = 2)              # 25%
  lines(mu_sim, col = "black", lwd = 3)               # Media
  lines(pct[4, ], col = "blue", lty = 2)              # 75%
  lines(pct[5, ], col = "orange", lty = 2, lwd = 2)   # 95%
  lines(pct[6, ], col = "red", lty = 3, lwd = 2)      # 99%
  
  legend("topleft", c("Media", "P5-P95", "P1-P99"), 
         col = c("black", "orange", "red"), 
         lty = c(1, 2, 3), lwd = c(3, 2, 2))

  # 4. Histórico vs pronóstico
  fechas_hist <- as.Date(train_data$date)
  start_pred <- max(fechas_hist) + 1
  fechas_pred <- seq(start_pred, by = "day", length.out = days_simulated)
  combined_dates <- c(tail(fechas_hist, 60), fechas_pred)
  combined_vals <- c(tail(train_data[[col_precio]], 60), rep(NA, days_simulated))
  
  plot(
    combined_dates,
    combined_vals,
    type = "l",
    main = glue("Histórico vs Pronóstico - {s}"),
    xlab = "Fecha",
    ylab = "Precio ($)",
    ylim = range(c(combined_vals, price_paths), na.rm = TRUE),
    lwd = 2
  )
  abline(v = start_pred, col = "red", lty = 2, lwd = 2)
  
  # Bandas de confianza
  polygon(
    c(fechas_pred, rev(fechas_pred)),
    c(pct[2, ], rev(pct[5, ])),  # 90% CI
    col = alpha("lightblue", 0.3),
    border = NA
  )
  polygon(
    c(fechas_pred, rev(fechas_pred)),
    c(pct[3, ], rev(pct[4, ])),  # 50% CI
    col = alpha("lightblue", 0.5),
    border = NA
  )
  
  lines(fechas_pred, mu_sim, col = "darkblue", lwd = 3)

  # 5. Distribución precios finales con métricas de riesgo
  hist(
    finales,
    main = glue("Distribución Final - {s}"),
    xlab = "Precio Final ($)",
    col = "lightgreen",
    border = "white",
    breaks = 100,
    freq = FALSE
  )
  abline(v = mean(finales), col = "black", lwd = 3)
  abline(v = price_inicial, col = "blue", lwd = 2, lty = 2)
  abline(v = var_95, col = "orange", lwd = 2)
  abline(v = var_99, col = "red", lwd = 2)
  
  legend("topright", 
         c("Media", "Precio Inicial", "VaR 95%", "VaR 99%"), 
         col = c("black", "blue", "orange", "red"), 
         lwd = c(3, 2, 2, 2),
         lty = c(1, 2, 1, 1))

  dev.off()

  # GUARDAR RESULTADOS MEJORADOS
  last_date <- max(as.Date(train_data$date))
  fechas_predicciones <- seq(last_date + 1, by = "day", length.out = days_simulated)
  
  output <- tibble(
    date = fechas_predicciones,
    pred = rowMeans(price_paths),
    pred_lo99 = pct[1, ],
    pred_lo95 = pct[2, ],
    pred_lo50 = pct[3, ],
    pred_hi50 = pct[4, ],
    pred_hi95 = pct[5, ],
    pred_hi99 = pct[6, ],
    volatility = apply(price_paths, 1, sd, na.rm = TRUE),
    prob_above_initial = apply(price_paths, 1, function(x) mean(x > price_inicial))
  )
  
  # Metadatos enriquecidos
  metrics <- list(
    stock = s,
    scenarios = escenarios,
    initial_price = price_inicial,
    annual_drift = mu * 252,
    annual_volatility = sigma * sqrt(252),
    var_95 = var_95,
    var_99 = var_99,
    expected_shortfall_95 = es_95,
    prob_loss = prob_loss,
    avg_max_drawdown = avg_max_drawdown,
    normality_p_value = shapiro_result$p.value,
    independence_p_value = lb_test$p.value
  )
  
  saveRDS(output, glue("models/montecarlo/forecasting/model_montecarlo_{s}_{Sys.Date()}.rds"))

  saveRDS(metrics, glue("models/montecarlo/artifacts/model_montecarlo_{s}_{Sys.Date()}.rds"))


  cat(glue("\n✅ Modelo completado para {s}\n"))
  cat(glue("📊 Archivo guardado: model_montecarlo_{s}_{Sys.Date()}.rds\n"))
  cat(rep("=", 60), "\n")
}
