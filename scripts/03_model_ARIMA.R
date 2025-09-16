# Carga de librerías
library(tidyverse)
library(yaml)
library(glue)
library(forecast)

# Carga de parámetros
params <- read_yaml("params.yaml")
days_simulated <- params$days_simulated
stocks <- params$stocks

# Carga de datos
for (s in stocks) {
  train_data <- read_csv(glue("data/processed/{s}_full.csv")) |>
    arrange(date)

  # Extraer la columna de precios y ordenarlos del más antiguo al más reciente
  precios <- train_data$close

  # Calcular retornos logarítmicos
  retornos <- diff(log(precios))
  media_ret <- mean(retornos, na.rm = TRUE)
  sd_ret <- sd(retornos, na.rm = TRUE)

  # Ajuste automático de ARIMA con auto.arima()
  fit <- auto.arima(
    retornos,
    seasonal = FALSE, # sin componente estacional
    stepwise = TRUE, # búsqueda rápida
    approximation = TRUE # aproximación para velocidad
  )
  summary(fit)

  # Pronóstico con ARIMA (usando days_simulated del parámetro)
  fc <- forecast(fit, h = 1)

  n_sim <- 1000 # Número de simulaciones

  # Generar simulaciones
  sim_matrix <- matrix(NA, nrow = days_simulated, ncol = n_sim)

  for (i in 1:n_sim) {
    # Simular retornos futuros
    sim_returns <- simulate(fit, nsim = days_simulated)
    sim_matrix[, i] <- sim_returns
  }

  # Convertir retornos simulados a precios
  precio_inicial <- tail(precios, 1) # Último precio observado
  sim_precios <- matrix(NA, nrow = days_simulated, ncol = n_sim)

  for (i in 1:n_sim) {
    # Convertir retornos logarítmicos a precios
    precios_sim <- numeric(days_simulated)
    precio_actual <- precio_inicial

    for (j in 1:days_simulated) {
      precio_actual <- precio_actual * exp(sim_matrix[j, i])
      precios_sim[j] <- precio_actual
    }

    sim_precios[, i] <- precios_sim
  }

  # Crear tabla de resultados con precios (no retornos)
  fecha_inicio_predicciones <- max(train_data$date) + 1 # Día siguiente al último dato
  fechas <- seq(
    fecha_inicio_predicciones,
    by = "day",
    length.out = days_simulated
  )

  output_model2 <- data.frame(
    date = fechas,
    pred = rowMeans(sim_precios), # Promedio de precios simulados
    pred_lower = apply(sim_precios, 1, quantile, 0.025), # Intervalo inferior
    pred_upper = apply(sim_precios, 1, quantile, 0.975) # Intervalo superior
  )
  
  png(glue("output/plots/{s}/ARIMA_{s}.png"),width = 800,height = 800)
  par(mfrow = c(2, 2), mar = c(4, 4, 3, 2))

  # 1. Histograma de retornos logarítmicos históricos
  hist(
    retornos,
    main = glue("Distribución Retornos Históricos - {s}"),
    xlab = "Retornos Logarítmicos",
    ylab = "Frecuencia",
    col = "lightblue",
    border = "white",
    breaks = 30
  )
  abline(v = mean(retornos, na.rm = TRUE), col = "red", lty = 2)
  abline(v = median(retornos, na.rm = TRUE), col = "blue", lty = 2)
  legend("topright", c("Media", "Mediana"), col = c("red", "blue"), lty = 2)

  # 2. Caminos Monte Carlo (100 muestra)
  sample_idx <- sample(n_sim, 200)
  plot(
    1:days_simulated,
    sim_precios[, sample_idx[1]],
    type = "n",
    main = glue("MC Simulaciones - {s}"),
    xlab = "Días",
    ylab = "Precio ($)",
    ylim = range(sim_precios, na.rm = TRUE)
  )
  for (j in sample_idx) {
    lines(sim_precios[, j], col = alpha("gray", 0.3))
  }
  pct <- apply(sim_precios, 1, quantile, probs = c(0.05, 0.25, 0.75, 0.95))
  mu_sim <- rowMeans(sim_precios)
  lines(pct[1, ], lty = 3)
  lines(pct[2, ], lty = 2)
  lines(mu_sim, lwd = 2)
  lines(pct[3, ], lty = 2)
  lines(pct[4, ], lty = 3)
  legend(
    "topleft",
    c("Caminos", "Media", "Q25-Q75", "Q5-Q95"),
    lty = c(1, 1, 2, 3),
    lwd = c(1, 2, 1, 1)
  )

  # 3. Histórico vs pronóstico
  fechas_hist <- as.Date(train_data$date)
  start_pred <- max(fechas_hist) + 1
  fechas_pred <- seq(start_pred, by = "day", length.out = days_simulated)
  combined_dates <- c(tail(fechas_hist, 30), fechas_pred)
  combined_vals <- c(tail(train_data$close, 30), rep(NA, days_simulated))
  plot(
    combined_dates,
    combined_vals,
    type = "l",
    main = glue("Histórico vs Pronóstico - {s}"),
    xlab = "Fecha",
    ylab = "Precio ($)",
    ylim = range(c(combined_vals, sim_precios), na.rm = TRUE)
  )
  abline(v = start_pred, col = "red", lty = 2)
  polygon(
    c(fechas_pred, rev(fechas_pred)),
    c(pct[1, ], rev(pct[4, ])),
    col = alpha("lightblue", 0.3),
    border = NA
  )
  lines(fechas_pred, mu_sim, lwd = 2)

  # 4. Función ACF de los retornos logarítmicos
  acf(
    retornos,
    main = glue("Función ACF - Retornos {s}"),
    xlab = "Lag",
    ylab = "Autocorrelación",
    col = "darkblue",
    lwd = 2
  )

  par(mfrow = c(2, 2))
  dev.off()

  # Guardar resultados
  saveRDS(output_model2, glue("models/ARIMA/model_ARIMA_{s}.rds"))

  # Mostrar resumen de los resultados
  cat("Resumen del modelo ARIMA:\n")
  print(summary(fit))
  cat("\nPrimeras predicciones:\n")
  print(head(output_model2))
}
