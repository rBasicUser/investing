# Carga de librerías
library(tidyverse)
library(janitor)
library(yaml)
library(glue)
library(forecast)

# Carga de parámetros
params <- read_yaml("params.yaml")
days_simulated <- 5
stocks <- params$stocks

# Carga de datos
for (s in stocks) {
  
  # Definimos el prefijo por cada stock "s"
  pref <- glue("{s}_daily_return")  # Usar retornos logarítmicos para ARIMA

  # Cargar y limpiar datos
  train_data <- readRDS(glue("data/processed/returns/{s}_returns.RDS")) %>%
    clean_names()

  # Seleccionar la columna de retornos logarítmicos
  train_data_sub <- train_data %>% 
    select(starts_with(pref)) %>%
    pull()  # Convertir a vector
  
  # Remover NAs
  train_data_sub <- train_data_sub[!is.na(train_data_sub)]
  
  # Ajuste automático de ARIMA con auto.arima()
  fit <- auto.arima(
    train_data_sub,
    seasonal = FALSE,  # Cambiado a FALSE - retornos diarios no suelen tener estacionalidad
    stepwise = TRUE
  )
  
  cat("Resumen del modelo ARIMA para", s, ":\n")
  print(summary(fit))

  # Pronóstico con ARIMA
  fc <- forecast(fit, h = days_simulated)

  n_sim <- 1000

  # Generar simulaciones
  sim_matrix <- matrix(NA, nrow = days_simulated, ncol = n_sim)

  for (i in 1:n_sim) {
    # Simular retornos futuros
    sim_returns <- simulate(fit, nsim = days_simulated)
    sim_matrix[, i] <- as.numeric(sim_returns)
  }

  # Obtener precio inicial - usar la columna de precios
  precio_col <- glue("{tolower(s)}_adjusted")
  precio_inicial <- train_data %>% 
    select(all_of(precio_col)) %>% 
    tail(1) %>% 
    pull()
  
  # Convertir retornos simulados a precios
  sim_precios <- matrix(NA, nrow = days_simulated, ncol = n_sim)

  for (i in 1:n_sim) {
    precios_sim <- numeric(days_simulated)
    precio_actual <- precio_inicial

    for (j in 1:days_simulated) {
      # Convertir retornos logarítmicos a precios
      precio_actual <- precio_actual * exp(sim_matrix[j, i])
      precios_sim[j] <- precio_actual
    }

    sim_precios[, i] <- precios_sim
  }

  # Crear tabla de resultados con precios
  fecha_inicio_predicciones <- max(train_data$date) + 1
  fechas <- seq(
    fecha_inicio_predicciones,
    by = "day",
    length.out = days_simulated
  )

  output_model2 <- data.frame(
    date = fechas,
    pred = rowMeans(sim_precios),
    pred_lower = apply(sim_precios, 1, quantile, 0.025),
    pred_upper = apply(sim_precios, 1, quantile, 0.975)
  )
  
  # Crear directorio si no existe
  dir.create(glue("output/plots/{s}"), recursive = TRUE, showWarnings = FALSE)
  
  # Generar gráficos
  png(glue("output/plots/{s}/ARIMA_{s}.png"), width = 800, height = 800)
  par(mfrow = c(2, 2), mar = c(4, 4, 3, 2))

  # 1. Histograma de retornos logarítmicos históricos
  retornos <- train_data_sub  # Variable definida correctamente
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

  # 2. Caminos Monte Carlo (200 muestra)
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
    lines(sim_precios[, j], col = rgb(0.5, 0.5, 0.5, 0.3))  # Cambié alpha() por rgb()
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
  precios_hist <- train_data %>% select(all_of(precio_col)) %>% pull()
  start_pred <- max(fechas_hist) + 1
  fechas_pred <- seq(start_pred, by = "day", length.out = days_simulated)
  combined_dates <- c(tail(fechas_hist, 30), fechas_pred)
  combined_vals <- c(tail(precios_hist, 30), rep(NA, days_simulated))
  
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
    col = rgb(0.7, 0.9, 1, 0.3),  # Cambié alpha() por rgb()
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
    lwd = 2,
    na.action = na.pass
  )

  dev.off()

  # Crear directorio para modelos si no existe
  dir.create("models/ARIMA", recursive = TRUE, showWarnings = FALSE)
  
  # Guardar resultados
  saveRDS(output_model2, glue("models/ARIMA/model_ARIMA_{s}.rds"))

  # Mostrar resumen de los resultados
  cat("\nPrimeras predicciones para", s, ":\n")
  print(head(output_model2))
  cat("\n", rep("=", 50), "\n")
}