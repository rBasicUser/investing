# ===================================================================
# SIMULACIÓN MONTE CARLO PARA PRONÓSTICO DE ACCIONES
# Modelo para posición de Quant Analyst
# ===================================================================

library(tidyverse)
library(glue)
library(yaml)
library(scales) # Para alpha en gráficos

# Cargar parámetros
params <- read_yaml("params.yaml")
stocks <- params$stocks
days_simulated <- params$days_simulated

# Función principal

for (s in stocks) {
  cat(glue("\n=== Procesando {s} ===\n"))

  # Cargar datos
  train_data <- read_csv(glue("data/processed/{s}_full.csv")) |>
    mutate(
      log_returns = log(close) - lag(log(close)),
      returns = (close - lag(close)) / lag(close)
    )

  price_inicial <- last(train_data$close)
  mu <- mean(train_data$log_returns, na.rm = TRUE)
  sigma <- sd(train_data$log_returns, na.rm = TRUE)

  cat(glue("Precio inicial: ${round(price_inicial, 2)}\n"))
  cat(glue("Drift promedio: {round(mu * 100, 4)}% diario\n"))
  cat(glue("Volatilidad: {round(sigma * 100, 2)}% diaria\n"))

  escenarios <- 1000

  price_paths <- matrix(NA_real_, nrow = days_simulated, ncol = escenarios)
  price_paths[1, ] <- price_inicial

  # Simulaciones GBM
  for (i in seq_len(escenarios)) {
    for (t in 2:days_simulated) {
      dW <- rnorm(1)
      price_paths[t, i] <- price_paths[t - 1, i] *
        exp((mu - sigma^2 / 2) + sigma * dW)
    }
  }
  png(glue("output/plots/{s}/MonteCarlo_{s}.png"),width = 800,height = 800)
  par(mfrow = c(2, 2), mar = c(4, 4, 3, 2))

  # 1. Histograma precios históricos
  hist(
    train_data$close,
    main = glue("Distribución Histórica - {s}"),
    xlab = "Precio ($)",
    ylab = "Frecuencia",
    col = "lightblue",
    border = "white",
    breaks = 30
  )
  abline(v = mean(train_data$close, na.rm = TRUE), col = "red", lty = 2)
  abline(v = median(train_data$close, na.rm = TRUE), col = "blue", lty = 2)
  legend("topright", c("Media", "Mediana"), col = c("red", "blue"), lty = 2)

  # 2. Caminos Monte Carlo (100 muestra)
  sample_idx <- sample(escenarios, 100)
  plot(
    1:days_simulated,
    price_paths[, sample_idx[1]],
    type = "n",
    main = glue("MC Simulaciones - {s}"),
    xlab = "Días",
    ylab = "Precio ($)",
    ylim = range(price_paths, na.rm = TRUE)
  )
  for (j in sample_idx) {
    lines(price_paths[, j], col = alpha("gray", 0.3))
  }
  pct <- apply(price_paths, 1, quantile, probs = c(0.05, 0.25, 0.75, 0.95))
  mu_sim <- rowMeans(price_paths)
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
    ylim = range(c(combined_vals, price_paths), na.rm = TRUE)
  )
  abline(v = start_pred, col = "red", lty = 2)
  polygon(
    c(fechas_pred, rev(fechas_pred)),
    c(pct[1, ], rev(pct[4, ])),
    col = alpha("lightblue", 0.3),
    border = NA
  )
  lines(fechas_pred, mu_sim, lwd = 2)

  # 4. Distribución precios finales
  finales <- price_paths[days_simulated, ]
  hist(
    finales,
    main = glue("Distribución Final - {s}"),
    xlab = "Precio Final ($)",
    col = "lightgreen",
    border = "white",
    breaks = 50
  )
  abline(v = mean(finales), col = "red", lty = 2)
  abline(v = quantile(finales, c(0.05, 0.95)), col = "orange", lty = 3)
  legend("topright", c("Media", "IC 90%"), col = c("red", "orange"), lty = 2)
  par(mfrow = c(1, 1))

  dev.off()

  # Guardar resultados
  last_date <- max(as.Date(train_data$date))
  fechas_predicciones <- seq(
    last_date + 1,
    by = "day",
    length.out = days_simulated
  )
  output <- tibble(
    date = fechas_predicciones,
    pred = rowMeans(price_paths),
    pred_lo95 = pct[1, ],
    pred_hi95 = pct[4, ],
    volatility = apply(price_paths, 1, sd, na.rm = TRUE)
  )
  attr(output, "metrics") <- list(
    stock = s,
    scenarios = escenarios
  )
  saveRDS(output, glue("models/MonteCarlo/model_monetcarlo_{s}.rds"))

  cat(glue("\n✅ Modelo completado para {s}\n"))
  cat(rep("=", 50), "\n")
}
