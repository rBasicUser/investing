# FORECASTING SIMPLIFICADO - CORREGIDO (base R + residuos lado a lado)

library(tidyverse)
library(yaml)
library(forecast)
library(tseries)
library(glue)

# Cargar funciones y parámetros
params <- read_yaml("params.yaml")
stocks <- params$stocks

# Cargar datos macro (M2) con validación
M2 <- read_csv("data/external/WM2NS.csv")

# Explorar la correlación entre el Crecimiento Interanual del M"

for (s in stocks) {
  # Cargar datos históricos de cada acción
  train_data <- readRDS(glue("data/processed/returns/{s}_full.rds"))
  
  # Limpiar y preparar datos
  train_data_clean <- train_data |>
    left_join(M2, by = "date") |>
    arrange(date) |>
    rename(M2 = value) |>
    fill(M2, .direction = "downup") |>
    mutate(
      month = month(date),
      lag_volume = lag(volume, 1),
      lag_close = lag(close, 1),
      log_M2 = log(M2)
    ) |>
    drop_na() |>
    select(date, close, lag_close, lag_volume, month, log_M2)

  # Ajustar modelo de regresión
  model_ts <- lm(
    close ~ lag_close + lag_volume + factor(month) + log_M2,
    data = train_data_clean
  )

  # Valores finales de entrenamiento
  last_row   <- tail(train_data_clean, 1)
  last_close <- last_row$close
  last_volume <- last_row$lag_volume
  last_date   <- last_row$date
  last_M2     <- last_row$log_M2

  # Horizonte de pronóstico
  h <- params$days_simulated

  forecasts <- numeric(h)
  current_close  <- last_close
  current_volume <- last_volume
  current_M2     <- last_M2

  for (i in seq_len(h)) {
    future_date  <- last_date + i
    current_month <- month(future_date)

    newdata <- tibble(
      lag_close  = current_close,
      lag_volume = current_volume,
      month       = current_month,
      log_M2      = current_M2
    )

    pred_i <- predict(model_ts, newdata = newdata)
    forecasts[i] <- pred_i

    # Actualizar variables para siguiente paso
    current_close  <- pred_i
    current_volume <- mean(tail(train_data_clean$lag_volume, 10), na.rm = TRUE)
  }

  # Error estándar residual para IC
  resid_se <- summary(model_ts)$sigma

  # Construir data.frame de pronóstico con intervalos
  forecast_dates <- seq(from = last_date + 1, by = "day", length.out = h)
  forecasts_df <- tibble(
    date          = forecast_dates,
    pred          = forecasts,
    lo_80         = forecasts - qnorm(0.90)  * resid_se,
    hi_80         = forecasts + qnorm(0.90)  * resid_se,
    lo_95         = forecasts - qnorm(0.975) * resid_se,
    hi_95         = forecasts + qnorm(0.975) * resid_se
  )

  # Preparar datos históricos para gráficos (últimos 50 días)
  historical_df <- train_data_clean |>
    select(date, close) |>
    tail(50) |>
    rename(value = close)

  # Residuales históricos últimos 50
  res_hist    <- resid(model_ts)
  dates_res   <- train_data_clean$date[!is.na(res_hist)]
  res_last50  <- tail(res_hist, 50)
  dates_res50 <- tail(dates_res, 50)

  # Crear gráfico combinado en PNG
  png(glue("output/plots/{s}/Regression_{s}.png"), width = 800, height = 800)
  par(mfrow = c(2, 1), mar = c(4, 4, 2, 1))

  # Gráfico 1: histórico + pronóstico + IC95%
  plot(
    x    = c(historical_df$date, forecasts_df$date),
    y    = c(historical_df$value, forecasts_df$pred),
    type = "l",
    col  = c(rep("blue", nrow(historical_df)), rep("red", h)),
    lty  = c(rep(1,    nrow(historical_df)), rep(2,   h)),
    xlab = "Fecha",
    ylab = "Precio de Cierre ($)",
    main = glue("Pronóstico {s}: {h} días")
  )
  polygon(
    x = c(forecasts_df$date, rev(forecasts_df$date)),
    y = c(forecasts_df$lo_95,  rev(forecasts_df$hi_95)),
    border = NA,
    col    = rgb(1, 0, 0, alpha = 0.2)
  )
  abline(v = tail(historical_df$date, 1), lty = 3, col = "gray50")
  legend(
    "topleft",
    legend = c("Histórico", "Pronóstico", "IC 95%"),
    col    = c("blue",     "red",         rgb(1, 0, 0, 0.2)),
    lty    = c(1,          2,              NA),
    pch    = c(NA,         NA,             15),
    pt.cex = 1,
    bty    = "n"
  )

  # Gráfico 2: residuos últimos 50 días
  plot(
    x    = dates_res50,
    y    = res_last50,
    type = "b",
    pch  = 16,
    xlab = "Fecha",
    ylab = "Residuo",
    main = "Residuos últimos 50 días"
  )
  abline(h = 0, lty = 2, col = "gray50")
  dev.off()

  # Métricas de desempeño
  residuals     <- resid(model_ts)
  actuals       <- train_data_clean$close[!is.na(residuals)]
  mse           <- mean(residuals^2)
  rmse          <- sqrt(mse)
  mae           <- mean(abs(residuals))
  mape          <- mean(abs(residuals / actuals)) * 100
  r2            <- summary(model_ts)$r.squared
  adj_r2        <- summary(model_ts)$adj.r.squared
  aic_val       <- AIC(model_ts)
  bic_val       <- BIC(model_ts)

  metrics <- tibble(
    Métrica       = c("RMSE", "MAE", "MAPE (%)", "R²", "R² Ajustado", "AIC", "BIC"),
    Valor         = round(c(rmse, mae, mape, r2, adj_r2, aic_val, bic_val), 4)
  )
  print(metrics)

  # Guardar pronóstico y métricas
  saveRDS(forecasts_df, glue("models/Regression/model_regression_{s}.rds"))
  write_csv(metrics, glue("output/tables/metrics_{s}.csv"))

  cat("✓ Pronóstico completado para stock:", s, "\n")
}
