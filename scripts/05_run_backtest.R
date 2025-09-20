library(tidyverse)
library(lubridate)
library(yaml)
library(glue)
library(timeDate)

# Cargar funciones
source("R/engine_backtest.R") # Tu motor actual + el nuevo código
source("R/last_date.R")
# 1. CONFIGURACIÓN INICIAL

params <- read_yaml("params.yaml")

CONFIG <- params$CONFIG
symbols <- params$stocks

# ================================
# 2. CARGA Y PREPARACIÓN DE DATOS
# ================================

all_data <- list()

for (s in symbols) {
  stock_data <- readRDS(glue("data/processed/returns/{s}_returns.rds"))
  all_data[[s]] <- stock_data
}

# Combinar todos los datos
combined_data <- all_data |>
  reduce(full_join, by = "date")

load_predictions <- function(symbols, model_type = "ARIMA") {
  predictions_list <- list()

  nyse_holidays <- holidayNYSE(2000:2030)

  for (s in symbols) {
    
    date <- get_last_business_day()

    # Cargar predicciones del modelo especificado
    model_path <- glue(
      "models/",
      model_type,
      "/forecasting/forecasting_{model_type}_{s}_{date}.rds"
    )
  }

  return(bind_rows(predictions_list))
}

generate_trading_signals()

# ================================
# 3. EJECUCIÓN DE BACKTESTS
# ================================

run_all_backtests <- function() {
  cat("Iniciando sistema de backtesting...\n")

  # 1. Cargar datos
  cat("Cargando datos históricos...\n")
  price_data <- load_stock_data(SYMBOLS)

  # 2. Resultados para cada estrategia
  results <- list()

  # ESTRATEGIA 1: ARIMA
  cat("Ejecutando backtest ARIMA...\n")
  arima_predictions <- load_predictions(SYMBOLS, "ARIMA")
  arima_signals <- generate_trading_signals(
    arima_predictions,
    price_data,
    CONFIG$signal_threshold
  )

  results$arima <- run_backtest(
    arima_signals,
    CONFIG$initial_capital,
    CONFIG$position_size,
    CONFIG$transaction_cost,
    CONFIG$start_date,
    CONFIG$end_date
  )

  # ESTRATEGIA 2: SMA
  cat("Ejecutando backtest SMA...\n")
  sma_signals <- generate_sma_signals(price_data, CONFIG$signal_threshold)

  results$sma <- run_backtest(
    sma_signals,
    CONFIG$initial_capital,
    CONFIG$position_size,
    CONFIG$transaction_cost,
    CONFIG$start_date,
    CONFIG$end_date
  )

  # ESTRATEGIA 3: REGRESIÓN
  cat("Ejecutando backtest Regresión...\n")
  regression_predictions <- load_predictions(SYMBOLS, "Regression")
  regression_signals <- generate_trading_signals(
    regression_predictions,
    price_data,
    CONFIG$signal_threshold
  )

  results$regression <- run_backtest(
    regression_signals,
    CONFIG$initial_capital,
    CONFIG$position_size,
    CONFIG$transaction_cost,
    CONFIG$start_date,
    CONFIG$end_date
  )

  # BUY & HOLD (Benchmark)
  cat("Ejecutando backtest Buy & Hold...\n")
  results$buy_hold <- run_buy_hold_backtest(
    SYMBOLS,
    price_data,
    CONFIG$initial_capital,
    CONFIG$start_date,
    CONFIG$end_date
  )

  return(results)
}

# Función específica para señales SMA
generate_sma_signals <- function(price_data, threshold = 0.02) {
  sma_signals <- price_data %>%
    group_by(s) %>%
    arrange(date) %>%
    mutate(
      sma_20 = zoo::rollmean(close, k = 20, fill = NA, align = "right"),
      sma_50 = zoo::rollmean(close, k = 50, fill = NA, align = "right"),
      signal_raw = case_when(
        sma_20 > sma_50 * (1 + threshold) ~ "BUY",
        sma_20 < sma_50 * (1 - threshold) ~ "SELL",
        TRUE ~ "HOLD"
      )
    ) %>%
    filter(signal_raw != "HOLD", !is.na(sma_20), !is.na(sma_50)) %>%
    select(date, s, current_price = close, signal = signal_raw) %>%
    ungroup()

  return(sma_signals)
}

# ================================
# 4. ANÁLISIS Y REPORTES
# ================================

create_performance_dashboard <- function(results) {
  # Tabla resumen de todas las estrategias
  summary_metrics <- data.frame(
    Strategy = c("ARIMA", "SMA", "Regression", "Buy & Hold"),
    Total_Return = c(
      results$arima$metrics$total_return,
      results$sma$metrics$total_return,
      results$regression$metrics$total_return,
      results$buy_hold$metrics$total_return
    ),
    Annualized_Return = c(
      results$arima$metrics$annualized_return,
      results$sma$metrics$annualized_return,
      results$regression$metrics$annualized_return,
      results$buy_hold$metrics$annualized_return
    ),
    Sharpe_Ratio = c(
      results$arima$metrics$sharpe_ratio,
      results$sma$metrics$sharpe_ratio,
      results$regression$metrics$sharpe_ratio,
      results$buy_hold$metrics$sharpe_ratio
    ),
    Max_Drawdown = c(
      results$arima$metrics$max_drawdown,
      results$sma$metrics$max_drawdown,
      results$regression$metrics$max_drawdown,
      results$buy_hold$metrics$max_drawdown
    ),
    Final_Value = c(
      results$arima$metrics$final_value,
      results$sma$metrics$final_value,
      results$regression$metrics$final_value,
      results$buy_hold$metrics$final_value
    )
  ) %>%
    mutate(
      Total_Return = glue(round(Total_Return * 100, 2), "%"),
      Annualized_Return = glue(round(Annualized_Return * 100, 2), "%"),
      Sharpe_Ratio = round(Sharpe_Ratio, 3),
      Max_Drawdown = glue(round(Max_Drawdown * 100, 2), "%"),
      Final_Value = glue("$", format(round(Final_Value, 0), big.mark = ","))
    )

  return(summary_metrics)
}

# ================================
# 5. EJECUCIÓN PRINCIPAL
# ================================

main <- function() {
  cat("=================================\n")
  cat("SISTEMA DE TRADING AUTOMATIZADO\n")
  cat("=================================\n\n")

  # Ejecutar todos los backtests
  results <- run_all_backtests()

  # Crear dashboard de performance
  dashboard <- create_performance_dashboard(results)

  cat("\nRESUMEN DE RESULTADOS:\n")
  cat("======================\n")
  print(dashboard)

  # Guardar resultados
  saveRDS(results, "output/backtest_results.rds")
  write.csv(dashboard, "output/performance_summary.csv", row.names = FALSE)

  # Generar reportes individuales
  for (strategy in names(results)[names(results) != "buy_hold"]) {
    report <- generate_backtest_report(
      results[[strategy]],
      results$buy_hold,
      toupper(strategy)
    )
    write.csv(
      report,
      glue("output/report_", strategy, ".csv"),
      row.names = FALSE
    )
  }

  cat("\nResultados guardados en carpeta 'output/'\n")
  cat("Sistema completado exitosamente!\n")

  return(results)
}

# Ejecutar sistema completo
results <- main()
