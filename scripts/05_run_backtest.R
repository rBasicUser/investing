# ================================
# SCRIPT PRINCIPAL - SISTEMA DE TRADING
# ================================

library(tidyverse)
library(lubridate)

# Cargar funciones
source("R/engine_backtest.R")  # Tu motor actual + el nuevo código

# ================================
# 1. CONFIGURACIÓN INICIAL
# ================================

# Parámetros del sistema
CONFIG <- list(
  initial_capital = 100000,
  position_size = 5000,
  transaction_cost = 0.001,  # 0.1% por operación
  signal_threshold = 0.02,   # 2% para generar señal
  start_date = "2020-01-01",
  end_date = "2024-12-31",
  rebalance_frequency = "monthly"  # weekly, monthly, quarterly
)

# Stocks a analizar (basado en tu estructura)
SYMBOLS <- read_yaml("params.yaml")$stocks

# ================================
# 2. CARGA Y PREPARACIÓN DE DATOS
# ================================

load_stock_data <- function(symbols, data_path = "data/processed/returns/") {
  all_data <- list()
  
  for (symbol in symbols) {
    file_path <- paste0(data_path, symbol, "_returns.rds")
    
    if (file.exists(file_path)) {
      stock_data <- readRDS(file_path)
      stock_data$symbol <- symbol
      all_data[[symbol]] <- stock_data
    } else {
      warning(paste("Archivo no encontrado para", symbol))
    }
  }
  
  # Combinar todos los datos
  combined_data <- bind_rows(all_data)
  return(combined_data)
}

load_predictions <- function(symbols, model_type = "ARIMA") {
  predictions_list <- list()
  
  for (symbol in symbols) {
    # Cargar predicciones del modelo especificado
    model_path <- paste0("models/", model_type, "/model_", model_type, "_", symbol, ".rds")
    
    if (file.exists(model_path)) {
      model <- readRDS(model_path)
      
      # Generar predicciones (esto depende de cómo tengas estructurados tus modelos)
      # Ejemplo genérico - tendrás que adaptarlo a tu estructura
      predictions <- generate_model_predictions(model, symbol)
      predictions$symbol <- symbol
      predictions_list[[symbol]] <- predictions
    }
  }
  
  return(bind_rows(predictions_list))
}

# Función para generar señales de trading
generate_trading_signals <- function(predictions, price_data, threshold = 0.02) {
  
  signals <- predictions %>% 
    left_join(price_data, by = c("symbol", "date")) %>% 
    mutate(
      expected_return = (predicted_price - current_price) / current_price,
      signal = case_when(
        expected_return > threshold ~ "BUY",
        expected_return < -threshold ~ "SELL",
        TRUE ~ "HOLD"
      ),
      confidence = abs(expected_return)
    ) %>% 
    filter(signal != "HOLD") %>% 
    arrange(desc(confidence))  # Priorizar señales con mayor confianza
  
  return(signals)
}

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
  arima_signals <- generate_trading_signals(arima_predictions, price_data, CONFIG$signal_threshold)
  
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
  regression_signals <- generate_trading_signals(regression_predictions, price_data, CONFIG$signal_threshold)
  
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
    group_by(symbol) %>% 
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
    select(date, symbol, current_price = close, signal = signal_raw) %>% 
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
    Total_Return = paste0(round(Total_Return * 100, 2), "%"),
    Annualized_Return = paste0(round(Annualized_Return * 100, 2), "%"),
    Sharpe_Ratio = round(Sharpe_Ratio, 3),
    Max_Drawdown = paste0(round(Max_Drawdown * 100, 2), "%"),
    Final_Value = paste0("$", format(round(Final_Value, 0), big.mark = ","))
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
    report <- generate_backtest_report(results[[strategy]], results$buy_hold, toupper(strategy))
    write.csv(report, paste0("output/report_", strategy, ".csv"), row.names = FALSE)
  }
  
  cat("\nResultados guardados en carpeta 'output/'\n")
  cat("Sistema completado exitosamente!\n")
  
  return(results)
}

# Ejecutar sistema completo
results <- main()
