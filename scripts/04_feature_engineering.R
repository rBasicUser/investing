# =============================================================================
# DATA PROCESSING - INDICADORES Y SEÑALES DE TRADING
# =============================================================================

library(tidyverse)
library(xts)
library(TTR)
library(quantmod)
library(glue)

# Función para procesar un stock individual
for(s in stocks) {
  
  prices <- readRDS(glue("data/raw/{stock_symbol}_prices.rds"))}

# =============================================================================
# INDICADORES TÉCNICOS
# =============================================================================
  
  # Precios de cierre para indicadores
  Adose_prices <- Ad(prices)
  high_prices <- Hi(prices)
  low_prices <- Lo(prices)
  volume <- Vo(prices)
  
  # Medias móviles
  sma_10 <- SMA(Ad(prices), n = 10)
  sma_20 <- SMA(Ad(prices), n = 20)
  sma_50 <- SMA(Ad(prices), n = 50)
  sma_200 <- SMA(Ad(prices), n = 200)

  # RSI
  rsi_14 <- RSI(Adose_prices, n = 14)
  
  # MACD
  macd_result <- MACD(Adose_prices, nFast = 12, nSlow = 26, nSig = 9)
  
  # Bollinger Bands
  bb_result <- BBands(HLC(prices), n = 20, sd = 2)
  
  # =============================================================================
  # VOLATILIDAD
  # =============================================================================
  
  # Volatilidad rolling (20 días)
  vol_20 <- rollapply(returns_log, width = 20, FUN = sd, na.rm = TRUE, fill = NA) * sqrt(252)
  
  # =============================================================================
  # SEÑALES BÁSICAS
  # =============================================================================
  
  # Señales de MA crossover
  signal_ma_cross <- ifelse(sma_10 > sma_20, 1, -1)
  
  # Señales RSI
  signal_rsi_oversold <- ifelse(rsi_14 < 30, 1, 0)
  signal_rsi_overbought <- ifelse(rsi_14 > 70, -1, 0)
  signal_rsi <- signal_rsi_oversold + signal_rsi_overbought
  
  # Señales MACD
  signal_macd <- ifelse(macd_result[,"macd"] > macd_result[,"signal"], 1, -1)
  
  # Señales Bollinger Bands
  signal_bb_lower <- ifelse(Adose_prices <= bb_result[,"dn"], 1, 0)  # Compra en banda inferior
  signal_bb_upper <- ifelse(Adose_prices >= bb_result[,"up"], -1, 0) # Venta en banda superior
  signal_bb <- signal_bb_lower + signal_bb_upper


# Extraer solo las señales de todos los stocks
create_signal_summary <- function() {
  
  signal_data <- list()
  
  for (stock in names(processed_stocks)) {
    data <- processed_stocks[[stock]]
    
    # Extraer columnas de señales y precio de cierre
    signals <- data[, grepl("Signal|Adose", names(data))]
    
    # Crear señal combinada simple (promedio de señales)
    signal_cols <- grepl("Signal", names(signals))
    combined_signal <- rowMeans(signals[, signal_cols], na.rm = TRUE)
    
    # Crear data frame para este stock
    stock_summary <- data.frame(
      date = index(data),
      stock = stock,
      price = as.numeric(signals[, grepl("Adose", names(signals))]),
      signal_ma = as.numeric(signals[, grepl("Signal.MA", names(signals))]),
      signal_rsi = as.numeric(signals[, grepl("Signal.RSI", names(signals))]),
      signal_macd = as.numeric(signals[, grepl("Signal.MACD", names(signals))]),
      signal_bb = as.numeric(signals[, grepl("Signal.BB", names(signals))]),
      signal_combined = as.numeric(combined_signal),
      stringsAsFactors = FALSE
    )
    
    signal_data[[stock]] <- stock_summary
  }
  
  # Combinar todos los stocks
  all_signals <- bind_rows(signal_data)
  
  return(all_signals)
}

# Crear resumen de señales
portfolio_signals <- create_signal_summary()

# Guardar señales consolidadas
saveRDS(portfolio_signals, "data/processed/portfolio_signals.rds")

# Opcional: Guardar como CSV para fácil visualización
write.csv(portfolio_signals, "data/processed/portfolio_signals.csv", row.names = FALSE)

# =============================================================================
# MENSAJE DE CONFIRMACIÓN
# =============================================================================

cat("\n=== PROCESAMIENTO COMPLETADO ===\n")
cat(glue("Stocks procesados: {length(processed_stocks)}\n"))
cat(glue("Archivos generados en: data/processed/\n"))
cat(glue("Señales consolidadas: {nrow(portfolio_signals)} observaciones\n"))

# Mostrar últimas señales
cat("\n=== ÚLTIMAS SEÑALES (Top 5) ===\n")
latest_signals <- portfolio_signals %>% 
  arrange(desc(date)) %>% 
  head(15)

print(latest_signals)