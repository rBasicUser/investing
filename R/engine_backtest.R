library(tidyverse)
library(lubridate)

# ================================
# SISTEMA DE BACKTESTING COMPLETO
# ================================

# Función principal de backtesting
run_backtest <- function(signals_data, initial_capital = 100000, position_size = 5000, 
                        transaction_cost = 0.001, start_date = "2020-01-01", 
                        end_date = "2024-12-31") {
  
  # Configuración inicial
  portfolio <- list(
    cash = initial_capital,
    positions = data.frame(),
    transactions = data.frame(),
    daily_values = data.frame(),
    metrics = list()
  )
  
  # Procesar señales día a día
  dates <- seq(as.Date(start_date), as.Date(end_date), by = "day")
  
  for (date in as.character(dates)) {
    # Obtener señales del día
    daily_signals <- signals_data %>% 
      filter(date == !!date)
    
    if (nrow(daily_signals) > 0) {
      # Procesar cada señal
      for (i in 1:nrow(daily_signals)) {
        signal <- daily_signals[i, ]
        portfolio <- process_signal(portfolio, signal, position_size, transaction_cost)
      }
    }
    
    # Actualizar valor diario del portafolio
    portfolio <- update_portfolio_value(portfolio, date, signals_data)
  }
  
  # Calcular métricas finales
  portfolio$metrics <- calculate_portfolio_metrics(portfolio)
  
  return(portfolio)
}

# Procesar señal individual
process_signal <- function(portfolio, signal, position_size, transaction_cost) {
  
  symbol <- signal$symbol
  action <- signal$signal  # BUY, SELL, HOLD
  price <- signal$current_price
  date <- signal$date
  
  if (action == "BUY") {
    # Verificar si tenemos efectivo y no tenemos posición
    if (portfolio$cash >= position_size && 
        !symbol %in% portfolio$positions$symbol) {
      
      shares <- floor((position_size * (1 - transaction_cost)) / price)
      cost <- shares * price * (1 + transaction_cost)
      
      if (shares > 0 && cost <= portfolio$cash) {
        # Agregar posición
        new_position <- data.frame(
          symbol = symbol,
          shares = shares,
          entry_price = price,
          entry_date = date,
          stringsAsFactors = FALSE
        )
        
        portfolio$positions <- rbind(portfolio$positions, new_position)
        portfolio$cash <- portfolio$cash - cost
        
        # Registrar transacción
        transaction <- data.frame(
          date = date,
          symbol = symbol,
          action = "BUY",
          shares = shares,
          price = price,
          cost = cost,
          stringsAsFactors = FALSE
        )
        
        portfolio$transactions <- rbind(portfolio$transactions, transaction)
      }
    }
    
  } else if (action == "SELL") {
    # Verificar si tenemos la posición
    if (symbol %in% portfolio$positions$symbol) {
      position_idx <- which(portfolio$positions$symbol == symbol)
      position <- portfolio$positions[position_idx, ]
      
      proceeds <- position$shares * price * (1 - transaction_cost)
      
      # Eliminar posición
      portfolio$positions <- portfolio$positions[-position_idx, ]
      portfolio$cash <- portfolio$cash + proceeds
      
      # Registrar transacción
      transaction <- data.frame(
        date = date,
        symbol = symbol,
        action = "SELL",
        shares = position$shares,
        price = price,
        cost = -proceeds,  # Negativo porque es ingreso
        stringsAsFactors = FALSE
      )
      
      portfolio$transactions <- rbind(portfolio$transactions, transaction)
    }
  }
  
  return(portfolio)
}

# Actualizar valor diario del portafolio
update_portfolio_value <- function(portfolio, date, price_data) {
  
  total_value <- portfolio$cash
  
  if (nrow(portfolio$positions) > 0) {
    # Obtener precios actuales
    current_prices <- price_data %>% 
      filter(date == !!date) %>% 
      select(symbol, current_price)
    
    # Valorar posiciones
    positions_value <- portfolio$positions %>% 
      left_join(current_prices, by = "symbol") %>% 
      mutate(
        current_price = ifelse(is.na(current_price), entry_price, current_price),
        position_value = shares * current_price
      ) %>% 
      summarise(total_positions = sum(position_value, na.rm = TRUE)) %>% 
      pull(total_positions)
    
    total_value <- total_value + positions_value
  }
  
  # Guardar valor diario
  daily_value <- data.frame(
    date = as.Date(date),
    portfolio_value = total_value,
    cash = portfolio$cash,
    positions_value = total_value - portfolio$cash,
    stringsAsFactors = FALSE
  )
  
  portfolio$daily_values <- rbind(portfolio$daily_values, daily_value)
  
  return(portfolio)
}

# Calcular métricas del portafolio
calculate_portfolio_metrics <- function(portfolio) {
  
  daily_returns <- portfolio$daily_values %>% 
    arrange(date) %>% 
    mutate(
      daily_return = (portfolio_value - lag(portfolio_value)) / lag(portfolio_value),
      cumulative_return = (portfolio_value - first(portfolio_value)) / first(portfolio_value)
    ) %>% 
    filter(!is.na(daily_return))
  
  # Métricas básicas
  total_return <- tail(daily_returns$cumulative_return, 1)
  annualized_return <- (1 + total_return)^(252/nrow(daily_returns)) - 1
  volatility <- sd(daily_returns$daily_return, na.rm = TRUE) * sqrt(252)
  sharpe_ratio <- annualized_return / volatility
  
  # Drawdown
  daily_returns <- daily_returns %>% 
    mutate(
      peak = cummax(portfolio_value),
      drawdown = (portfolio_value - peak) / peak
    )
  
  max_drawdown <- min(daily_returns$drawdown, na.rm = TRUE)
  
  # Número de operaciones
  total_trades <- nrow(portfolio$transactions)
  winning_trades <- portfolio$transactions %>% 
    filter(action == "SELL") %>% 
    left_join(
      portfolio$transactions %>% filter(action == "BUY"),
      by = "symbol",
      suffix = c("_sell", "_buy")
    ) %>% 
    mutate(profit = (price_sell - price_buy) * shares_sell) %>% 
    summarise(wins = sum(profit > 0, na.rm = TRUE)) %>% 
    pull(wins)
  
  win_rate <- ifelse(total_trades > 0, winning_trades / (total_trades/2), 0)
  
  metrics <- list(
    total_return = total_return,
    annualized_return = annualized_return,
    volatility = volatility,
    sharpe_ratio = sharpe_ratio,
    max_drawdown = max_drawdown,
    total_trades = total_trades,
    win_rate = win_rate,
    final_value = tail(portfolio$daily_values$portfolio_value, 1)
  )
  
  return(metrics)
}

# Función para Buy & Hold
run_buy_hold_backtest <- function(symbols, price_data, initial_capital = 100000,
                                 start_date = "2020-01-01", end_date = "2024-12-31") {
  
  # Dividir capital igualmente entre todos los símbolos
  capital_per_stock <- initial_capital / length(symbols)
  
  portfolio_value <- price_data %>% 
    filter(date >= as.Date(start_date), date <= as.Date(end_date)) %>% 
    group_by(symbol) %>% 
    arrange(date) %>% 
    mutate(
      shares = ifelse(row_number() == 1, 
                     floor(capital_per_stock / current_price), 
                     first(floor(capital_per_stock / current_price))),
      position_value = shares * current_price
    ) %>% 
    group_by(date) %>% 
    summarise(
      total_value = sum(position_value, na.rm = TRUE),
      .groups = 'drop'
    ) %>% 
    arrange(date)
  
  # Calcular métricas
  daily_returns <- portfolio_value %>% 
    mutate(
      daily_return = (total_value - lag(total_value)) / lag(total_value),
      cumulative_return = (total_value - first(total_value)) / first(total_value)
    ) %>% 
    filter(!is.na(daily_return))
  
  total_return <- tail(daily_returns$cumulative_return, 1)
  annualized_return <- (1 + total_return)^(252/nrow(daily_returns)) - 1
  volatility <- sd(daily_returns$daily_return, na.rm = TRUE) * sqrt(252)
  sharpe_ratio <- annualized_return / volatility
  
  # Drawdown
  daily_returns <- daily_returns %>% 
    mutate(
      peak = cummax(total_value),
      drawdown = (total_value - peak) / peak
    )
  
  max_drawdown <- min(daily_returns$drawdown, na.rm = TRUE)
  
  buy_hold_metrics <- list(
    total_return = total_return,
    annualized_return = annualized_return,
    volatility = volatility,
    sharpe_ratio = sharpe_ratio,
    max_drawdown = max_drawdown,
    final_value = tail(portfolio_value$total_value, 1)
  )
  
  return(list(
    daily_values = portfolio_value,
    metrics = buy_hold_metrics
  ))
}

# Función para comparar estrategias
compare_strategies <- function(strategy_results, buy_hold_results) {
  
  comparison <- data.frame(
    Metric = c("Total Return", "Annualized Return", "Volatility", 
               "Sharpe Ratio", "Max Drawdown", "Final Value"),
    Strategy = c(
      paste0(round(strategy_results$metrics$total_return * 100, 2), "%"),
      paste0(round(strategy_results$metrics$annualized_return * 100, 2), "%"),
      paste0(round(strategy_results$metrics$volatility * 100, 2), "%"),
      round(strategy_results$metrics$sharpe_ratio, 3),
      paste0(round(strategy_results$metrics$max_drawdown * 100, 2), "%"),
      paste0("$", format(round(strategy_results$metrics$final_value, 0), big.mark = ","))
    ),
    BuyHold = c(
      paste0(round(buy_hold_results$metrics$total_return * 100, 2), "%"),
      paste0(round(buy_hold_results$metrics$annualized_return * 100, 2), "%"),
      paste0(round(buy_hold_results$metrics$volatility * 100, 2), "%"),
      round(buy_hold_results$metrics$sharpe_ratio, 3),
      paste0(round(buy_hold_results$metrics$max_drawdown * 100, 2), "%"),
      paste0("$", format(round(buy_hold_results$metrics$final_value, 0), big.mark = ","))
    ),
    stringsAsFactors = FALSE
  )
  
  return(comparison)
}

# Función para generar reporte
generate_backtest_report <- function(strategy_results, buy_hold_results, strategy_name) {
  
  cat("=================================\n")
  cat("REPORTE DE BACKTESTING\n")
  cat("=================================\n\n")
  
  cat("Estrategia:", strategy_name, "\n")
  cat("Período de análisis:", min(strategy_results$daily_values$date), "a", 
      max(strategy_results$daily_values$date), "\n\n")
  
  # Tabla comparativa
  comparison <- compare_strategies(strategy_results, buy_hold_results)
  print(comparison)
  
  cat("\n")
  cat("Número total de operaciones:", strategy_results$metrics$total_trades, "\n")
  cat("Tasa de acierto:", paste0(round(strategy_results$metrics$win_rate * 100, 1), "%"), "\n")
  
  return(comparison)
}