library(tidyverse)
library(glue)
library(yaml)
library(scales)   # Para alpha en gráficos

# Función para calcular métricas de riesgo
calculate_risk_metrics <- function(returns) {
  wealth <- cumprod(1 + returns)
  drawdown <- (cummax(wealth) - wealth) / cummax(wealth)
  volatility = sd(returns, na.rm = TRUE)
  var_95 = quantile(returns, 0.05, na.rm = TRUE)
  var_99 = quantile(returns, 0.01, na.rm = TRUE)
  cvar_95 = mean(returns[returns <= quantile(returns, 0.05, na.rm = TRUE)], na.rm = TRUE)
  max_drawdown = max(drawdown, na.rm = TRUE)
  sharpe_ratio = mean(returns, na.rm = TRUE) / sd(returns, na.rm = TRUE)
  
  risk_metrics <- tibble(
  Métrica         = c("Volatilidad","VaR (95%)","VaR (99%)","CVaR (95%)","Máx. Drawdown","Sharpe Ratio"),
  Valor           = c(
    round(volatility, 4),
    round(var_95,       4),
    round(var_99,       4),
    round(cvar_95,      4),
    round(max_drawdown, 4),
    round(sharpe_ratio, 4))
  )
  return(risk_metrics)
}

# Función para métricas de precisión del modelo
calculate_accuracy_metrics <- function(actual, predicted) {
  residuals <- actual - predicted
  list(
    mae = mean(abs(residuals), na.rm = TRUE),
    rmse = sqrt(mean(residuals^2, na.rm = TRUE)),
    mape = mean(abs(residuals / actual) * 100, na.rm = TRUE),
    r_squared = cor(actual, predicted, use = "complete.obs")^2
  )
}