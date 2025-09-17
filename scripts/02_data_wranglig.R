library(quantmod)
library(janitor)
library(glue)
library(tidyverse)
library(yaml)

params <- read_yaml("params.yaml")
stocks <- params$stocks

# Función para procesar un stock individual
for (s in stocks) {
  prices <- readRDS(glue("data/raw/{s}_full.rds"))

  # Precio ajustado
  prices <- Ad(prices)

  # Retornos diarios con quantmod
  returns <- dailyReturn(prices, type = "arithmetic")
  returns_log <- dailyReturn(prices, type = "log")

  weekly_returns <- ROC(prices, n = 5, type = "discrete")
  weekly_returns_log <- ROC(prices, n = 5, type = "continuous")

  monthly_returns <- ROC(prices, n = 30, type = "discrete")
  monthly_returns_log <- ROC(prices, n = 30, type = "continuous")

  yearly_returns <- ROC(prices, n = 365, type = "discrete")
  yearly_returns_log <- ROC(prices, n = 365, type = "continuous")

  # Asignar nombres descriptivos a las columnas ANTES del merge
  colnames(returns) <- paste0(s, "_daily_return")
  colnames(returns_log) <- paste0(s, "_daily_return_log")
  colnames(weekly_returns) <- paste0(s, "_weekly_return")
  colnames(weekly_returns_log) <- paste0(s, "_weekly_return_log")
  colnames(monthly_returns) <- paste0(s, "_monthly_return")
  colnames(monthly_returns_log) <- paste0(s, "_monthly_return_log")
  colnames(yearly_returns) <- paste0(s, "_yearly_return")
  colnames(yearly_returns_log) <- paste0(s, "_yearly_return_log")

  # Combinar todo
  processed_data <- merge(
    prices,
    returns,
    weekly_returns,
    weekly_returns_log,
    monthly_returns,
    monthly_returns_log,
    yearly_returns,
    yearly_returns_log
  )

  # Convertir a tibble y guardar
  processed_tibble <- as_tibble(
    data.frame(
      date = index(processed_data),
      coredata(processed_data),
      check.names = FALSE
    )
  )

  saveRDS(processed_tibble, glue("data/processed/returns/{s}_returns.rds"))
}
