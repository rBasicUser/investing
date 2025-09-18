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
  daily_returns <- dailyReturn(prices, type = "arithmetic")
  daily_returns_log <- dailyReturn(prices, type = "log")

  weekly_returns <- weeklyReturn(prices, n = 5, type = "arithmetic")
  weekly_returns_log <- weeklyReturn(prices, n = 5, type = "log")

  monthly_returns <- monthlyReturn(prices, n = 30, type = "arithmetic")
  monthly_returns_log <- monthlyReturn(prices, n = 30, type = "log")

  yearly_returns <- yearlyReturn(prices, n = 365, type = "arithmetic")
  yearly_returns_log <- yearlyReturn(prices, n = 365, type = "log")


  # Combinar todo
  processed_data <- merge(
    prices,
    daily_returns,
    daily_returns_log,
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
