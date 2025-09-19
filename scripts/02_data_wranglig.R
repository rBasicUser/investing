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

  weekly_returns <- weeklyReturn(prices,type = "arithmetic")
  weekly_returns_log <- weeklyReturn(prices,type = "log")

  monthly_returns <- monthlyReturn(prices,type = "arithmetic")
  monthly_returns_log <- monthlyReturn(prices,type = "log")
  
  yearly_returns <- yearlyReturn(prices,type = "arithmetic")
  yearly_returns_log <- yearlyReturn(prices,type = "log")
  

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
