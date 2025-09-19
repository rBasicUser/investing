# ================================
# 0. Librerías
# ================================
library(yaml)
library(glue)
library(quantmod)
library(tidyverse)
library(janitor)

# ================================
# 1. Configuración Inicial
# ================================
params <- read_yaml("params.yaml")
stocks <- params$stocks

# ================================
# 2. Bucle de Procesamiento
# ================================
for (s in stocks) {
  # 2.1 Leer datos crudos
  prices_raw <- readRDS(glue("data/raw/{s}_full.rds"))
  
  # 2.2 Extraer precio ajustado
  prices <- Ad(prices_raw)
  
  # 2.3 Calcular retornos
  returns_daily       <- dailyReturn(prices, type = "arithmetic")
  returns_daily_log   <- dailyReturn(prices, type = "log")
  returns_weekly      <- weeklyReturn(prices, type = "arithmetic")
  returns_weekly_log  <- weeklyReturn(prices, type = "log")
  returns_monthly     <- monthlyReturn(prices, type = "arithmetic")
  returns_monthly_log <- monthlyReturn(prices, type = "log")
  returns_yearly      <- yearlyReturn(prices, type = "arithmetic")
  returns_yearly_log  <- yearlyReturn(prices, type = "log")
  
  # 2.4 Combinar todos los xts en uno solo
  combined_xts <- merge(
    prices,
    returns_daily,
    returns_daily_log,
    returns_weekly,
    returns_weekly_log,
    returns_monthly,
    returns_monthly_log,
    returns_yearly,
    returns_yearly_log
  )
  
  # 2.5 Convertir a tibble y renombrar columnas con glue()
  processed_tibble <- as_tibble(
    data.frame(
      date = index(combined_xts),
      coredata(combined_xts)
    )
  ) |>
    rename_with(~ glue("{s}_{.x}"), -date)|>
    clean_names()
  
  # 2.6 Guardar resultado
  saveRDS(
    processed_tibble,
    glue("data/processed/returns/{s}_returns.rds")
  )
}
