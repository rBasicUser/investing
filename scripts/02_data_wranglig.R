library(tidyverse)
library(lubridate)
library(yaml)
library(glue)

# Función para obtener el cierre más cercano hacia atrás
get_prev_close <- function(dates, closes, offset_days) {
  map2_dbl(dates, seq_along(dates), ~{
    target <- dates[.y] - days(offset_days)
    idx    <- which(dates <= target)
    if (length(idx) == 0) NA_real_ else closes[max(idx)]
  })
}

# Función que, dado un tibble con date y close, calcula daily/weekly/monthly/yoy returns
compute_returns <- function(df) {
  df %>%
    arrange(date) %>%
    mutate(
      # Retorno diario
      daily_new    = (close / lag(close, 1)) - 1,
      # Retorno semanal (7 días atrás o más cercano)
      prev_wk      = get_prev_close(date, close, 7),
      weekly_new   = (close / prev_wk) - 1,
      # Retorno mensual (30 días atrás o más cercano)
      prev_mth     = get_prev_close(date, close, 30),
      monthly_new  = (close / prev_mth) - 1,
      # Retorno YoY (365 días atrás o más cercano)
      prev_yoy     = get_prev_close(date, close, 365),
      yoy_new      = (close / prev_yoy) - 1
    ) %>%
    # Dejamos solo date + columnas nuevas de retorno
    select(date, daily_new, weekly_new, monthly_new, yoy_new)
}

# Fecha de “ayer” para chequeo de NAs
yesterday <- Sys.Date() - 1

# Leer parámetros y lista de tickers
params <- read_yaml("params.yaml")
stocks <- params$stocks

for (s in stocks) {
  # Cargar serie histórica
  df <- read_csv(
    glue("data/processed/{s}_full.csv"),
    col_types = cols(date = col_date())
  ) %>% arrange(date)
  
  # Si no existen las columnas de retorno, inicializarlas
  if (!all(c("daily_return","weekly_return","monthly_return","yoy_return") %in% names(df))) {
    df <- df %>%
      mutate(
        daily_return   = NA_real_,
        weekly_return  = NA_real_,
        monthly_return = NA_real_,
        yoy_return     = NA_real_
      )
    needs_update <- TRUE
  } else {
    # Chequear si hay NAs desde ayer
    needs_update <- df %>%
      filter(date >= yesterday) %>%
      summarize(any_na = any(
        is.na(daily_return) |
        is.na(weekly_return) |
        is.na(monthly_return) |
        is.na(yoy_return)
      )) %>%
      pull(any_na)
  }
  
  if (needs_update) {
    # Calculamos todos los retornos "nuevos"
    new_rets <- compute_returns(df)
    
    # Actualizamos sólo donde había NA en las fechas recientes
    df_upd <- df %>%
      left_join(new_rets, by = "date") %>%
      mutate(
        daily_return   = coalesce(daily_return,   daily_new),
        weekly_return  = coalesce(weekly_return,  weekly_new),
        monthly_return = coalesce(monthly_return, monthly_new),
        yoy_return     = coalesce(yoy_return,     yoy_new)
      ) %>%
      # Eliminamos solo las columnas *_new
      select(-ends_with("_new"))
    
    # Escribir de vuelta el CSV
    write_csv(df_upd, glue("data/processed/{s}_full.csv"))
    message(glue("{s}: retornos actualizados desde {yesterday}"))
  } else {
    message(glue("{s}: sin NAs desde {yesterday}, no se requiere actualización"))
  }
}
