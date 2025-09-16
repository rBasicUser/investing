library(tidyverse)
library(httr)
library(jsonlite)
library(yaml)
library(glue)

params <- read_yaml("params.yaml")

# --- CONFIGURACIÓN ---
api_key <- params$api_key
stocks <- params$stocks

# ---- RECORRER PARA CADA STOCK

for (s in stocks) {
  # --- CARGAR DATA ACUMULADA SI EXISTE ---

  path <- glue("data/processed/{s}_full.csv")

  if (!file.exists(path)) {
    last_date <- as.Date("2020-09-12") # 2025-01-06 con este formato
    stock_data  <- tibble(
    date   = as.Date(character()),
    symbol = character(),
    open   = double(),
    high   = double(),
    low    = double(),
    close  = double(),
    volume = double()
  )
    request_date = today()
  } else {
    stock_data <- read_csv(path, show_col_types = FALSE)
    last_date <- max(stock_data$date)
    request_date <- last_date + 1

  }


  # --- CONSTRUIR URL DE CONSULTA ---
  url <- glue(
    "http://api.marketstack.com/v2/eod?",
    "access_key={api_key}",
    "&symbols={s}",
    "&date_from={last_date}",
    "&date_to={request_date}",
    "&limit=1000"
  )

  # --- HACER REQUEST ---
  res <- GET(url)

  if (res$status_code != 200) {
    stop("Error en la descarga: ", res$status_code)
  }

  data_raw <- content(res, as = "parsed", type = "application/json")

  # --- VALIDAR DATOS ---
  if (is.null(data_raw$data) || length(data_raw$data) == 0) {
    message("No hay datos nuevos disponibles.")
    stop()
    
  }

  # --- PROCESAR Y LIMPIAR ---
  data_clean <- bind_rows(data_raw$data) %>%
    select(date, symbol, open, high, low, close, volume) %>%
    mutate(date = as.Date(date)) %>%
    arrange(date)

  # --- FILTRAR DUPLICADOS POR SI ACASO ---
  if (nrow(stock_data) > 0) {
    data_clean <- data_clean %>% filter(date > last_date)
  }

  # --- COMBINAR Y GUARDAR ---
  data_actualizada <- bind_rows(stock_data, data_clean) %>%
    arrange(date)

  # Guardar archivo actualizado
  write_csv(data_actualizada, glue("data/processed/{s}_full.csv"))

  # Guardar backup del día
  write_csv(data_actualizada, glue("data/backup/{s}_{Sys.Date()}.csv"))

  message("✅ Datos de ", s, " actualizados y guardados con éxito.")
}
