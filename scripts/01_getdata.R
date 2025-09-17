library(quantmod)
library(yaml)
library(glue)

params <- read_yaml("params.yaml")
stocks <- params$stocks

# ---- RECORRER PARA CADA STOCK

for (s in stocks) {
  
  path <- glue("data/raw/{s}_full.rds")

  if (!file.exists(path)) {
    
    stock_data <- getSymbols(s, from="2007-01-01", auto.assign = FALSE)
    saveRDS(stock_data, glue("data/raw/{s}_full.rds"))

  } 
  else {
    stock_data <- readRDS(path)
    last_date <- max(stock_data$date)
    
    # Verificar si necesita actualización (último día hábil)
    if(last_date >= Sys.Date() - 1) {
      # Si ya está actualizada
      message("Datos actualizados a la última fecha disponible")
      next
    }
    else {
      # Obtener datos nuevos desde la última fecha
      new_data <- getSymbols(s, from = last_date + 1, to = Sys.Date(), auto.assign = FALSE)
      
      # Convertir nuevos datos a tibble
      new_data <- as_tibble(data.frame(date = index(new_data), coredata(new_data)))
      
      # Combinar datos existentes con nuevos
      stock_data <- bind_rows(stock_data, new_data)
      
      # Guardar datos actualizados
      saveRDS(stock_data, path)
      
      message(glue("Datos de {s} actualizados hasta {max(stock_data$date)}"))
    }
  }
}