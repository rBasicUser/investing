library(quantmod)
library(yaml)
library(glue)

params <- read_yaml("params.yaml")
stocks <- params$stocks

if (!dir.exists("data/raw")) dir.create("data/raw", recursive = TRUE)

for (s in stocks) {
  
  path <- glue("data/raw/{s}_full.rds")
  
  if (!file.exists(path)) {
    # Primera vez: traemos todo y guardamos como xts
    stock_data <- getSymbols(s, from = "2007-01-01", auto.assign = FALSE)
    saveRDS(stock_data, path)
    message(glue("{s}: datos descargados desde 2007-01-01"))
    
  } else {
    # Ya existe: leemos el xts
    stock_data <- readRDS(path)
    
    # Última fecha del índice, como Date
    last_date <- as.Date(last(index(stock_data)))
    
    # Si está desactualizado (hoy - 1 día hábil)
    if (last_date < Sys.Date() - 1) {
      # Traer sólo desde el día siguiente
      new_data <- getSymbols(s,
                             from = last_date + 1,
                             to   = Sys.Date(),
                             auto.assign = FALSE)
      
      # Apilar xts viejo y nuevo
      stock_data <- rbind(stock_data, new_data)
      
      # Guardar de nuevo
      saveRDS(stock_data, path)
      message(glue("{s}: actualizados hasta {as.Date(last(index(stock_data)))}"))
      
    } else {
      message(glue("{s}: ya estaba actualizado hasta {last_date}"))
    }
  }
}
