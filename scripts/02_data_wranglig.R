library(tidyverse)



# Leer parámetros y lista de tickers
params <- read_yaml("params.yaml")
stocks <- params$stocks

AAPL <- getSymbols("AAPL", from = "2007-01-01", auto.assign = FALSE)
