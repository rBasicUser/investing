# Librería

library(tidyverse)
library(quantmod)

source("R/xts to df.R")

# Obtener último año de data del M2

getSymbols("WM2NS",src="FRED",periodicity="monthly")

WM2NS <-xts_to_dataframe(WM2NS)

write_csv(WM2NS,"data/external/WM2NS.csv")
