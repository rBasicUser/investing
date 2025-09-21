# Librería

library(tidyverse)
library(quantmod)
library(yaml)

params <- read_yaml("params.yaml")
external_data <- params$external_data


# Datos de la FRED

for (e in external_data) {
  variable <- getSymbols(
    e,
    src = "FRED",
    periodicity = "monthly",
    auto.assign = FALSE
  )
  variable <- data.frame(
    date = index(external_data),
    coredata(external_data)
  )
  saveRDS(external_data, glue("data/external/{e}_full.rds"))
}
