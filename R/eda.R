library(quantmod)
library(tidyverse)

# Cargar datos de retornos

df <- readRDS("data/processed/returns/AIG_returns.rds")|>
  xts::last('5 years')

df

# Encontramos que la stock sigue una tendencia alcista

plot(x=df$date, y=df$monthly.returns, type="p")
plot(x=df$date, y=df$monthly.returns.1, type = "p")

plot(x=df$date,y=df$AIG_monthly_return, type = "l")


# Cargar datos Externos
 

# Examinar Autocorrelación y PACF

acf <- acf(df$AIG_daily_return)

pacf <- pacf(df$AIG_daily_return)

# PMI y sus lags

# Inflación y sus lags



