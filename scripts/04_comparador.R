library(tidyverse)

model_montecarlo <- readRDS("models/MonteCarlo/model_monetcarlo_AAPL.rds")
model_ARIMA <- readRDS("models/ARIMA/model_ARIMA_AAPL.rds")
model_regression   <- readRDS("models/Regression/model_regression_AAPL.rds")
model_SMA   <- readRDS("models/SMA/model_SMA_AAPL.rds")|>
  rename(pred=sma_prediction)

