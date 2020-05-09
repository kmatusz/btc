suppressWarnings({
suppressPackageStartupMessages({
library(xts)
library(quantmod)
library(forecast)
library(tidyverse)

library(vars)
}
)
})
source('functions/function_testdf.R')

# save(df_main, file = 'data/series.Rdata')

load(file = 'data/series.Rdata')

#' Plot time both time series - as Etherum is way less priced max normalizing the values for plot
df_plot <- df_main
df_plot$eth_n <- df_main$eth/max(df_main$eth)
df_plot$btc_n <- df_main$btc/max(df_main$btc)

#' normalized plot
autoplot(df_plot[,c('eth_n', 'btc_n')], facets = NULL)

#' Test cointegration with ECM

model.coint <- lm(btc ~ eth, 
                  data = df_main)

summary(model.coint)

#' Test stationarity
testdf(variable = residuals(model.coint), 
       max.augmentations = 10)
#' First augumentation is enough. Non-stationarity is not rejected.

#' Plot of residuals:
plot(residuals(model.coint), type = 'l')
#' Indeed, does not look stationary.



#' Johansen test
johan.test.trace <- ca.jo(df_main,         # data 
                          ecdet = "const", # "none" for no intercept in cointegrating equation, 
                          # "const" for constant term in cointegrating equation and 
                          # "trend" for trend variable in cointegrating equation
                          type = "trace",  # type of the test: trace or eigen
                          K = 30) 

summary(johan.test.trace) 

#' Null not rejected. No cointegrating vector.