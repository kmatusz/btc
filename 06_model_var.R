library(xts)
library(quantmod)
library(forecast)
library(tidyverse)

library(vars)
source('functions/function_testdf.R')

btc_raw <- read_csv("data/Gemini_BTCUSD_d.csv", 
                    skip = 1)

eth_raw <- read_csv("data/Gemini_ETHUSD_d.csv", 
                    skip = 1)


# data preparation to xts ----

date_to <- as.Date('2018-10-01')

eth_raw %>% 
  mutate(eth = Close) %>% 
  select(Date, eth) %>% 
  rename(date = Date) %>% 
  filter(date >= date_to) -> eth_cleaned

btc_raw %>%
  mutate(Date = as.Date(Date)) %>%
  mutate(btc = Close) %>%
  select(Date, btc) %>%
  rename(date = Date) %>% 
  filter(date >= date_to) -> btc_cleaned


full_data <- eth_cleaned %>%
  full_join(btc_cleaned)



df <- xts(full_data[,-1], # data columns (without the first column with date)
          full_data$date) # date/time index

# select out of sample period as 2020-02- 2020-05-05
df['/2020-01-31',]
# 




df_main <- df['/2020-01-31',]
df_main <- df['2020-01-31/2020-03-31',]
df_test <- df['2020-04-01/',]


df_main

df_plot <- df_main
df_plot$eth_n <- df_main$eth/max(df_main$eth)
df_plot$btc_n <- df_main$btc/max(df_main$btc)

# normalized plot
autoplot(df_plot[,c('eth_n', 'btc_n')], facets = NULL)


# Test cointegration with ECM


model.coint <- lm(btc ~ eth, 
                  data = df_main)

summary(model.coint)
# Value for eth is significant

testdf(variable = residuals(model.coint), 
       max.augmentations = 10)
# residuals are stationary
# so the series are cointegrated
# (inference from VAR is impossible)

plot(residuals(model.coint))
# not stationary plot of residuals 

# Finding appropriate VAR model
VARselect(df_main, # input data for VAR
          lag.max = 10)     # maximum lag
# Appropriate number of lags selected by all criteria is 1 

# model 1 - VAR(1) -----

model1 <- VAR(df_main, p = 1)
summary(model1)
# as series are cointegrated, coefficients standard errors cannot be trusted

# diagnostics
serial.test(model1)
# residuals are not autocorrelated

ggAcf(residuals(model1$varresult$eth))
ggPacf(residuals(model1$varresult$eth))

# 4. lag significant

ggAcf(residuals(model1$varresult$btc))
ggPacf(residuals(model1$varresult$btc))
# all insignificant


# model 2 - VAR(4) -----

model2 <- VAR(df_main, p = 4)
summary(model2)
# as series are cointegrated, coefficients standard errors cannot be trusted

# diagnostics
serial.test(model2)
# residuals are not autocorrelated

ggAcf(residuals(model2$varresult$eth))
ggPacf(residuals(model2$varresult$eth))

# No significant lags

ggAcf(residuals(model2$varresult$btc))
ggPacf(residuals(model2$varresult$btc))
# all insignificant


AIC(model1, model2)
BIC(model1, model2)

model1_f <- predict(model1,
                                 n.ahead = 31,
                                 ci = 0.95) # 95% confidence interval

model1_f$fcst$btc[,1]
model1_f$fcst$eth[,1]

johan.test.trace <- ca.jo(df_main,         # data 
                          ecdet = "const", # "none" for no intercept in cointegrating equation, 
                          # "const" for constant term in cointegrating equation and 
                          # "trend" for trend variable in cointegrating equation
                          type = "trace",  # type of the test: trace or eigen
                          K = 4) 

summary(johan.test.trace) 


