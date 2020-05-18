library(xts)
library(quantmod)
library(forecast)
library(vars)
library(tidyverse)

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


df_main <- df['/2020-01-31',]
df_main <- df['2020-01-31/2020-03-31',]
df_test <- df['2020-04-01/',]



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
# but all are insignificant

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

# one step forecast


df_test2 <- df_test
df_test2$btc <- df_test$btc*1000
predict(model1)

model_t <- model1

predict(model1$varresult$eth, newdata = as_tibble(model1$datamat))

a <- lm(eth~eth.l1+btc.l1, data=as_tibble(model1$datamat))
predict(a, newdata = as_tibble(model1$datamat))


# var na train + test
# weź datamat
# zrób lm dla danych train
# predict(lm, newdata = datamat z  train+ test)

# P = 1
model1_tt <- VAR(df_main, p = 1)
preprocessed_data_train <- model1_tt$datamat

model1_tt <- VAR(df_test,p = 1)
preprocessed_data_test <- model1_tt$datamat

lm_eth <- lm(eth~eth.l1+btc.l1, data = preprocessed_data_train)

one_step_predictions_test_eth_1 <- predict(lm_eth, newdata = preprocessed_data_test)

lm_btc <- lm(btc~eth.l1+btc.l1, data = preprocessed_data_train)

one_step_predictions_test_btc_1 <- predict(lm_btc, newdata = preprocessed_data_test)

# P = 4
model1_tt <- VAR(df_main, p = 4)
preprocessed_data_train <- model1_tt$datamat

model1_tt <- VAR(df_test,p = 4)
preprocessed_data_test <- model1_tt$datamat

lm_eth <- lm(eth~eth.l1+btc.l1+
               eth.l2+btc.l2+
               eth.l3+btc.l3+
               eth.l4+btc.l4, data = preprocessed_data_train)

one_step_predictions_test_eth_4 <- predict(lm_eth, newdata = preprocessed_data_test)

lm_btc <- lm(btc~eth.l1+btc.l1+
               eth.l2+btc.l2+
               eth.l3+btc.l3+
               eth.l4+btc.l4, data = preprocessed_data_train)

one_step_predictions_test_btc_4 <- predict(lm_btc, newdata = preprocessed_data_test)
predict(model1,n.ahead = 1)

save(
  df_test,
  model1,
  model2,
  one_step_predictions_test_eth_1,
  one_step_predictions_test_btc_1,
  one_step_predictions_test_eth_4,
  one_step_predictions_test_btc_4,
  file = 'data/06_outputs.Rdata'
)



# Porównanie

rmse_out_sample <- rbind(
  accuracy(one_step_predictions_test_eth_1, df_test$eth[2:nrow(df_test),1]),
  accuracy(one_step_predictions_test_eth_4, df_test$eth[5:nrow(df_test),1])
)[,2]


# model1, model2, model3, model4, model5, model6, model7
cbind(
  AIC(model1, model2),
  BIC(model1, model2),
  rmse_out_sample) -> measures

measures[c(3)] <- NULL

measures %>%
  as_tibble(rownames = 'model_no') -> measures


measures %>%
  arrange(rmse_out_sample)
# 3,5,1,7,2

measures %>%
  arrange(AIC)
# 7,3,5,2,1

measures %>%
  arrange(BIC)
# 2,1,7,5,3




