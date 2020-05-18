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




df_main <- df['2020-01-31/2020-03-31',]
df_test <- df['2020-04-01/',]


df_main

df_plot <- df_main
df_plot$eth_n <- df_main$eth/max(df_main$eth)
df_plot$btc_n <- df_main$btc/max(df_main$btc)

# normalized plot
autoplot(df_plot[,c('eth_n', 'btc_n')], facets = NULL)


eth <- df_main$eth
eth_test <- df_test$eth

# quick EDA ----

autoplot(eth)

ggAcf(eth)
# slowly declining
ggPacf(eth)
# only first significant



testdf(eth, max.augmentations = 8)
# 1 augumentation is enough
# not stationary

testdf(diff(eth), max.augmentations = 8)
# now it's stationary - no augumentations needed



ggAcf(diff(eth), lag.max = 100)
ggAcf(diff(eth), lag.max = 30)
# only 4 lag significant
ggPacf(diff(eth), lag.max = 100)
ggPacf(diff(eth), lag.max = 30)
# significant lags: lnly lag 4 significant

# 4

# Start forecasting with Arima(4,1,4) ----

model1 <- Arima(eth$eth,  # variable
                order = c(4, 1, 4)  # (p,d,q) parameters
)

model1

checkresiduals(model1,plot = F,lag = 30)
# H0 - residuals are not correlated
# ok!

model1 %>% 
  residuals() %>% 
  ggtsdisplay()
# no significant lags

coeftest(model1)
# remove ar2, ar3

# model 2 - Arima(4, 1, 4) with lags ar1,4, ma1,2,3,4-----
fixed_ar <- c(NA, 0, 0, NA)
fixed_ma <- rep(NA, 4)

model2 <- Arima(eth$eth,  # variable
                order = c(4, 1, 4),  # (p,d,q) parameters
                fixed = c(fixed_ar, fixed_ma)  # (p,d,q) parameters
)

model2

checkresiduals(model2,plot = F)
# p.val >0.05 - nieskorelowane

model2 %>% 
  residuals() %>% 
  ggtsdisplay(lag.max = 50)
# all residuals autocorrelations are not significant. Good

coeftest(model2)
# wszystkie znaczące

autoplot(forecast(model2, h = 5))


# model 3 - Arima(4, 1, 4) with lag 4 -----

fixed_ar <- c(0, 0, 0, NA)

model3 <- Arima(eth$eth,  # variable
                order = c(4, 1, 4),  # (p,d,q) parameters
                fixed = c(fixed_ar, fixed_ar)  # (p,d,q) parameters
)

model3

checkresiduals(model3,plot = F)
# p.val >0.05 - nieskorelowane

model3 %>% 
  residuals() %>% 
  ggtsdisplay(lag.max = 50)
# all residuals autocorrelations are not significant. Good

coeftest(model3)
# w ogóle nie znaczące

autoplot(forecast(model3, h = 5))

# Model 4 ----
model4 <- auto.arima(eth,
                     d = 1,             # parameter d of ARIMA model
                     max.p = 10,         # Maximum value of p
                     max.q = 10,         # Maximum value of q
                     max.order = 10,    # maximum p+q
                     start.p = 1,       # Starting value of p in stepwise procedure
                     start.q = 1,       # Starting value of q in stepwise procedure
                     ic = "aic",        # Information criterion to be used in model selection.
                     stepwise = FALSE,  # if FALSE considers all models
                     allowdrift = TRUE, # include a constant
                     trace = TRUE)      # show summary of all models considered


checkresiduals(model4,plot = F)
# p.val >0.05 - nieskorelowane

model4 %>% 
  residuals() %>% 
  ggtsdisplay(lag.max = 50)
# all residuals autocorrelations are not significant. Good

coeftest(model4)
# tylko ma4 znaczące





# compare models ----

save(
  model1,
  model2,
  model3,
  model4,
  eth,
  eth_test,
  file = 'data/07_outputs.Rdata'
)


rmse <- rbind(
  accuracy(model1),
  accuracy(model2),
  accuracy(model3),
  accuracy(model4)
  # accuracy(model5)
  # # accuracy(model6),
  # accuracy(model7),
  # accuracy(model9)
)[,2]

# model1, model2, model3, model4, model5, model6, model7
cbind(
  AIC(model1, model2, model3, model4),
  BIC(model1, model2, model3, model4),
  rmse)




# obtain out of sample forecasts ----
test_fit1 <- Arima(eth_test, model=model1)
test_fit2 <- Arima(eth_test, model=model2)
test_fit3 <- Arima(eth_test, model=model3)
test_fit4 <- Arima(eth_test, model=model4)

onestep1 <- fitted(test_fit1)
onestep2 <- fitted(test_fit2)
onestep3 <- fitted(test_fit3)
onestep3 <- fitted(test_fit4)

rmse_out_sample <- rbind(
  accuracy(fitted(test_fit1), eth_test),
  accuracy(fitted(test_fit2), eth_test),
  accuracy(fitted(test_fit3), eth_test),
  accuracy(fitted(test_fit4), eth_test)
)[,2]


rmse_in_sample <- rbind(
  accuracy(model1),
  accuracy(model2),
  accuracy(model3),
  accuracy(model4)
)[,2]

# model1, model2, model3, model4, model5, model6, model7
cbind(
  AIC(model1, model2, model3, model4),
  BIC(model1, model2, model3, model4),
  rmse_in_sample, 
  rmse_out_sample) -> measures

measures[c(3)] <- NULL

measures %>%
  as_tibble(rownames = 'model_no') -> measures


measures %>%
  arrange(rmse_out_sample)
# 1,2,4,3

measures %>%
  arrange(AIC)
# 3,4,2,1

measures %>%
  arrange(BIC)
# 3,4,2,1

