library(xts)
library(quantmod)
library(forecast)
library(tidyverse)
source('functions/function_testdf.R')

theme_set(theme_minimal())

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

btc <- df['/2020-01-31',]$btc
btc_test <- df['2020-02-01/',]$btc

# quick EDA ----


autoplot(btc)

ggAcf(btc)
# slowly declining
ggPacf(btc)
# only first mildly significant



testdf(btc, max.augmentations = 1)
# 1 augumentation is enough
# not stationary

testdf(diff(btc), max.augmentations = 0)
# now it's stationary - no augumentations needed



ggAcf(diff(btc), lag.max = 100)
ggAcf(diff(btc), lag.max = 50)
# most lags are insignificant, except: 17, 18, 20, 44
ggPacf(diff(btc), lag.max = 100)
ggPacf(diff(btc), lag.max = 50)
# significant lags: 1, 9, 20, 40, 44, 46, 50

# All significant: 1, 9, 17, 18, 20, 40, 44, 46, 50

# Arima(1,1,1) most basic model ----

model1 <- Arima(btc$btc,  # variable
                order = c(1, 1, 1)  # (p,d,q) parameters
)

model1

checkresiduals(model1,plot = F,lag = 30)
# H0 - residuals are not correlated
# p.val < źle

model1 %>% 
  residuals() %>% 
  ggtsdisplay()
# lag 20 is still present

coeftest(model1)
# both coefficients are not significant!

# model 2 - Full Arima(20, 1, 20) -----

model2 <- Arima(btc$btc,  # variable
                order = c(20, 1, 20)  # (p,d,q) parameters
)

model2

checkresiduals(model2,plot = F)
# p.val <0.05 - skorelowane

model2 %>% 
  residuals() %>% 
  ggtsdisplay(lag.max = 50)
# all residuals autocorrelations are not significant. Good

coeftest(model2)
# some are significant

autoplot(forecast(model2, h = 95))


# model 3 - Arima(20, 1, 20) with lags 9,17,18,20 -----

fixed_ar <- rep(0, 20)
fixed_ar[c(9,17,18,20)] <- NA
fixed_ar

model3 <- Arima(btc$btc,  # variable
                order = c(20, 1, 20),  # (p,d,q) parameters
                fixed = c(fixed_ar, fixed_ar)
)

model3

checkresiduals(model3,plot = F)
# p-value < 0.05 - residuals are correlated

model3 %>% 
  residuals() %>% 
  ggtsdisplay(lag.max = 50)
# 44, 47 lags are mildly significant in pacf

coeftest(model3)
# all significant except ar18, ma18. Drop in next iteration

autoplot(forecast(model3, h = 10))


# model 4 - Arima(20, 1, 20) with lags 9,17,20 -----

fixed_ar <- rep(0, 20)
fixed_ar[c(9,17,20)] <- NA
fixed_ar

model4 <- Arima(btc$btc,  # variable
                order = c(20, 1, 20),  # (p,d,q) parameters
                fixed = c(fixed_ar, fixed_ar)
)

model4

checkresiduals(model4,plot = F)
# autocorrelated

model4 %>% 
  residuals() %>% 
  ggtsdisplay(lag.max = 50)
# 44, 46 lags are mildly significant in pacf

coeftest(model4)
# all significant except ar17, ma17.  Drop in next iteration. AR9 0.06

autoplot(forecast(model4, h = 95))


# model 5 - Arima(20, 1, 20) with lags 9,20 -----

fixed_ar <- rep(0, 20)
fixed_ar[c(9,20)] <- NA
fixed_ar

model5 <- Arima(btc$btc,  # variable
                order = c(20, 1, 20),  # (p,d,q) parameters
                fixed = c(fixed_ar, fixed_ar)
)

model5

checkresiduals(model5,plot = F)
# autocorrelated

model5 %>% 
  residuals() %>% 
  ggtsdisplay(lag.max = 50)
# 44, 46 lags are mildly significant in pacf

coeftest(model5)
# all significant AR9 0.08

autoplot(forecast(model5, h = 95))

# model 6 - Arima(50, 1, 50) with lags 9,20 and added 44, 46, 50 -----

fixed_ar <- rep(0, 50)
fixed_ar[c(9,20, 44, 46, 50)] <- NA
fixed_ar

model6 <- Arima(btc$btc,  # variable
                order = c(50, 1, 50),  # (p,d,q) parameters
                fixed = c(fixed_ar, fixed_ar),
                method="CSS"
)

model6

checkresiduals(model6,plot = F)
# autocorrelated

model6 %>% 
  residuals() %>% 
  ggtsdisplay(lag.max = 50)
# lags 1, 7 are significant

coeftest(model6)
#po dodaniu 50 dużo insignificant

autoplot(forecast(model6, h = 95))



# model 7 - Arima(50, 1, 50) with all significant lags 9, 17, 18, 20, 40, 44, 46, 50 ----


fixed_ar <- rep(0, 50)
fixed_ar[c(9,17, 18, 20, 40, 44, 46, 50)] <- NA
fixed_ar

model7 <- Arima(btc$btc,  # variable
                order = c(50, 1, 50),  # (p,d,q) parameters
                fixed = c(fixed_ar, fixed_ar)
)

model7

checkresiduals(model7,plot = F)
# autocorrelated

model7 %>% 
  residuals() %>% 
  ggtsdisplay(lag.max = 50)
# lags 1, is significant

coeftest(model7)
# wszystkie poza ma9 i ar50 insignificant

autoplot(forecast(model7, h = 95))

# model 9 - rfe - Arima(20,1,20) z lagami:1,5,8,9,10,12,15,16,18,20 ----


fixed_ar <- rep(NA, 20)
fixed_ar[c(2,3,4,6,7,11,13,14,17,19)] <- 0
fixed_ar

model9 <- Arima(btc$btc,  # variable
                order = c(20, 1, 20),  # (p,d,q) parameters
                fixed = c(fixed_ar, fixed_ar)
)


coeftest(model9)
# wszystkie significant

model9

checkresiduals(model9,plot = F)
# autocorrelated

model9 %>% 
  residuals() %>% 
  ggtsdisplay(lag.max = 50)
# lags 46 significant mildly


autoplot(forecast(model9, h = 95))




# ----
model8 <- models[[9]]

model8

checkresiduals(model8,plot = F)
# autocorrelated

model8 %>% 
  residuals() %>% 
  ggtsdisplay(lag.max = 50)
# lags 1, is significant

coeftest(model8)
# wszystkie poza ma9 i ar50 insignificant

autoplot(forecast(model8, h = 95))

# Wykres z orginalne ts+one step ahead dla test dla kilku modeli
# Wykres z orginalne ts+ wiele ahead dla kilku modeli


btc
btc_test


# Plotting ----
test_fit1 <- Arima(btc_test, model=model1)
test_fit3 <- Arima(btc_test, model=model3)

onestep1 <- fitted(test_fit1)
onestep3 <- fitted(test_fit3)

print(forecast(model1, h = nrow(btc_test)))-> ahead1
allsteps1 <- ahead1$`Point Forecast`

print(forecast(model3, h = nrow(btc_test)))-> ahead3
allsteps3 <- ahead3$`Point Forecast`

btc_test %>%
  fortify() %>%
  as_tibble() -> btc_test2

btc_test2$onestep1 <- as.numeric(onestep1)  
btc_test2$onestep3 <- as.numeric(onestep3)

btc_test2$allsteps1 <- as.numeric(allsteps1)  
btc_test2$allsteps3 <- as.numeric(allsteps3)

accuracy(onestep1, btc_test)

ggplot(btc_test2, aes(x = Index)) +
  geom_line(aes(y = btc)) +
  geom_line(aes(y = onestep1), color = 'blue')+
  geom_line(aes(y = allsteps1), color = 'red')


btc_test3 <- btc_test2 %>%
  pivot_longer(3:6)

autoplot(btc_test) +
  geom_line(data = btc_test3, aes(x = Index, y= value, color = name))


accuracy(onestep, btc_test)


# accuracy(fit2)

# compare models ----

# obtain out of sample forecasts
test_fit1 <- Arima(btc_test, model=model1)
test_fit2 <- Arima(btc_test, model=model2)
test_fit3 <- Arima(btc_test, model=model3)
test_fit4 <- Arima(btc_test, model=model4)
test_fit5 <- Arima(btc_test, model=model5)
test_fit7 <- Arima(btc_test, model=model7)
test_fit9 <- Arima(btc_test, model=model9)

onestep1 <- fitted(test_fit1)
onestep1 <- fitted(test_fit1)
onestep1 <- fitted(test_fit1)
onestep1 <- fitted(test_fit1)
onestep1 <- fitted(test_fit1)
onestep1 <- fitted(test_fit1)
onestep1 <- fitted(test_fit1)
onestep1 <- fitted(test_fit1)
onestep1 <- fitted(test_fit1)

rmse_out_sample <- rbind(
  accuracy(fitted(test_fit1), btc_test),
  accuracy(fitted(test_fit2), btc_test),
  accuracy(fitted(test_fit3), btc_test),
  accuracy(fitted(test_fit4), btc_test),
  accuracy(fitted(test_fit5), btc_test),
  accuracy(fitted(test_fit7), btc_test),
  accuracy(fitted(test_fit9), btc_test)
)[,2]

model1
model2
model3
model4
model5
model7
model9

rmse_in_sample <- rbind(
  accuracy(model1),
  accuracy(model2),
  accuracy(model3),
  accuracy(model4),
  accuracy(model5),
  # accuracy(model6),
  accuracy(model7),
  accuracy(model9)
)[,2]

# model1, model2, model3, model4, model5, model6, model7
cbind(
  AIC(model1, model2, model3, model4, model5, model7, model9),
  BIC(model1, model2, model3, model4, model5, model7, model9),
  rmse_in_sample, 
  rmse_out_sample) -> measures

measures[c(3)] <- NULL
  
measures %>%
  as_tibble(rownames = 'model_no') -> measures

measures %>%
  arrange(rmse_in_sample)
# 2, 9, 7, 3

measures %>%
  arrange(rmse_out_sample)
# (1, 3), 4, 7, 5, 2, 9

measures %>%
  arrange(AIC)
# 9,5,4,3

measures %>%
  arrange(BIC)
# 5,3,7,9

# wygrywa 3

# wnioski - autokorelacja residuali nieunikniona
# prosty model nie jest wiele lepszy od złożonego