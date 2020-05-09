library(xts)
library(quantmod)
library(forecast)
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
df['/2020-01-31',]
# 

eth <- df['/2020-01-31',]$eth
eth_test <- df['2020-02-01/',]$eth

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
#
# most lags are insignificant, except:  5, 20, 34, 41
ggPacf(diff(eth), lag.max = 100)
ggPacf(diff(eth), lag.max = 40)
# 
# significant lags: 5, 15, 20,

# 5, 15, 20, 34

# Start forecasting ---- 
# with Arima(1,1,1)

model1 <- Arima(eth$eth,  # variable
                order = c(1, 1, 1)  # (p,d,q) parameters
)

model1

checkresiduals(model1,plot = F,lag = 30)
# H0 - residuals are not correlated
# p.val < źle

model1 %>% 
  residuals() %>% 
  ggtsdisplay()
# 5, 15, 20 nadal są

coeftest(model1)
# both coefficients are not significant!

# model 2 - Arima(20, 1, 20) -----

model2 <- Arima(eth$eth,  # variable
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
# dużo nieznaczących, ale też sporo NA

autoplot(forecast(model2, h = 95))


# model 3 - Arima(34, 1, 34) with lags 5, 15, 20, 34, 37 -----

fixed_ar <- rep(0, 34)
fixed_ar[c(5, 15, 20, 34)] <- NA
fixed_ar

model3 <- Arima(eth$eth,  # variable
                order = c(34, 1, 34),  # (p,d,q) parameters
                fixed = c(fixed_ar, fixed_ar)
)

model3

checkresiduals(model3,plot = F)
# p-value < 0.05 - residuals are correlated

model3 %>% 
  residuals() %>% 
  ggtsdisplay(lag.max = 50)
# 37 significant in acf and pacf

coeftest(model3)
# tylko ar,ma15, ma34

autoplot(forecast(model3, h = 95))


# model 4 - Arima with lags MA(15, 34, 37), AR(15,37) -----

fixed_ar <- rep(0, 37)
fixed_ar[c(15,37)] <- NA
fixed_ar
fixed_ma <- fixed_ar
fixed_ma[c(34)] <- NA

model4 <- Arima(eth$eth,  # variable
                order = c(37, 1, 37),  # (p,d,q) parameters
                fixed = c(fixed_ar, fixed_ma)
)

model4

checkresiduals(model4,plot = F)
# autocorrelated

model4 %>% 
  residuals() %>% 
  ggtsdisplay(lag.max = 50)
# 20 lag significant

coeftest(model4)
# only ma34 significant

autoplot(forecast(model4, h = 95))


# model 5 - Arima(0, 1, 34) with lag ma34 -----

fixed_ma <- rep(0, 34)
fixed_ma[c(34)] <- NA

model5 <- Arima(eth$eth,  # variable
                order = c(0, 1, 34),  # (p,d,q) parameters
                fixed = c(fixed_ma)
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

model6 <- Arima(eth$eth,  # variable
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

model7 <- Arima(eth$eth,  # variable
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

# rfe - Arima(20,1,20) z lagami:1,5,8,9,10,12,15,16,18,20 ----


fixed_ar <- rep(NA, 20)
fixed_ar[c(2,3,4,6,7,11,13,14,17,19)] <- 0
fixed_ar

model9 <- Arima(eth$eth,  # variable
                order = c(20, 1, 20),  # (p,d,q) parameters
                fixed = c(fixed_ar, fixed_ar)
)


coeftest(model9)
# wszystkie poza ma9 i ar50 insignificant

model9

checkresiduals(model9,plot = F)
# autocorrelated

model9 %>% 
  residuals() %>% 
  ggtsdisplay(lag.max = 50)
# lags 1, is significant


autoplot(forecast(model7, h = 95))




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





fit2 <- Arima(eth_test[1:60], model=model2)
onestep <- fitted(fit2)
autoplot(fit2)

plot(a[,1],col="red")
lines(a[,2],col="blue")

a <- fit2$x
a$b <- fitted(fit2)

accuracy(fit2)
# accuracy(fit2)

# compare models ----
rmse <- rbind(
  accuracy(model1),
  accuracy(model2),
  accuracy(model3),
  accuracy(model4),
  accuracy(model5)
  # # accuracy(model6),
  # accuracy(model7),
  # accuracy(model9)
)[,2]

# model1, model2, model3, model4, model5, model6, model7
cbind(
  AIC(model1, model2, model3, model4, model5),
  BIC(model1, model2, model3, model4, model5),
  rmse)


# wnioski - autokorelacja residuali nieunikniona
# prosty model nie jest wiele lepszy od złożonego