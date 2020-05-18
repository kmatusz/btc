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

eth <- df['/2020-03-31',]$eth
eth_test <- df['2020-04-01/',]$eth


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
# most lags are insignificant, except:  4, 20
ggPacf(diff(eth), lag.max = 100)
ggPacf(diff(eth), lag.max = 30)
# 
# significant lags: 4,5,20, 29 


# model 1 - Arima(1,1,1) ---- 

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
# 4, 20 nadal są

coeftest(model1)
# both coefficients are not significant!

# model 2 - Arima(20, 1, 20) with 4, 20 lags -----
fixed_ar <- rep(0, 20)
fixed_ar[c(4, 20)] <- NA
fixed_ar


model2 <- Arima(eth$eth,  # variable
                order = c(20, 1, 20),  # (p,d,q) parameters
                fixed = c(fixed_ar, fixed_ar)
                )

model2

checkresiduals(model2,plot = F)
# p.val <0.05 - skorelowane

model2 %>% 
  residuals() %>% 
  ggtsdisplay(lag.max = 50)
# 1,5,18 became significant
# all residuals autocorrelations are not significant. Good

coeftest(model2)
# dużo nieznaczących, ale też sporo NA

autoplot(forecast(model2, h = 95))


# model 3 - Arima(20, 1, 20) (all lags) -----


model3 <- Arima(eth$eth,  # variable
                order = c(20, 1, 20)  # (p,d,q) parameters

)

model3

checkresiduals(model3,plot = F)
# p-value < 0.05 - residuals are correlated

model3 %>% 
  residuals() %>% 
  ggtsdisplay(lag.max = 50)
# no significant values!

coeftest(model3)

autoplot(forecast(model3, h = 95))



# model 5 - auto Arima aic (4,1,4) ----
# model5 <- auto.arima(eth,
#                      d = 1,             # parameter d of ARIMA model
#                      max.p = 20,         # Maximum value of p
#                      max.q = 20,         # Maximum value of q
#                      max.order = 15,    # maximum p+q
#                      start.p = 1,       # Starting value of p in stepwise procedure
#                      start.q = 1,       # Starting value of q in stepwise procedure
#                      ic = "aic",        # Information criterion to be used in model selection.
#                      stepwise = FALSE,  # if FALSE considers all models
#                      allowdrift = TRUE, # include a constant
#                      trace = TRUE)      # show summary of all models considered

model5 <- Arima(eth$eth,  # variable
                order = c(4, 1, 4)  # (p,d,q) parameters
                
)

model5

checkresiduals(model5,plot = F)
# not autocorrelated

model5 %>% 
  residuals() %>% 
  ggtsdisplay(lag.max = 50)
# 20 very significant

coeftest(model5)

autoplot(forecast(model5, h = 95))

# Model 6 - auto arima bic(0,1,0) - sic! ----

# model6 <- auto.arima(eth,
#                      d = 1,             # parameter d of ARIMA model
#                      max.p = 20,         # Maximum value of p
#                      max.q = 20,         # Maximum value of q
#                      max.order = 30,    # maximum p+q
#                      start.p = 1,       # Starting value of p in stepwise procedure
#                      start.q = 1,       # Starting value of q in stepwise procedure
#                      ic = "bic",        # Information criterion to be used in model selection.
#                      stepwise = FALSE,  # if FALSE considers all models
#                      allowdrift = TRUE, # include a constant
#                      trace = TRUE)      # show summary of all models considered

model6 <- Arima(eth$eth,  # variable
                order = c(0, 1, 0)  # (p,d,q) parameters
                
)

model6

checkresiduals(model6,plot = F)
# not autocorrelated

model6 %>% 
  residuals() %>% 
  ggtsdisplay(lag.max = 50)
# 20 significant

coeftest(model6)
# significant

autoplot(forecast(model6, h = 95))




# model 7 - Arima(4, 1, 4) as in model 5 but with added lag 20 (visible on acf and pacf) ----


fixed_ar <- rep(0, 20)
fixed_ar[c(1,2,3,4,20)] <- NA
fixed_ar

model7 <- Arima(eth$eth,  # variable
                order = c(20, 1, 20),  # (p,d,q) parameters
                fixed = c(fixed_ar, fixed_ar)
)

model7

checkresiduals(model7,plot = F)
# autocorrelated

model7 %>% 
  residuals() %>% 
  ggtsdisplay(lag.max = 50)
# no significant lags

coeftest(model7)
# 20 not very significant

autoplot(forecast(model7, h = 95))


# ----
save(
  model1,
  model2,
  model3,
  model5,
  model6,
  model7,
  eth,
  eth_test,
  file = 'data/05_outputs.Rdata'
)


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
  # accuracy(model4),
  accuracy(model5),
  # accuracy(model6),
  accuracy(model7)
  # accuracy(model9)
)[,2]

# model1, model2, model3, model4, model5, model6, model7
cbind(
  AIC(model1, model2, model3, model5, model7),
  BIC(model1, model2, model3, model5, model7),
  rmse)


# wnioski - autokorelacja residuali nieunikniona
# prosty model nie jest wiele lepszy od złożonego



# obtain out of sample forecasts ----
test_fit1 <- Arima(eth_test, model=model1)
test_fit2 <- Arima(eth_test, model=model2)
test_fit3 <- Arima(eth_test, model=model3)
test_fit5 <- Arima(eth_test, model=model5)
test_fit7 <- Arima(eth_test, model=model7)


rmse_out_sample <- rbind(
  accuracy(fitted(test_fit1), eth_test),
  accuracy(fitted(test_fit2), eth_test),
  accuracy(fitted(test_fit3), eth_test),
  accuracy(fitted(test_fit5), eth_test),
  accuracy(fitted(test_fit7), eth_test)
)[,2]


rmse_in_sample <- rbind(
  accuracy(model1),
  accuracy(model2),
  accuracy(model3),
  # accuracy(model4),
  accuracy(model5),
  accuracy(model7)
)[,2]

# model1, model2, model3, model4, model5, model6, model7
cbind(
  AIC(model1, model2, model3, model5, model7),
  BIC(model1, model2, model3, model5, model7),
  rmse_in_sample, 
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

# wygrywa 5, za nim 4