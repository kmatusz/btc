library(xts)
library(quantmod)
library(forecast)
source('functions/function_testdf.R')
library(tidyverse)

do_fit <- TRUE

if(!do_fit){
  load('data/models_2.Rdata')
}

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

btc <- df['/2020-03-31',]$btc
# btc_test <- df['2020-02-01/',]$btc
btc_test <- df['2020-04-01/',]$btc

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
# most lags are insignificant, except: 1, 17, 18, 20
ggPacf(diff(btc), lag.max = 100)
ggPacf(diff(btc), lag.max = 50)
# significant lags: 1, 9, 20

# All significant: 1, 9, 17, 18, 20

# Arima(1,1,1) most basic model ----
if(do_fit) {
  model1 <- Arima(btc$btc,  # variable
                  order = c(1, 1, 1)  # (p,d,q) parameters
  )
}

model1

checkresiduals(model1,plot = F,lag = 30)
# H0 - residuals are not correlated
# p.val > ok!

model1 %>% 
  residuals() %>% 
  ggtsdisplay()
# lag 20 is still present

coeftest(model1)
# both coefficients are not significant!

# model 2 - Full Arima(20, 1, 20) NOT ABLE TO FIT-----
# if(do_fit) {
#   model2 <- Arima(btc$btc,  # variable
#                   order = c(20, 1, 20)  # (p,d,q) parameters
#   )
# }
# 
# model2
# 
# checkresiduals(model2,plot = F)
# # p.val <0.05 - skorelowane
# 
# model2 %>% 
#   residuals() %>% 
#   ggtsdisplay(lag.max = 50)
# # all residuals autocorrelations are not significant. Good
# 
# coeftest(model2)
# # some are significant
# 
# autoplot(forecast(model2, h = 95))


# model 3 - Arima(20, 1, 20) with lags 1, 9, 17, 18, 20 -----

fixed_ar <- rep(0, 20)
fixed_ar[c(1, 9, 17, 18, 20)] <- NA
fixed_ar
if(do_fit) {
  model3 <- Arima(btc$btc,  # variable
                  order = c(20, 1, 20),  # (p,d,q) parameters
                  fixed = c(fixed_ar, fixed_ar)
  )
}

model3

checkresiduals(model3,plot = F)
# p-value < 0.05 - residuals are correlated

model3 %>% 
  residuals() %>% 
  ggtsdisplay(lag.max = 50)
# 50 lags are mildly significant in pacf

coeftest(model3)

autoplot(forecast(model3, h = 10))


# model 4 - Arima(20, 1, 20) with lags 1, 9, 17 -----

fixed_ar <- rep(0, 20)
fixed_ar[c(1, 9, 17)] <- NA
fixed_ar

if(do_fit) {
  model4 <- Arima(btc$btc,  # variable
                  order = c(20, 1, 20),  # (p,d,q) parameters
                  fixed = c(fixed_ar, fixed_ar)
  )
}

model4

checkresiduals(model4,plot = F)
# autocorrelated

model4 %>% 
  residuals() %>% 
  ggtsdisplay(lag.max = 50)
# 20 significant

coeftest(model4)
# only ar1, ma1 significant

autoplot(forecast(model4, h = 95))


# model 5 - auto Arima aic (2,1,9) ----
# model5 <- auto.arima(btc,
#                              d = 1,             # parameter d of ARIMA model
#                              max.p = 20,         # Maximum value of p
#                              max.q = 20,         # Maximum value of q
#                              max.order = 15,    # maximum p+q
#                              start.p = 1,       # Starting value of p in stepwise procedure
#                              start.q = 1,       # Starting value of q in stepwise procedure
#                              ic = "aic",        # Information criterion to be used in model selection.
#                              stepwise = FALSE,  # if FALSE considers all models
#                              allowdrift = TRUE, # include a constant
#                              trace = TRUE)      # show summary of all models considered

model5 <- Arima(btc$btc,  # variable
                order = c(2, 1, 9)  # (p,d,q) parameters
)

model5

checkresiduals(model5,plot = F)
# not autocorrelated

model5 %>% 
  residuals() %>% 
  ggtsdisplay(lag.max = 50)
# 20, 17 significant

coeftest(model5)
# only ar1, ma1 significant

autoplot(forecast(model4, h = 95))

# Model 6 - auto arima bic (0,1,1) ----

# model6 <- auto.arima(btc,
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

model6 <- Arima(btc$btc,  # variable
                order = c(0, 1, 1)  # (p,d,q) parameters
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


save(
  model1,
  # model2,
  model3,
  model4,
  model5,
  model6,
  btc,
  btc_test,
  file = 'data/03_outputs.Rdata'
)



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




# accuracy(fit2)

# compare models ----

# obtain out of sample forecasts
test_fit1 <- Arima(btc_test, model=model1)
test_fit3 <- Arima(btc_test, model=model3)
test_fit4 <- Arima(btc_test, model=model4)
test_fit5 <- Arima(btc_test, model=model5)
test_fit6 <- Arima(btc_test, model=model6)
test_fit7 <- Arima(btc_test, model=model7)


rmse_out_sample <- rbind(
  accuracy(fitted(test_fit1), btc_test),
  accuracy(fitted(test_fit3), btc_test),
  accuracy(fitted(test_fit4), btc_test),
  accuracy(fitted(test_fit5), btc_test),
  accuracy(fitted(test_fit6), btc_test),
  accuracy(fitted(test_fit7), btc_test)
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
  # accuracy(model2),
  accuracy(model3),
  accuracy(model4),
  accuracy(model5),
  accuracy(model6),
  accuracy(model7)
)[,2]

# model1, model2, model3, model4, model5, model6, model7
cbind(
  AIC(model1, model3, model4, model5, model6, model7),
  BIC(model1, model3, model4, model5, model6, model7),
  rmse_in_sample, 
  rmse_out_sample) -> measures

measures[c(3)] <- NULL

measures %>%
  as_tibble(rownames = 'model_no') -> measures


measures %>%
  arrange(rmse_out_sample)
# 5,4,3,1,6,7

measures %>%
  arrange(AIC)
# 4,3,5,6,1,7

measures %>%
  arrange(BIC)
# 6,7,1,4,3,5

# wygrywa 5, za nim 4
