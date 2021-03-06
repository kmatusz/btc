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




# df_main <- df['/2020-01-31',]
df_main <- df['2020-01-31/2020-03-31',]
df_test <- df['2020-04-01/',]



df_plot <- df_main
df_plot$eth_n <- df_main$eth/max(df_main$eth)
df_plot$btc_n <- df_main$btc/max(df_main$btc)

# normalized plot
autoplot(df_plot[,c('eth_n', 'btc_n')], facets = NULL)


btc <- df_main$btc
btc_test <- df_test$btc

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
ggAcf(diff(btc), lag.max = 30)
# only 4 significant
ggPacf(diff(btc), lag.max = 100)
ggPacf(diff(btc), lag.max = 30)
# no significant lags

# only 4

# Arima(4,1,4) most basic model ----

model1 <- Arima(btc$btc,  # variable
                order = c(4, 1, 4)  # (p,d,q) parameters
)

model1

checkresiduals(model1,plot = F,lag = 30)
# H0 - residuals are not correlated
# p.val > 0 - ok

model1 %>% 
  residuals() %>% 
  ggtsdisplay()
# no significant lags

coeftest(model1)
# only ar3 significant

# model 2 - Arima(4, 1, 4) with lag 4 -----
fixed_ar <- c(0, 0, 0, NA)


model2 <- Arima(btc$btc,  # variable
                order = c(4, 1, 4),  # (p,d,q) parameters
                fixed = c(fixed_ar, fixed_ar)
)

model2

checkresiduals(model2,plot = F)
# p.val >0.05 - nieskorelowane (ale na granicy, 0.07)

model2 %>% 
  residuals() %>% 
  ggtsdisplay(lag.max = 50)
# all residuals autocorrelations are not significant. Good

coeftest(model2)
# no significant

autoplot(forecast(model2, h = 95))


# Model 3 - auto arima(1,1,0) ----
model3 <- auto.arima(btc,
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

model3 <- Arima(btc$btc,  # variable
                order = c(1, 1, 0)  # (p,d,q) parameters
)


checkresiduals(model3,plot = F)
# p.val >0.05 - nieskorelowane 

model3 %>% 
  residuals() %>% 
  ggtsdisplay(lag.max = 50)
# all residuals autocorrelations are not significant. Good

coeftest(model3)
# little significant (0.07)




# Plotting ----
# test_fit1 <- Arima(btc_test, model=model1)
# test_fit2 <- Arima(btc_test, model=model2)
# 
# onestep1 <- fitted(test_fit1)
# onestep2 <- fitted(test_fit2)
# 
# print(forecast(model1, h = nrow(btc_test)))-> ahead1
# allsteps1 <- ahead1$`Point Forecast`
# 
# print(forecast(model2, h = nrow(btc_test)))-> ahead2
# allsteps2 <- ahead2$`Point Forecast`
# 
# btc_test %>%
#   fortify() %>%
#   as_tibble() -> btc_test2
# 
# btc_test2$onestep1 <- as.numeric(onestep1)  
# btc_test2$onestep2 <- as.numeric(onestep2)
# 
# btc_test2$allsteps1 <- as.numeric(allsteps1)  
# btc_test2$allsteps2 <- as.numeric(allsteps2)
# 
# accuracy(onestep1, btc_test)
# 
# ggplot(btc_test2, aes(x = Index)) +
#   geom_line(aes(y = btc)) +
#   geom_line(aes(y = onestep1), color = 'blue')+
#   geom_line(aes(y = allsteps1), color = 'red')
# 
# 
# btc_test3 <- btc_test2 %>%
#   pivot_longer(3:6)
# 
# autoplot(btc_test) +
#   geom_line(data = btc_test3, aes(x = Index, y= value, color = name))
# 
# 
# accuracy(onestep, btc_test)


# accuracy(fit2)

# compare models ----

save(model1, 
     model2,
     model3, 
     btc, 
     btc_test,
     file = 'data/08_outputs.Rdata')


# obtain out of sample forecasts
test_fit1 <- Arima(btc_test, model=model1)
test_fit2 <- Arima(btc_test, model=model2)
test_fit3 <- Arima(btc_test, model=model3)

onestep1 <- fitted(test_fit1)
onestep2 <- fitted(test_fit2)
onestep3 <- fitted(test_fit3)

rmse_out_sample <- rbind(
  accuracy(fitted(test_fit1), btc_test),
  accuracy(fitted(test_fit2), btc_test),
  accuracy(fitted(test_fit3), btc_test)
)[,2]


rmse_in_sample <- rbind(
  accuracy(model1),
  accuracy(model2),
  accuracy(model3)
)[,2]

# model1, model2, model3, model4, model5, model6, model7
cbind(
  AIC(model1, model2, model3),
  BIC(model1, model2, model3),
  rmse_in_sample, 
  rmse_out_sample) -> measures

measures[c(3)] <- NULL

measures %>%
  as_tibble(rownames = 'model_no') -> measures


measures %>%
  arrange(rmse_out_sample)
# 3,2,1

measures %>%
  arrange(AIC)
# 2,3,1

measures %>%
  arrange(BIC)
# 3,2,1

# wygrywa 3

# wnioski - autokorelacja residuali nieunikniona
# prosty model nie jest wiele lepszy od złożonego