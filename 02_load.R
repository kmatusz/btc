library(xts)
library(quantmod)
library(forecast)
library(tidyverse)

zec <- read_csv("data/gemini_ZECUSD_day.csv", 
                              skip = 1)

ltc <- read_csv("data/Gemini_LTCUSD_d.csv", 
                            skip = 1)

btc <- read_csv("data/Gemini_BTCUSD_d.csv", 
                skip = 1)

eth <- read_csv("data/Gemini_ETHUSD_d.csv", 
                skip = 1)


eth


zec %>%
  mutate(Date = as.Date(Date)) %>%
  select(Date) %>%
  distinct()
  summary()

  
  
zec %>%
  ggplot(aes(x = Date, y = Close)) + geom_line()


ltc %>%
  ggplot(aes(x = Date, y = Close)) + geom_line()

zec %>%
  mutate(Date = as.Date(Date)) %>%
  mutate(zec = Close) %>%
  select(Date, zec) %>%
  full_join(ltc %>% mutate(ltc = Close) %>% select(Date, ltc)) -> both

both %>%
  pivot_longer(2:3) %>%
  ggplot(aes(x = Date, y = value, color = name)) + 
  geom_line()

# słabo, tylko rok da się sensownie analizować 



btc %>%
  mutate(Date = as.Date(Date)) %>%
  mutate(btc = Close) %>%
  select(Date, btc) %>%
  full_join(eth %>% mutate(eth = Close*50) %>% select(Date, eth)) -> both_to_plot



both_to_plot %>%
  pivot_longer(2:3) %>%
  filter(Date > as.Date('2018-10-01')) %>%
  ggplot(aes(x = Date, y = value, color = name)) + 
  geom_line()

# nie są zbyt mocno cointegrated. I co teraz?

# Edit - są ale 50*etherum =btc
# Tylko od 2019


# Decyzja - zostawiam bitcoin i etherum, biorę okres od 2018-10-01
# dla danych dziennych




# data preparation to xts

date_to <- as.Date('2018-10-01')

eth %>% 
  mutate(eth = Close) %>% 
  select(Date, eth) %>% 
  rename(date = Date) %>% 
  filter(date >= date_to) -> eth_cleaned

btc %>%
  mutate(Date = as.Date(Date)) %>%
  mutate(btc = Close) %>%
  select(Date, btc) %>%
  rename(date = Date) %>% 
  filter(date >= date_to) -> btc_cleaned


full_data <- eth_cleaned %>%
  full_join(btc_cleaned)



df <- xts(full_data[,-1], # data columns (without the first column with date)
          full_data$date) # date/time index



btc <- df$btc

autoplot(btc)


ggAcf(btc)
# slowly declining
ggPacf(btc)
# only first mildly significant

source('functions/function_testdf.R')

testdf(btc, max.augmentations = 8)
# 1 augumentation is enough
# not stationary

testdf(diff(btc), max.augmentations = 8)
# now it's stationary - no augumentations needed



ggAcf(diff(btc), lag.max = 100)
ggAcf(diff(btc), lag.max = 30)
# most lags are insignificant, except: 17, 18, 20
ggPacf(diff(btc), lag.max = 100)
ggPacf(diff(btc), lag.max = 40)
# significant lags: 9, 20



# Start forecasting ---- 
# with Arima(1,1,1)

model1 <- Arima(btc$btc,  # variable
                  order = c(1, 1, 1)  # (p,d,q) parameters
)

model1

checkresiduals(model1,plot = F,lag = 30)

model1 %>% 
  residuals() %>% 
  ggtsdisplay()
# lag 20 is still present

coeftest(model1)
# both coefficients are not significant!

# Arima(20, 1, 20) -----

model2 <- Arima(btc$btc,  # variable
                order = c(20, 1, 20)  # (p,d,q) parameters
)

model2

checkresiduals(model1,plot = F,lag = 30)

model1 %>% 
  residuals() %>% 
  ggtsdisplay()
# lag 20 is still present

coeftest(model1)
# both coefficients are not significant!




