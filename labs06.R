##############################################################
# dr Paweł Sakowski                                          #             
# sakowski@wne.uw.edu.pl                                     #
# Faculty of Economic Sciences, University of Warsaw         #
#                                                            #
# Time Series Analysis, Spring 2020                          #
# lab sections #6                                            #
##############################################################

# We start with defining path to current working directory
# (not needed if you work with the RStudio project)
setwd("...")

# install and load needed libraries
# install.packages("forecast")

library(lmtest)
library(fBasics)
library(urca)
library(xts)
library(forecast)
library(quantmod)

# lets load additional function testdf prepared 
# by the lecturer which was already used on lab 4 & 5

source("functions/function_testdf.R")

# lets import the data with prices of SP500
# directly from Yahoo finance
# by the getSymbols() function from quantmod
# we need to know a correct ticker
# for world indices check:
# https://finance.yahoo.com/world-indices
# ^GSPC for S&P500 index

getSymbols("^GSPC",             # ticker
           from = "2000-01-01", # starting date
           to = "2019-03-25")   # end date

head(GSPC)
tail(GSPC)
str(GSPC)

# lets keep just the close price (Adjusted)
GSPC <- GSPC[, 6]
head(GSPC)

# and change its name to SP500
names(GSPC) <- "SP500"

# lets calculate first differences 
GSPC$dSP500 <- diff.xts(GSPC$SP500)

# plot of the original and differenced series
plot(GSPC, 
     # plot data in two panels (each column separately)
     multi.panel = 2,
     main = "Original and differenced data for S&P500",
     col = c("darkblue", "darkgreen"),
     major.ticks = "years", 
     grid.ticks.on = "years",
     grid.ticks.lty = 3,
     yaxis.same = FALSE, 
     # otherwise scale is the same for each column!
     cex = .6)

# lets put a "penalty" on scientific notation
options(scipen  =  10)

# lets test integration level - d parameter 
# using testdf function 
# (we did it on last lab, but lets just shortly remind it)

testdf(variable = GSPC$SP500,
       max.augmentations = 2)

# H0 (about non-stationarity) cannot be rejected 
# (we use ADF with 1 augmentation due to 
# autocorrelation of residuals in basic DF test).
# Next we turn to test for non-stationarity of 1st differences 

testdf(variable = GSPC$dSP500,
       max.augmentations = 2)

# 1st differences are already stationary
# (here we can use a basic DF test), 
# so SP500 close price is an I(1) process - d = 1 


#######################################################################
# below we apply the Box-Jenkins procedure

#######################################################################
# step 1. INITIAL IDENTIFICATION of parameters p and q 

# lets see ACF and PACF for non-stationary variable
# ACF and PACF are calculated up to 36th lag

acf(GSPC$dSP500,
    lag.max = 36, # max lag for ACF
    ylim = c(-0.1, 0.1),    # limits for the y axis - we give c(min,max)
    lwd = 5,               # line width
    col = "dark green") 

# if there are missing values in the data we need 
# to add an additional option na.action = na.pass (see below)

# lets plot them together and limit the scale of ACF
par(mfrow = c(2, 1)) 
  acf(GSPC$dSP500,
      lag.max = 36, # max lag for ACF
      ylim = c(-0.1, 0.1),    # limits for the y axis - we give c(min,max)
      lwd = 5,               # line width
      col = "dark green",
      na.action = na.pass)   # do not stop if there are missing values in the data
  pacf(GSPC$dSP500, 
       lag.max = 36, 
       lwd = 5, col = "dark green",
       na.action = na.pass)
par(mfrow = c(1, 1)) # we restore the original single panel

# ACF and PACF suggest that maybe ARIMA (5,1,5) could be
#	a sensible model for SP500, probably without lags 3 and 4

# lets compare different models with AIC criteria


#######################################################################
# steps 2 and 3 interchangeably. MODEL ESTIMATION and DIAGNOSTICS


###############################################################################
# lets start with ARIMA(1,1,1)
# we are using Arima function from the forecast package

arima111 <- Arima(GSPC$SP500,  # variable
                  order = c(1, 1, 1)  # (p,d,q) parameters
                  )

# by default the model on differenced data (d = 1) is 
# estimated by without a constant term
# (assuming that first differences fluctuate arround 0)

# lets use coeftest() function from the lmtest package
# to test for significance of model parameters

coeftest(arima111)

# additional summary measures (eg. information criteria)
summary(arima111)

# both are highly significant

# however, using that syntax produces a model without 
# a constant term; the constant is included when d = 0

# if one wishes to include a constant also when d = 1
# an additional option include.constant = T has to be used

arima111_2 <- Arima(GSPC$SP500,  # variable
                    order = c(1, 1, 1),  # (p,d,q) parameters
                    include.constant = TRUE)  # including a constant

# a constant for a model with d = 1 is reported 
# as a drift parameter

coeftest(arima111_2)

# it is not statistically significant here

# lets move back to the model without a constant term

# are residuals of arima111 model white noise? 
# resid() function applied to model results returns residuals

plot(resid(arima111))

# lets check ACF and PACF

par(mfrow = c(2, 1)) 
   acf(resid(arima111), 
       lag.max = 36,
       ylim = c(-0.1, 0.1), 
       lwd = 5, col = "dark green",
       na.action = na.pass)
   pacf(resid(arima111), 
        lag.max = 36, 
        lwd = 5, col = "dark green",
        na.action = na.pass)
par(mfrow = c(1, 1))

# significance of lag 2 disappeared, but 
# there are some other still significant (3, 14, 15, 16, 17...)

# Ljung-Box test (for a maximum of 10 lags)
# H0 - residuals are not correlated

Box.test(resid(arima111), type = "Ljung-Box", lag = 20)

# at 5% we cannot reject the null about residuals being 
# white noise -- so ARIMA(1,1,1) might be correct

# However, we saw that lag 3 of residuals is significant.
# On the other hand -- initially lag 5 was significant 
# and we believed that ARIMA (5,1,5) would be good

# Therefore lets estimate other models and compare
#	-> are AIC "better" (lower)?
#	-> are new parameters significant? 

###############################################################################
# lets try ARIMA(3,1,3)

arima313 <- Arima(GSPC$SP500, 
                  order = c(3, 1, 3), 
                  # try constant term here
                  include.constant = TRUE)

coeftest(arima313)

# constant term significant at 10% level
# and all other parameters highly significant

# Ljung-Box test for autocorrelation of model residuals

Box.test(resid(arima313),
         type = "Ljung-Box", lag = 10)

# null cannot be rejected - p-value very high!

# ACF and PACF for residuals
# to simplify plotting there is
# a function written by the teacher
# which requires only the name
# of estimated ARIMA model

source("functions/function_plot_ACF_PACF_resids.R")

plot_ACF_PACF_resids(arima313)

# lags of ACF and PACF 12, 14, 15 seem to be significant

# does it make sense theoretically?


# lets try ARIMA(5,1,5)

arima515 <- Arima(GSPC$SP500, 
                  order = c(5, 1, 5), 
                  include.constant = TRUE)

coeftest(arima515)

# if standard errors are not calculated
# one may try more iterations
# and/or change the optimization method
# be patient: it takes about 10 seconds!
arima515 <- Arima(GSPC$SP500,
                  order = c(5, 1, 5), 
                  include.constant = TRUE,
                  # controlling number of iterations
                  optim.control = list(maxit = 800),
                  # optimization method:
                  # "Nelder-Mead" (default), "BFGS", "CG", 
                  # "L-BFGS-B", "SANN", "Brent"
                  optim.method = "L-BFGS-B")

coeftest(arima515)

# lags number 2 and 4 are not significant

# Ljung-Box test for autocorrelation of model residuals

Box.test(resid(arima515),
         type = "Ljung-Box", lag = 15)
# null cannot be rejected - p-value high!

# ACF and PACF for residuals

plot_ACF_PACF_resids(arima515)

# lags of ACF and PACF 12, 14, 15 still significant


###############################################################################
# lets try ARIMA(5,1,5) model without lag no 2 and 4

# intermediate lags can be set to 0 by using the 
# fixed= argument
# CAUTION! The order of parameters can be checked by:

coefficients(arima515)

# lets add restrictions on ar2, ar4 
# and ma2, ma4 (were not significant,
# so we assume ar2 = 0, ar4 = 0 
# and ma2 = 0 and ma4 = 0)
# CAUTION! vector provided in fixed =   must have length
# equal to the number of parameters!

arima515_2 <- Arima(GSPC$SP500,
                    order = c(5, 1, 5),
                    fixed = c(NA, 0, NA, 0, NA,      # vector of length
                              NA, 0, NA, 0 ,NA, NA), # equal to total number of parameters,
                    include.constant = TRUE          # last is for the intercept (if included)
                   )                                 # NA means no restriction on a parameter

coeftest(arima515_2)

# now all the lags are significant !!!

# lets check if residuals are white noise
plot_ACF_PACF_resids(arima515_2)

# Ljung-Box test
Box.test(resid(arima515_2),
         type = "Ljung-Box",lag = 20)

# the null cannot be rejected



###############################################################################
# alretnatively lets apply ARIMA(5,1,1)
# without ar2 and ar4

arima511 <- Arima(GSPC$SP500,
                  order = c(5, 1, 1),
                  fixed = c(NA, 0, NA, 0, NA,
                            NA, NA),
                  include.constant = TRUE)

coeftest(arima511)

summary(arima511)

# maybe lag 3 and the constant can also 
# be removed from the model

# Ljung-Box test
Box.test(resid(arima511),
         type = "Ljung-Box",lag = 10)

# null cannot be rejected 

# ACF and PACF for residuals
plot_ACF_PACF_resids(arima511)


###############################################################################
# in the end lets apply ARIMA(1,1,5) 
# model without ma2, ma4

arima115 <- Arima(GSPC$SP500,
                  order = c(1, 1, 5),
                  fixed = c(NA,
                            NA, 0, NA, 0, NA, NA),
                  include.constant = TRUE)

coeftest(arima115)

# same as for the previous one - lag 3
# and the constant can also be removed

summary(arima115)

# Ljung-Box test
Box.test(resid(arima115),
         type = "Ljung-Box", lag = 10)

# null cannot be rejected 

# ACF and PACF for residuals
plot_ACF_PACF_resids(arima115)


###############################################################################
# lets compare AIC for all models estimated so far
# CAUTION! for some of them rediduals are not white noise!

# Based on AIC which model is best? 

AIC(arima111, arima111_2, 
    arima115, arima511,
    arima515, arima515_2)

# arima515_2


# lets do the same for BIC
BIC(arima111, arima111_2, 
    arima115, arima511, 
    arima515, arima515_2)

# arima111

# from the perspective of sensibility arima515_2 seems 
# to be the best (all terms significant, residuals 
# are white noise and low IC)


################################################################################

# Exercise 6.1
# In the example above, try to estimate ARIMA models that include more lags 
# (including 12, 14, 15 which were significant in residuals)
# Can the model be improved based on AIC and/or BIC evaluation?
acf(diff(GSPC$SP500))

order <- c(14)
ar_fixed <- rep(NA, order[1])
ar_fixed[c(2,3,5,6,7,8,9,10,11, 12)] <- 0


arima511 <- Arima(GSPC$SP500,
                  order = c(14, 1, 14),
                  fixed = c(ar_fixed, ar_fixed),
                  # fixed = c(NA, 0, NA, 0, rep(0,9), NA, NA, NA),
                  include.constant = FALSE)

coeftest(arima511)


summary(arima511)


# Ljung-Box test
Box.test(resid(arima511),
         type = "Ljung-Box",lag = 10)

# null cannot be rejected 

# ACF and PACF for residuals
# plot_ACF_PACF_resids(arima511)
acf(resid(arima511))

# Exercise 6.2
# a) import the data for any other financial asset 
#    and calculate the first differences 
# b) using testdf function test integration level of the series
# c) create plots of ACF and PACF functions and decide 
#    on potential orders of ARIMA model 
# d) estimate selected model and test for autocorrelation
#	   of residuals (formal test and correlogram)
# e) estimate several alternative models - compare AIC and BIC.
#	   Which model(s) is finally selected? 

# Exercise 6.3
# We consider the data in the mrates.csv file, which cointains monthly levels
# of long- and short term interest rates in the USA:
#	  r3m : 3-Month Treasury Constant Maturity Rate,
#	  r10y: 10-Year Treasury Constant Maturity Rate.
#   period: 01.1986-02.2011
#   source: Board of Governors of the Federal Reserve System
#   Economic Data - FRED® II - http://research.stlouisfed.org/fred2/

mrates <- read.csv("data/mrates.csv", sep = ";", dec = ",")

# a) calculate first differences of the two variables 
# b) determine the level of integration for both variables
# c) based on ACF and PACF determine potential orders of the ARIMA model 
# d) estimate the first model and diagnose autocorrelation of residuals 
# e) if needed, estimate alternative models and choose the final model
#    based on AIC or/and BIC information criteria and statistical significance
#    of additional parameters
# f) for the final model 

# Exercise 6.4
# We consider the data/simul.txt file, which contains simulated realizations 
# of an ARMA(p,q) process. Apply the Box-Jenkins procedure and find 
# the orders p and q which were used in the Data Generating Process. 
